:-module(proactive,
         [trigger_proactive_recompile/1,
          broadcast_proactive_message/2]).


:- use_module(library(http/http_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/websocket)).
:- use_module(library(http/http_session)).


:-http_handler(proactive(goal), execute_proactive, []).
:-http_handler(proactive(listen), listen_proactive, [spawn([])]).
:-http_handler(proactive(form/FormId), serve_proactive_form(FormId), [prefix]).
:-http_handler(proactive(component/FormId), serve_component(FormId), []).

:-http_handler(proactive('boilerplate.pl'), http_reply_file(proactive_lib('boilerplate.pl'), []), []).
:-http_handler(proactive(lib/'proactive.js'), http_reply_file(proactive_lib('proactive.js'), []), []).
:-http_handler(proactive(lib/'proscript.wasm'), http_reply_file(proactive_lib('proscript.wasm'), []), []).

execute_proactive(Request):-
        ( http_in_session(SessionID)->
            true
        ; SessionID = {null}
        ),
        http_upgrade_to_websocket(execute_proactive_ws_guarded(SessionID, Request), [], Request).

listen_proactive(Request):-
        ( http_in_session(SessionID)->
            true
        ; SessionID = {null}
        ),
        http_upgrade_to_websocket(listen_proactive_loop(SessionID), [], Request).


:-multifile(open_proactive_session_hook/1).
:-multifile(close_proactive_session_hook/1).
:-dynamic(proactive_listener/2).

open_proactive_session(Self, RootComponent):-
        forall(open_proactive_session_hook(Self), true),
        ( setof(RelatedModule,
                related_react_module(RootComponent, RelatedModule),
                Modules)->
            true
        ; Modules = []
        ),
        assert(proactive_listener(Self, Modules)).

close_proactive_session(Self):-
        retractall(proactive_listener(Self, _)),
        forall(close_proactive_session_hook(Self), true).

:-multifile(proactive_message_hook/1).

listen_proactive_loop(SessionID, Websocket):-
        ( SessionID == {null}->
            true
        ; b_setval(http_session_id, SessionID)
        ),
        thread_self(Self),
        ws_receive(Websocket, Message),
        ( Message.opcode == text->
            Data = Message.data,
            read_term_from_atom(Data, RootComponent, []),
            thread_create(ws_listen_worker(Websocket, Self), Worker, [detached(false)]),
            setup_call_cleanup(open_proactive_session(Self, RootComponent),
                               listen_proactive_loop_1(Websocket, Worker),
                               close_proactive_session(Self))
        ; otherwise->
            true
        ).

listen_proactive_loop_1(Websocket, Worker):-
        thread_get_message(Message),
        ( Message == close->
            thread_join(Worker, _),
            destroy_listeners,
            ws_close(Websocket, 1000, goodbye),
            throw(terminated)
        ; Message == ping ->
            ws_send(Websocket, text(pong))
        ; Message = message(Class, Term)->
            forall(should_handle_message(Class, Term, Key),
                   dispatch_user_message(Websocket, Term, Key))
        ; Message = consulted(_)->
            format(atom(Text), 'system(~k)', [Message]),
            ws_send(Websocket, text(Text))
        ; Message = system(_)->
            format(atom(Text), '~k', [Message]),
            ws_send(Websocket, text(Text))
        ; format(user_error, 'Bad proactive message: ~q~n', [Message])
        ),
        !,
        listen_proactive_loop_1(Websocket, Worker).

destroy_listeners:-
        thread_self(Self),
        retractall(proactive_message_listener(_, _, _, Self)).

dispatch_user_message(Websocket, Term, Key):-
        package_message(Term, Key, Text),
        ws_send(Websocket, text(Text)).


package_message(Term, Key, Text):-
        format(atom(Text), '~k', [user(Term, Key)]).

should_handle_message(Class, Term, Key):-
        thread_self(Self),
        proactive_message_listener(Class, Discriminator, Key, Self),
        catch(\+ \+ (call(Discriminator, Term)), _, fail).

:-dynamic
        proactive_message_listener/4.

broadcast_proactive_message(Class, Term):-
        forall(proactive_message_listener(Class, _, _, Queue),
               thread_send_message(Queue, message(Class, Term))).

ws_listen_worker(Websocket, Owner):-
        ws_receive(Websocket, Message),
        ( Message.opcode == close->
            thread_send_message(Owner, close)
        ; Message.opcode == text->
            Data = Message.data,
            read_term_from_atom(Data, Term, []),
            ( Term = message(T)->
                ( catch(proactive_message_hook(T),
                        Exception,
                        format(user_error, 'Exception handling Proactive message ~q: ~p~n', [T, Exception]))->
                    true
                ; format(user_error, 'Failure handling Proactive message ~q~n', [T])
                )
            ; Term = register_for(Class, Discriminator, Key)->
                assert(proactive_message_listener(Class, Discriminator, Key, Owner))
            ; Term = deregister(Key)->
                retractall(proactive_message_listener(_, _, Key, Owner))
            ; Term == ping ->
                thread_send_message(Owner, ping)
            ; format(user_error, 'Unexpected message from client: ~q~n', [Term])
            ),
            ws_listen_worker(Websocket, Owner)
        ; otherwise->
            ws_listen_worker(Websocket, Owner)
        ).

trigger_proactive_recompile(Module):-
        forall(proactive_listener(Queue, Modules),
               ( member(Module, Modules)->
                   thread_send_message(Queue, consulted(Module))
               ; true
               )).


:-multifile(proactive:goal_is_safe/1).

% SWI uses message_to_string/2 to print the message if there is an error
% This is fine, bug RFC-6455 says that no control packet may be > 125 bytes, or fragmented
% and the error can easily be much longer than that. So we just suppress it here and use 'error' instead
execute_proactive_ws_guarded(SessionID, OriginalRequest, WebSocket):-
        ( SessionID == {null}->
            true
        ; b_setval(http_session_id, SessionID)
        ),
        ( catch(execute_proactive_ws(OriginalRequest, WebSocket), E, true)->
	    ( var(E)->
		Msg = bye, Code = 1000
	    ; otherwise->
		Msg = error, Code = 1011
	    )
	; Msg = failed, Code = 1011
	),
	catch(ws_close(WebSocket, Code, text(Msg)), Error, print_message(error, Error)).

execute_proactive_ws(OriginalRequest, WebSocket):-
        ws_receive(WebSocket, Message, []),
        ( Message.opcode == text->
            Data = Message.data,
            catch(read_proactive_goal_and_execute_if_safe(WebSocket, OriginalRequest, Data),
                  Exception,
                  ( handle_proactive_goal_exception(Exception, WebSocket),
                    throw(Exception)
                  ))
        ; Message.opcode == close->
            !
        ).

read_proactive_goal_and_execute_if_safe(WebSocket, OriginalRequest, Data):-
        read_term_from_atom(Data, Goal, []),
        uuid(GoalId),
        thread_self(Self),
        ( goal_is_safe(Goal)->
            ws_send(WebSocket, text(GoalId)),
            setup_call_cleanup(assert(current_proactive_goal(GoalId, Self, Goal)),
                               execute_proactive_ws(OriginalRequest, WebSocket, Goal, Goal),
                               % The mutex here ensures that we will not release this GoalId while someone is
                               % trying to kill it. This ensures we will not kill the wrong thread by getting
                               % the goal id, finding the thread currently executing it, and then then (many
                               % cycles later), signalling that thread (now busy with another task) to abort.
                               with_mutex(react_goal_abort_mutex,
                                          retract(current_proactive_goal(GoalId, Self, Goal))))
        ; permission_error(execute, goal, Goal)
        ).

:-meta_predicate(execute_proactive_ws(+, +, +, 0)).
:-multifile(check:string_predicate/1).
execute_proactive_ws(OriginalRequest, WebSocket, ReplyGoal, Goal):-
        ws_receive(WebSocket, Message, []),
        ( Message.opcode == text->
            Data = Message.data,
            check_data(Data),
            execute_proactive_ws_1(OriginalRequest, Goal, ReplyGoal, WebSocket)
        ; Message.opcode == close->
            !
        ).

:-meta_predicate(execute_proactive_ws_1(+, 0, +, +)).
execute_proactive_ws_1(OriginalRequest, Goal, ReplyGoal, WebSocket):-
        setup_call_catcher_cleanup(true,
                                   execute_proactive_goal(OriginalRequest, Goal),
                                   Catcher,
                                   react_cleanup(ReplyGoal, Catcher, WebSocket)),
        ( var(Catcher)->            
            send_reply(WebSocket, exit(ReplyGoal))
        ; otherwise->
            true
        ),
        % Wait for the next request
        ws_receive(WebSocket, Message, []),
        Data = Message.data,
        ( Message.opcode == text->
            \+check_data(Data)
        ; Message.opcode == close->
            true
        ),
        !.

check_data(";").

:-multifile(proactive:react_goal_hook/2).
:-meta_predicate(proactive:react_goal_hook(+, 0)).

:-meta_predicate(execute_proactive_goal(+, 0)).
execute_proactive_goal(OriginalRequest, Goal):-
        ( predicate_property(proactive:react_goal_hook(_, _), number_of_clauses(_))->
            proactive:react_goal_hook(OriginalRequest, Goal)
        ; Goal
        ).
        

react_cleanup(Goal, exit, WebSocket):-
        send_reply(WebSocket, cut(Goal)).

react_cleanup(Goal, external_exception(E), WebSocket):-
	react_cleanup(Goal, WebSocket, exception(E)).

react_cleanup(_Goal, exception(_), _):- true. % Handled later in handle_proactive_goal_exception/2.

react_cleanup(_Goal, fail, WebSocket):-
        send_reply(WebSocket, fail).

react_cleanup(Goal, !, WebSocket):-
        send_reply(WebSocket, cut(Goal)).

:-multifile(proactive:react_exception_hook/1).

handle_proactive_goal_exception(E, WebSocket):-
	( E = error(Error, Context)->
	    format(atom(ContextAtom), '~p', [Context]),
	    send_reply(WebSocket, exception(error(Error, ContextAtom)))
	; E = application_error(Error, Cause, Context)->
	    format(atom(ContextAtom), '~p', [Context]),
	    format(atom(CauseAtom), '~p', [Cause]),
	    send_reply(WebSocket, exception(application_error(Error, CauseAtom, ContextAtom)))
	; otherwise->
	    send_reply(WebSocket, exception(E))
        ),
        ignore(proactive:react_exception_hook(E)).


send_reply(WebSocket, Term):-
        format(atom(Text), '~k', [Term]),
	ws_send(WebSocket, text(Text)).


:-multifile(proactive_component_hook/2).
% Define proactive_component_hook/2 clauses to extend the objects available in Proactive
% FIXME: Implement this. Add extra args to Proactive.installComponents() and extra CSS/Javascript to the body

serve_proactive_form(FormId, Request):-
        ( predicate_property(proactive:allow_access_to_form(_), number_of_clauses(_))->
            ( proactive:allow_access_to_form(FormId)->
                true
            ; otherwise->
                memberchk(path(Path), Request),
                throw(http_reply(forbidden(Path)))
            )
        ; true
        ),
        subtract(Request, [path(_)], R1),
        http_absolute_location(proactive('.'), Path, []),
        parse_url(URL, [path(Path)|R1]),
        http_absolute_location(proactive('lib/proactive.js'), LibPath, []),

        format(atom(Bootstrap), 'window.onPrologReady = function() {Proactive.installComponents(ReactBootstrap); Proactive.installComponents({Datetime: Datetime}); Proactive.render("~w", "~w", document.getElementById("container"));}; if (window.prologReady) {console.log("Prolog already ready. Booting proactive"); window.onPrologReady();}', [URL, FormId]),

        % Change development -> production.min to get minified version
        HTML = element(html, [], [element(head, [], [element(script, [src='https://unpkg.com/react/umd/react.development.js', crossorigin=anonymous], []),
                                                     element(script, [src='https://unpkg.com/react-dom/umd/react-dom.development.js', crossorigin=anonymous], []),
                                                     element(script, [src=LibPath], []),
                                                     element(link, [rel=stylesheet,
                                                                    href='https://maxcdn.bootstrapcdn.com/bootstrap/4.5.0/css/bootstrap.min.css',
                                                                    integrity='sha384-9aIt2nRpC12Uk9gS9baDl411NQApFmC26EwAOH8WgZl5MYYxFfc+NcPb1dKGj7Sk',
                                                                    crossorigin=anonymous], [])]),
                                  element(body, [], [element(div, [id=container], []),
                                                     element(script, [src='https://unpkg.com/react-bootstrap@next/dist/react-bootstrap.js', crossorigin=anonymous], []),

                                                     % This looks nicer but is very hard to serve from a CDN
                                                     %element(script, [src='https://unpkg.com/prop-types@15.7.2/prop-types.min.js', crossorigin=anonymous], []),
                                                     %element(script, [src='https://unpkg.com/react-onclickoutside@6.9.0/dist/react-onclickoutside.min.js', crossorigin=anonymous], []),
                                                     %element(script, [src='https://cdn.date-fns.org/v2.0.0-alpha0/date_fns.min.js', crossorigin=anonymous], []),
                                                     %element(script, [src='https://unpkg.com/react-datepicker@2.16/dist/react-datepicker.min.js', crossorigin=anonymous], []),


                                                     % react-datetime and dependencies
                                                     element(script, [src='https://unpkg.com/moment@2.27.0/min/moment.min.js', crossorigin=anonymous], []),
                                                     element(script, [src='https://unpkg.com/react-datetime@2.16.3/dist/react-datetime.min.js', crossorigin=anonymous], []),
                                                     element(link, [rel=stylesheet, href='https://unpkg.com/react-datetime@2.16.3/css/react-datetime.css'], []),

                                                     element(script, [type='text/javascript'], [Bootstrap])])]),
        format(current_output, 'Content-type: text/html~n~n', []),
        html_write(current_output, HTML, []).



:-multifile(proactive:allow_access_to_form/1).

serve_component(Module, Request):-
        ( predicate_property(proactive:allow_access_to_form(_), number_of_clauses(_))->
            ( proactive:allow_access_to_form(Module)->
                true
            ; otherwise->
                memberchk(path(Path), Request),
                throw(http_reply(forbidden(Path)))
            )
        ; true
        ),
        ( current_module(Module)->
            findall(Candidate,
                    related_react_module(Module, Candidate),
                    Modules),
            sort(Modules, ModulesWithoutDuplicates),
            findall(Clause,
                    ( member(AModule, ModulesWithoutDuplicates),
                      react_clause(AModule, Clause)
                    ),
                    Clauses),
            format(current_output, 'Access-Control-Allow-Origin: *~n', []),

            % There seems to be a bug in IE11 where sending the response deflated, *even if they explicitly say they support it* will
            % get SCRIPT7002: No data is available for the requested resource
            % MS doesnt put IE in the user agent string anymore (perhaps theyre suitably embarassed about IE6 and IE8) but we can still
            % detect IE11 by checking for the substring 'Trident/'
            ( memberchk(user_agent(UserAgent), Request),
              sub_atom(UserAgent, _, _, _, 'Trident/') ->
                TargetStream = current_output
            ; memberchk(accept_encoding(AcceptEncoding), Request),
              sub_atom(AcceptEncoding, _, _, _, deflate)->
                format(current_output, 'Content-Encoding: deflate~n', []),
                % We shouldnt close the CGI-stream ourself. The wrapper will do that when the handler is finished
                zopen(current_output, TargetStream, [close_parent(false)])
            ; otherwise->
                TargetStream = current_output
            ),
            format(current_output, 'Content-Type: text/prolog~n~n', []),

            % We emit this list of components in module(user) because proscript does not have current_module/1 and current_predicate/1 is not module-aware
            findall(Component,
                    ( member(Component, ModulesWithoutDuplicates),
                      current_predicate(Component:render/3)
                    ),
                    Components),
            write_term(TargetStream, get_components(Components), [numbervars(true), quoted(true), ignore_ops(true)]),
            writeln(TargetStream, '.'),

            forall(member(Clause, Clauses),
                   ( numbervars(Clause, 0, _, [singletons(true)]),
                     write_term(TargetStream, Clause, [numbervars(true), quoted(true), ignore_ops(true)]),
                     writeln(TargetStream, '.')
                   )),
            ( TargetStream == current_output ->
                true
            ; otherwise->
                close(TargetStream)
            )
        ; otherwise->
            format(current_output, 'Access-Control-Allow-Origin: *~n', []),
            format(current_output, 'Content-Type: text/prolog~n~n', [])
        ).

%!      related_react_module(+BaseModule, ?OtherModule) is semidet.
%       Succeeds if OtherModule is a module which BaseModule depends on
%       If OtherModule is unbound, on backtracking all modules related to the root
%       are enumerated.
%       Note that BaseModule is defined to be dependent on BaseModule itself. This tends
%       to make code which finds all code related to a particular module simpler to implement

related_react_module(Root, Root).
related_react_module(Module, Related):-
        current_predicate(_, Module:depends_on(_)),
        predicate_property(Module:depends_on(_), interpreted),
        \+predicate_property(Module:depends_on(_), imported_from(_)),
        clause(Module:depends_on(SubModule), _, _),
        related_react_module(SubModule, Related).


react_clause(Module, :-module(Module, Exports)):-
        module_property(Module, exports(Exports)).
react_clause(Module, Head:-Body):-
        current_predicate(_, Module:Head),
        predicate_property(Module:Head, interpreted),
        functor(Head, Name, Arity),
        \+predicate_property(Module:Head, imported_from(_)),
        % Do not provide the source code of tabled predicates if they are also in the same module
        ( current_predicate(_, Module:tabled_predicate(_, _))->
            \+clause(Module:tabled_predicate(Module, Name/Arity), true)
        ; true
        ),
        clause(Module:Head, Body, _).
react_clause(Module, Head):-
        current_predicate(_, Module:tabled_predicate(_, _)),
        clause(Module:tabled_predicate(SourceModule, Name/Arity), true),
        functor(Head, Name, Arity),
        call(SourceModule:Head).

user:term_expansion(requires(X), [depends_on(X), :-proactive:do_load_react_module(X)]).

:-multifile(proactive:load_react_module/1).

do_load_react_module(X):-
        ( proactive:load_react_module(X)->
            true
        ; use_module(X)
        ).

:-meta_predicate(user:on_server(0)).
user:on_server(Goal):- Goal.
user:get_this(fixme).
user:bubble_event(_, Event):-
        permission_error(handle, event, Event).
user:bubble_event(_, _, Event):-
        permission_error(handle, event, Event).
user:media_size(800, 600).

