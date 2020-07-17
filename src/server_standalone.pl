:- include(src/testing).
:- use_module(proactive).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(src/jsx).

http:location(proactive, root(proactive), []).

main:-
        http_server(http_dispatch, [port(8880)]).

:-multifile(proactive:goal_is_safe/1).
proactive:goal_is_safe(_).

:- use_module(src/foo).
:- use_module(src/bar).

term_expansion(end_of_file, _):-
        prolog_load_context(module, Module),
        current_predicate(trigger_proactive_recompile/1),
        trigger_proactive_recompile(Module),
        fail.
