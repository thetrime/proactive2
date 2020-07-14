var Prolog = require('proscript2');

//var Prolog = Proscript;
var getInitialStateFunctor;
var renderFunctor;
var crossModuleCallFunctor;
var listFunctor;
var emptyListAtom;
var nullAtom;
var elementFunctor;
var curlyFunctor;
var equalsFunctor;
var dictFunctor;
var dictPairFunctor;

function jsListToProlog(js)
{
    var list = [];
    for (var i = 0; i < js.length; i++)
        list.push(jsToProlog(js[i]));
    return list;
}

function jsToProlog(js)
{
    if (js.atom !== undefined)
        return Prolog.make_atom(js.atom);
    if (js.integer !== undefined)
        return Prolog.make_integer(js.integer);
    else if (js.compound !== undefined)
        return Prolog.make_compound(Prolog.make_functor(Prolog.make_atom(js.compound.name), js.compound.args.length), jsListToProlog(js.compound.args));
}

var foreign = 0;
var qOp = null;
var goalURI;

function indicateBusy()
{
    // FIXME: Implement
}

function indicateReady()
{
    // FIXME: Implement
}

function on_server(Goal)
{
    console.log("hello");
    // FIXME: Complain if we are inside render()
    var resume = Prolog._yield();
    var ws;
    if (foreign)
    {
        // We are backtracking. Try to get another solution by sending a ; and then yielding
        ws = Prolog.get_blob("websocket", foreign);
        ws.send(";");
        return 3; // YIELD
    }
    // First, create the websocket
    indicateBusy()
    ws = new WebSocket(goalURI);
    ws.state = "new";
    if (qOp == null)
    {
        qOp = Prolog.create_options();
        Prolog.set_option(qOp, Prolog.make_atom("quoted"), Prolog.make_atom("true"));
    }
    ws.onopen = function()
    {
        ws.send(Prolog.format_term(qOp, 1200, Goal) + ".\n");
        // Since any PrologState objects in the goal will never unify with the response, replace them all with variables
        // FIXME: Implement?
        // goal = delete_states(goal);
        ws.send(";");
        // This is all we do for now. Either we will get an error, find out that the goal failed, or that it succeeded
    }
    ws.cleanup = function()
    {
        indicateReady();
        if (foreign)
            Prolog.release_blob("websocket", foreign);
        foreign = 0;
        ws.close();
    };
    ws.onmessage = function(event)
    {
        //console.log("Got a message: " + util.inspect(event.data));
        if (ws.state == "new")
        {
            ws.state = "connected";
            return;
        }
        var term = Prolog.string_to_local_term(event.data);
        if (term == 0) // parse error
        {
            ws.cleanup();
            resume(false);
        }
        else if (term == Constants.failAtom)
        {
            ws.cleanup();
            resume(false);
        }
        else if (term == Constants.abortedAtom)
        {
            ws.cleanup();
            resume(false);
        }
        else if (Prolog.is_compound(term))
        {
            if (Prolog.term_functor(term) == Constants.exceptionFunctor)
            {
                ws.cleanup();
                Prolog.set_exception(Prolog.term_arg(term, 0));
                Prolog.free_local(term);
                resume(0);
            }
            else if (Prolog.term_functor(term) == Constants.cutFunctor)
            {
                ws.cleanup();
                var rc = Prolog.unify(Goal, Prolog.copy_term(Prolog.term_arg(term, 0)));
                if (!rc)
                    console.log("Warning: Could not unify " + Prolog.format_term(null, 1200, Prolog.term_arg(term, 0)) + " with goal " + Prolog.format_term(null, 1200, Goal));
                resume(rc);
                Prolog.free_local(term);
            }
            else
            {
                // OK, we need a backtrack point here so we can retry
                if (foreign)
                    Prolog.make_choicepoint_with_cleanup(foreign, ws.cleanup);
                else
                    Prolog.make_choicepoint_with_cleanup(Prolog.make_blob("websocket", ws), ws.cleanup);
                var rc = Prolog.unify(Goal, Prolog.copy_term(Prolog.term_arg(term, 0)));
                if (!rc)
                    console.log("Warning: Could not unify " + Prolog.format_term(null, 1200, Prolog.term_arg(term, 0)) + " with goal " + Prolog.format_term(null, 1200, Goal));
                resume(rc);
                Prolog.free_local(term);
            }
        }
    };
    ws.onerror = function(event)
    {
        console.log("WS error: " + event);
        ws.close();
        Errors.systemError(Prolog.make_atom(event.toString()));
        resume(0);
    }
    return 3; //  YIELD
}

function dot(State, Key, Value)
{
    if (isNull(State))
    {
        return Prolog.unify(Value, Prolog.make_compound(Constants.curlyFunctor, [Constants.nullAtom]));
    }
    if (!Prolog.is_blob(State, "state"))
    {
        // FIXME: Errors does not exist. Neither does Constants.
        return Errors.typeError(Constants.prologStateAtom, State);
    }
    if (Prolog.is_atom(Key))
    {
        var key = Prolog.atom_chars(Key);
        console.log("Is a key: " + key);
        console.log(Prolog.get_blob("state", State));
        return Prolog.unify(Value, jsToProlog(Prolog.get_blob("state", State)[key]));
    }
    throw new Error("Oops2");
    return 0;
}


function isEventHandler(e)
{
    return e.startsWith("on");
}

function isNull(t)
{
    return (Prolog.is_compound(t) && Prolog.term_functor(t) == curlyFunctor && Prolog.term_arg(t, 0) == nullAtom);
}

function Null(value)
{
    return Prolog.unify(value, Prolog.make_compound(curlyFunctor, [nullAtom]));
}


Proactive = {render: function(url, module, container)
             {
                 console.log("Welcome to Proactive 2.0");
                 getInitialStateFunctor = Prolog.make_functor(Prolog.make_atom("getInitialState"), 2);
                 renderFunctor = Prolog.make_functor(Prolog.make_atom("render"), 3);
                 crossModuleCallFunctor = Prolog.make_functor(Prolog.make_atom(":"), 2);
                 elementFunctor = Prolog.make_functor(Prolog.make_atom("element"), 3);
                 equalsFunctor = Prolog.make_functor(Prolog.make_atom("="), 2);
                 listFunctor = Prolog.make_functor(Prolog.make_atom("."), 2);
                 dictFunctor = Prolog.make_functor(Prolog.make_atom(","), 2);
                 dictPairFunctor = Prolog.make_functor(Prolog.make_atom(":"), 2);
                 curlyFunctor = Prolog.make_functor(Prolog.make_atom("{}"), 1);
                 emptyListAtom = Prolog.make_atom("[]");
                 nullAtom = Prolog.make_atom("null");
                 classes = {};
                 Prolog.define_foreign(".", dot);
                 Prolog.define_foreign("on_server", on_server);
                 goalURI = "ws" + url.substring(4) + "/goal";
                 Prolog.consult_url(url + "/component/" + module, function()
                                    {
                                        var Components = Prolog.make_variable();
                                        var Goal = Prolog.make_compound(Prolog.make_functor(Prolog.make_atom("get_components"), 1), [Components]);
                                        console.log("Calling " + Prolog.portray(Goal));
                                        var rc = Prolog.call({}, Goal);
                                        if (rc == 1)
                                        {
                                            console.log("ok1");
                                            Prolog.forEach(Components,
                                                           function(Module)
                                                           {
                                                               this.classes[Prolog.atom_chars(Module)] = defineProactiveComponent(Prolog.atom_chars(Module));
                                                           },
                                                           function() {}
                                                          );
                                            ReactDOM.render(React.createElement(classes[module], null, []), container);
                                        }
                                        else if (rc == 4)
                                        {
                                            console.log("Exception getting components:" + Prolog.portray(Prolog.get_exception()));
                                        }
                                        else
                                            console.log("Failed to get components: " + rc);

                                    });

                 function defineProactiveComponent(module)
                 {
                     return class extends React.Component
                     {
                         constructor(props)
                         {
                             super(props);
                             console.log("Props in JS: "); console.log(props);
                             this.env = {};
                             if (Prolog.exists_predicate(Prolog.make_atom(module), getInitialStateFunctor))
                             {
                                 var State = Prolog.make_variable();
                                 var Props = Prolog.make_blob("props", this.props);
                                 var Goal = Prolog.make_compound(crossModuleCallFunctor,
                                                                 [Prolog.make_atom(module), Prolog.make_compound(getInitialStateFunctor,
                                                                                                                 [Props, State])]);
                                 var rc = Prolog.call({}, Goal);
                                 Prolog.release_blob("props", Props);
                                 if (rc == 1)
                                 {
                                     this.state = this.termToJS(State);
                                 }
                                 else
                                 {
                                     this.state = {}
                                 }
                             }
                             else
                             {
                                 this.state = {}
                             }
                         }
                         render()
                         {
                             console.log("Rendering...");
                             var Form = Prolog.make_variable();
                             var State = Prolog.make_blob("state", this.state);
                             var Props = Prolog.make_blob("props", this.props);
                             var Goal = Prolog.make_compound(crossModuleCallFunctor,
                                                             [Prolog.make_atom(module), Prolog.make_compound(renderFunctor,
                                                                                                             [State, Props, Form])]);
                             var rc = Prolog.call({}, Goal);
                             Prolog.release_blob("state", State);
                             Prolog.release_blob("props", Props);
                             if (rc == 4)
                             {
                                 console.log("Exception in render/3:" + Prolog.portray(Prolog.get_exception()));
                                 return React.createElement('div', null, `Failed to render component`);
                             }
                             if (rc == 1)
                             {
                                 var dom = this.nodeToDOM(Form);
                                 return dom;
                             }
                             else
                             {
                                 // failure
                                 return React.createElement('div', null, `Failed to render component`);
                             }
                         }

                         listToDOM(List)
                         {
                             var dom = [];
                             while (Prolog.is_compound(List) && Prolog.term_functor(List) == listFunctor)
                             {
                                 var Head = Prolog.term_arg(List, 0);
                                 dom.push(this.nodeToDOM(Head));
                                 List = Prolog.term_arg(List, 1);
                             }
                             if (List != emptyListAtom)
                                 console.log("Bad list in listToDOM");
                             return dom;
                         }

                         nodeToDOM(Term)
                         {
                             if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == elementFunctor)
                             {
                                 var tag = Prolog.atom_chars(Prolog.term_arg(Term, 0));
                                 var attributes = this.attributesToJS(Prolog.term_arg(Term, 1));
                                 var children = this.listToDOM(Prolog.term_arg(Term, 2));
                                 if (classes[tag] !== undefined)
                                     return React.createElement(classes[tag], attributes, children);
                                 else if (ReactBootstrap[tag] !== undefined)
                                     return React.createElement(ReactBootstrap[tag], attributes, children);
                                 else
                                     return React.createElement(tag, attributes, children);

                             }
                             else if (Prolog.is_constant(Term))
                             {
                                 return Prolog.portray(Term);
                             }
                             else
                             {
                                 console.log("Unexpected DOM: " + Prolog.portray(Term));
                             }
                             return null;
                         }

                         termToJS(Term)
                         {
                             if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == curlyFunctor)
                             {
                                 return this.dictEntriesToJS(Prolog.term_arg(Term, 0));
                             }
                             else if (Prolog.is_atom(Term))
                             {
                                 return {atom: Prolog.atom_chars(Term)};
                             }
                             else if (Prolog.is_compound(Term))
                             {
                                 var args = [];
                                 for (var i = 0; i < Prolog.term_functor_arity(Term); i++)
                                 {
                                     args.push(this.termToJS(Prolog.term_arg(Term, i)));
                                 }
                                 return {compound: {name: Prolog.term_functor_name(Term), args: args}}
                             }
                             else if (Prolog.is_integer(Term))
                             {
                                 return {integer: Prolog.numeric_value(Term)};
                             }
                             else
                             {
                                 console.log("No JS for " + Prolog.portray(Term));
                             }
                         }

                         dictEntriesToJS(Term)
                         {
                             var map = {};
                             while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == dictFunctor)
                             {
                                 var Head = Prolog.term_arg(Term, 0);
                                 Term = Prolog.term_arg(Term, 1);
                                 if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == dictPairFunctor)
                                 {
                                     var name = Prolog.atom_chars(Prolog.term_arg(Head, 0));
                                     var value = this.termToJS(Prolog.term_arg(Term, 1));
                                     map[name] = value;
                                 }
                             }
                             // The tail of a dict is just a non-,/2 term
                             if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == dictPairFunctor)
                             {
                                 var name = Prolog.atom_chars(Prolog.term_arg(Term, 0));
                                 var value = this.termToJS(Prolog.term_arg(Term, 1));
                                 map[name] = value;
                             }
                             return map;
                         }

                         attributesToJS(Term)
                         {
                             var map = {};
                             while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == listFunctor)
                             {
                                 var Head = Prolog.term_arg(Term, 0);
                                 Term = Prolog.term_arg(Term, 1);
                                 if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == equalsFunctor)
                                 {
                                     var name = Prolog.atom_chars(Prolog.term_arg(Head, 0));
                                     var Value = Prolog.term_arg(Head, 1);
                                     if (isEventHandler(name))
                                     {
                                         console.log("Making event handler...");
                                         map[name] = this.makeEventHandler(Value);
                                     }
                                     else if (Prolog.is_atom(Value))
                                     {
                                         map[name] = Prolog.atom_chars(Value);
                                     }
                                     else if (Prolog.is_compound(Value) && Prolog.term_functor(Value) == dictFunctor) // FIXME: Suspect?
                                     {
                                         map[name] = this.dictEntriesToJS(Value);
                                     }
                                     else if (Prolog.is_compound(Value))
                                     {
                                         map[name] = this.termToJS(Value);
                                     }
                                     else
                                     {
                                         console.log("Unhandled attribute value: " + Prolog.portray(Value));
                                     }
                                 }
                             }
                             if (Term != emptyListAtom)
                                 console.log("Bad list in attributesToJS");
                             return map;
                         }

                         makeEventHandler(Term)
                         {
                             // FIXME: These Handler terms are never cleaned up
                             var Handler = Prolog.make_local(Term);
                             var parent = this;
                             return function(e)
                             {
                                 var NewState = Prolog.make_variable();
                                 var PrologEvent = Prolog.make_variable(); // FIXME: Put a representation of e in here
                                 var State = Prolog.make_blob("state", this.state);
                                 var Props = Prolog.make_blob("props", this.props);
                                 var Goal = Prolog.make_compound(crossModuleCallFunctor,
                                                                 [Prolog.make_atom(module), Prolog.make_compound(Handler,
                                                                                                                 [PrologEvent, State, Props, NewState])]);
                                 Prolog.execute({},
                                                Goal,
                                                function(success)
                                                {
                                                    Prolog.release_blob("state", State);
                                                    Prolog.release_blob("props", Props);
                                                    if (success)
                                                    {
                                                        parent.setState(parent.termToJS(NewState));
                                                    }

                                                });
                             }.bind(this);
                         }

                     }
                 };

             }}
