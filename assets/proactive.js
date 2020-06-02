// var Prolog = require('proscript');
var Prolog = Proscript;
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

var dot = "'.'(State, Key, Value):-\
        ( State == {null} ->\
            Value = {null}\
        ; State = {Key: Value}->\
            true\
        ; State = {Key: Value, _}->\
            true\
        ; State = {_, Values},\
          '.'(Values, Key, Value)->\
            true\
        ; Value = {null}\
        ), writeln(after(State, Key, Value)).";


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

function emptyState()
{
    var value = Prolog.make_variable();
    Null(value);
    return Prolog.make_local(value);
}


// FIXME: Delete
function dot(state, key, value)
{
    if (isNull(state))
    {
        return Null(value)
    }
    else if (!Prolog.is_blob(state, "state"))
    {
        // FIXME: Raise Type error instead
        throw new Error("oops: Not a state");
    }
    if (Prolog.is_atom(key))
    {
        console.log(Prolog.get_blob("state", state));
        return Prolog.unify(value, Prolog.get_blob("state", state).get(key));
    }
    else
        throw new Error("oops: key is not atomic");
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
                 //Prolog.define_foreign(".", dot);
                 Prolog.consult_string(dot);
                 Prolog.consult_url(url + "/component/" + module, function()
                                    {
                                        // For each solution to current_predicate(Module:render/3), do this:
                                        this.classes["foo"] = defineProactiveComponent("foo");
                                        this.classes["bar"] = defineProactiveComponent("bar"); // etc

                                        ReactDOM.render(React.createElement(classes[module], null, []), container);
                                        //ReactDOM.render(React.createElement(ProactiveForm, null, []), container);
                                    });

                 function defineProactiveComponent(module)
                 {
                     return class extends React.Component
                     {
                         constructor(props)
                         {
                             super(props);
                             this.env = {};
                             this._props = Prolog.make_blob("props", props)
                             if (Prolog.exists_predicate(Prolog.make_atom(module), getInitialStateFunctor))
                             {
                                 var State = Prolog.make_variable();
                                 var Goal = Prolog.make_compound(crossModuleCallFunctor,
                                                                 [Prolog.make_atom(module), Prolog.make_compound(getInitialStateFunctor,
                                                                                                                 [this._props, State])]);
                                 if (Prolog.call({}, Goal) == 1)
                                 {
                                     this.state = {blob: Prolog.make_local(State)};
                                 }
                                 else
                                 {
                                     this.state = {blob: emptyState()};
                                 }
                             }
                             else
                             {
                                 this.state = {blob: emptyState()};
                             }
                         }
                         render()
                         {
                             console.log("Rendering...");
                             var Form = Prolog.make_variable();
                             var Goal = Prolog.make_compound(crossModuleCallFunctor,
                                                             [Prolog.make_atom(module), Prolog.make_compound(renderFunctor,
                                                                                                             [this.state.blob, this._props, Form])]);
                             var rc = Prolog.call({}, Goal);
                             if (rc == 4)
                             {
                                 console.log("Exception in render/3:" + Prolog.portray(Prolog.get_exception()));
                             }
                             if (rc == 1)
                             {
                                 var dom = this.nodeToDOM(Form);
                                 return dom;
                             }
                             else
                             {
                                 // Trouble here
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
                                 console.log("All set:");
                                 console.log(attributes);
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

                         dictToJS(Term)
                         {
                             var map = {};
                             while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == dictFunctor)
                             {
                                 var Head = Prolog.term_arg(Term, 0);
                                 Term = Prolog.term_arg(Term, 1);
                                 if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == dictPairFunctor)
                                 {
                                     var name = Prolog.atom_chars(Prolog.term_arg(Head, 0));
                                     // FIXME: Really this should be something like termToJS() since the value is not necessarily an atom
                                     var value = Prolog.atom_chars(Prolog.term_arg(Head, 1));
                                     map[name] = value;
                                 }
                             }
                             // The tail of a dict is just a non-,/2 term
                             if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == dictPairFunctor)
                             {
                                 var name = Prolog.atom_chars(Prolog.term_arg(Term, 0));
                                 // FIXME: Really this should be something like termToJS() since the value is not necessarily an atom
                                 var value = Prolog.atom_chars(Prolog.term_arg(Term, 1));
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
                                     else if (Prolog.is_compound(Value) && Prolog.term_functor(Value) == dictFunctor)
                                     {
                                         map[name] = this.dictToJS(Value);
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
                             var Handler = Prolog.make_local(Term);
                             var parent = this;
                             return function(e)
                             {
                                 console.log("hello");
                                 console.log(this);
                                 var NewState = Prolog.make_variable();
                                 var PrologEvent = Prolog.make_variable(); // FIXME: Put a representation of e in here
                                 var Goal = Prolog.make_compound(crossModuleCallFunctor,
                                                                 [Prolog.make_atom(module), Prolog.make_compound(Handler,
                                                                                                                 [PrologEvent, this.state.blob, this._props, NewState])]);
                                 Prolog.execute({},
                                                Goal,
                                                function(success)
                                                {
                                                    console.log("Result: " + success);
                                                    if (success)
                                                    {
                                                        // FIXME: Actually want to *merge* the states here. If the state was stored as a JS object, React would do this for us.
                                                        console.log(parent);
                                                        parent.setState({blob: Prolog.make_local(NewState)});
                                                    }

                                                });
                             }.bind(this);
                         }

                     }
                 };

             }}
