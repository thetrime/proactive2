var Prolog = require('proscript2');


function indicateBusy()
{
    // FIXME: Implement
}

function indicateReady()
{
    // FIXME: Implement
}

function isEventHandler(e)
{
    return e.startsWith("on");
}

Proactive = {render: function(url, module, container)
             {
                 console.log("Welcome to Proactive 2.0");
                 var Constants = require('./constants');
                 var PrologUtilities = require('./prolog_utilities');
                 classes = {};
                 Prolog.define_foreign(".", require('./dot'));
                 Prolog.define_foreign("on_server", require('./on_server')(url));
                 Prolog.consult_url(url + "/component/" + module, function()
                                    {
                                        var Components = Prolog.make_variable();
                                        var Goal = Prolog.make_compound(Prolog.make_functor(Prolog.make_atom("get_components"), 1), [Components]);
                                        var rc = Prolog.call({}, Goal);
                                        if (rc == 1)
                                        {
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
                             if (Prolog.exists_predicate(Prolog.make_atom(module), Constants.getInitialStateFunctor))
                             {
                                 var State = Prolog.make_variable();
                                 var Props = this.make_dict(this.props);
                                 var Goal = Prolog.make_compound(Constants.crossModuleCallFunctor,
                                                                 [Prolog.make_atom(module), Prolog.make_compound(Constants.getInitialStateFunctor,
                                                                                                                 [Props, State])]);
                                 var checkpoint = Prolog.save_state();
                                 var rc = Prolog.call({}, Goal);
                                 Prolog.release_blob("dict", Props);
                                 if (rc == 1)
                                 {
                                     this.state = this.termToJS(State);
                                 }
                                 else
                                 {
                                     this.state = {}
                                 }
                                 Prolog.restore_state(checkpoint);
                             }
                             else
                             {
                                 this.state = {}
                             }
                         }

                         make_dict(data)
                         {
                             return Prolog.make_blob("dict", {portray: PrologUtilities.portray_dict, data: data});
                         }

                         render()
                         {
                             var Form = Prolog.make_variable();
                             var State = this.make_dict(this.state);
                             var Props = this.make_dict(this.props);
                             var Goal = Prolog.make_compound(Constants.crossModuleCallFunctor,
                                                             [Prolog.make_atom(module), Prolog.make_compound(Constants.renderFunctor,
                                                                                                             [State, Props, Form])]);
                             var rc = Prolog.call({}, Goal);
                             Prolog.release_blob("dict", State);
                             Prolog.release_blob("dict", Props);
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
                             else if (rc == 2)
                             {
                                 console.log("Warning: render/3 was nondet");
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
                             while (Prolog.is_compound(List) && Prolog.term_functor(List) == Constants.listFunctor)
                             {
                                 var Head = Prolog.term_arg(List, 0);
                                 dom.push(this.nodeToDOM(Head));
                                 List = Prolog.term_arg(List, 1);
                             }
                             if (List != Constants.emptyListAtom)
                                 console.log("Bad list in listToDOM");
                             return dom;
                         }

                         nodeToDOM(Term)
                         {
                             if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.elementFunctor)
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
                             if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.curlyFunctor)
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
                             while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.dictFunctor)
                             {
                                 var Head = Prolog.term_arg(Term, 0);
                                 Term = Prolog.term_arg(Term, 1);
                                 if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == Constants.dictPairFunctor)
                                 {
                                     var name = Prolog.atom_chars(Prolog.term_arg(Head, 0));
                                     var value = this.termToJS(Prolog.term_arg(Term, 1));
                                     map[name] = value;
                                 }
                             }
                             // The tail of a dict is just a non-,/2 term
                             if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.dictPairFunctor)
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
                             while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.listFunctor)
                             {
                                 var Head = Prolog.term_arg(Term, 0);
                                 Term = Prolog.term_arg(Term, 1);
                                 if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == Constants.equalsFunctor)
                                 {
                                     var name = Prolog.atom_chars(Prolog.term_arg(Head, 0));
                                     var Value = Prolog.term_arg(Head, 1);
                                     if (isEventHandler(name))
                                     {
                                         map[name] = this.makeEventHandler(Value);
                                     }
                                     else if (Prolog.is_atom(Value))
                                     {
                                         map[name] = Prolog.atom_chars(Value);
                                     }
                                     else if (Prolog.is_compound(Value) && Prolog.term_functor(Value) == Constants.dictFunctor) // FIXME: Suspect?
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
                             if (Term != Constants.emptyListAtom)
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
                                 var State = this.make_dict(this.state);
                                 var Props = this.make_dict(this.props);
                                 var Goal = Prolog.make_compound(Constants.crossModuleCallFunctor,
                                                                 [Prolog.make_atom(module), Prolog.make_compound(Handler,
                                                                                                                 [PrologEvent, State, Props, NewState])]);
                                 var checkpoint = Prolog.save_state();
                                 Prolog.execute({},
                                                Goal,
                                                function(success)
                                                {
                                                    Prolog.release_blob("dict", State);
                                                    Prolog.release_blob("dict", Props);
                                                    if (success)
                                                    {
                                                        var newState = parent.termToJS(NewState);
                                                        Prolog.restore_state(checkpoint);
                                                        parent.setState(newState);
                                                    }
                                                    else
                                                        Prolog.restore_state(checkpoint);
                                                });
                             }.bind(this);
                         }

                     }
                 };

             }};
