Prolog = require('proscript2');

var next_id = 0;

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
                 Prolog.define_foreign("get_this", require('./get_this'));
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
                             this.id = next_id++;
                             this.module = module;
                             console.log("Allocating id " + this.id);
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
                                     this.state = PrologUtilities.prologToJS(State);
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

                             console.log("Rendering " + module + " with these props: ");
                             console.log(this.props);
                             window.zing = this.props;
                             console.log(Prolog.portray(Props));


                             var This = Prolog.make_blob("widget", this);
                             var Form = Prolog.make_variable();
                             var State = this.make_dict(this.state);
                             var Props = this.make_dict(this.props);
                             var Goal = Prolog.make_compound(Constants.crossModuleCallFunctor,
                                                             [Prolog.make_atom(module), Prolog.make_compound(Constants.renderFunctor,
                                                                                                             [State, Props, Form])]);
                             var env = {_this: This, blobs: []};
                             var rc = Prolog.call(env, Goal);
                             // We cannot release This just yet - we may need it when calling nodeToDOM()
                             Prolog.release_blob("dict", State);
                             Prolog.release_blob("dict", Props);
                             if (rc == 4)
                             {
                                 Prolog.release_blob("widget", This);
                                 this.releaseBlobs(env);
                                 console.log("Exception in render/3:" + Prolog.portray(Prolog.get_exception()));
                                 return React.createElement('div', null, `Failed to render component`);
                             }
                             if (rc == 1)
                             {
                                 var dom = this.nodeToDOM(Form);
                                 Prolog.release_blob("widget", This);
                                 this.releaseBlobs(env);
                                 return dom;
                             }
                             else if (rc == 2)
                             {
                                 console.log("Warning: render/3 was nondet");
                                 var dom = this.nodeToDOM(Form);
                                 Prolog.release_blob("widget", This);
                                 this.releaseBlobs(env);
                                 return dom;
                             }

                             else
                             {
                                 // failure
                                 Prolog.release_blob("widget", This);
                                 this.releaseBlobs(env);
                                 return React.createElement('div', null, `Failed to render component`);
                             }
                         }

                         releaseBlobs(env)
                         {
                             for (var i = 0; i < env.blobs; i++)
                                 Prolog.release_blob("widget", env.blobs[i]);
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
                                 var children = this.listToDOM(Prolog.term_arg(Term, 2));
                                 if (classes[tag] !== undefined)
                                 {
                                     // This is a Prolog-defined Proactive class
                                     var attributes = this.attributesToJS(Prolog.term_arg(Term, 1), true);
                                     console.log("Found these attributes for " + module);
                                     console.log(attributes);
                                     return React.createElement(classes[tag], attributes, children);
                                 }
                                 else if (ReactBootstrap[tag] !== undefined)
                                 {
                                     // This is a Bootstrap object
                                     var attributes = this.attributesToJS(Prolog.term_arg(Term, 1));
                                     return React.createElement(ReactBootstrap[tag], attributes, children);
                                 }
                                 else
                                 {
                                     // This is a plain old HTML object
                                     var attributes = this.attributesToJS(Prolog.term_arg(Term, 1));
                                     return React.createElement(tag, attributes, children);
                                 }

                             }
                             else if (Prolog.is_constant(Term))
                             {
                                 console.log("Here: " + Prolog.portray(Term));
                                 return Prolog.portray(Term);
                             }
                             else
                             {
                                 console.log("Unexpected DOM: " + Prolog.portray(Term));
                             }
                             return null;
                         }

                         attributesToJS(Term, pure)
                         {
                             console.log("Attributes: " + Prolog.portray(Term));
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
                                     else if (pure)
                                     {
                                         map[name] = PrologUtilities.prologToJS(Value);
                                     }
                                     else if (Prolog.is_atom(Value))
                                     {
                                         map[name] = Prolog.atom_chars(Value);
                                     }
                                     else if (Prolog.is_compound(Value) && Prolog.term_functor(Value) == Constants.dictFunctor) // FIXME: Suspect?
                                     {
                                         map[name] = PrologUtilities.dictEntriesToJS(Value);
                                     }
                                     else if (Prolog.is_compound(Value))
                                     {
                                         map[name] = PrologUtilities.prologToJS(Value);
                                     }
                                     else
                                     {
                                         console.log("Unhandled attribute value: " + Prolog.portray(Value));
                                     }
                                 }
                             }
                             if (Term != Constants.emptyListAtom)
                                 console.log("Bad list in attributesToJS");
                             console.log(map);
                             return map;
                         }

                         makeEventHandler(Term)
                         {
                             // FIXME: These Handler terms are never cleaned up
                             // FIXME: Also, if we have a $this term then we need to take some measures right away!
                             var target = this;
                             console.log("Making handler from: " + Term + " = " + Prolog.portray(Term)) ;
                             while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.thisFunctor)
                             {
                                 var Blob = Prolog.term_arg(Term, 0);
                                 console.log("Blob: " + Prolog.portray(Blob));
                                 target = Prolog.get_blob("widget", Blob);
                                 Term = Prolog.term_arg(Term, 1);
                             }
                             var handler = PrologUtilities.prologToJS(Term);
                             return function(e)
                             {
                                 var NewState = Prolog.make_variable();
                                 var PrologEvent = Prolog.make_variable(); // FIXME: Put a representation of e in here
                                 var State = this.make_dict(this.state);
                                 var Props = this.make_dict(this.props);
                                 var Handler = PrologUtilities.jsToProlog(handler);
                                 var Goal = Prolog.make_compound(Constants.crossModuleCallFunctor,
                                                                 [Prolog.make_atom(target.module), Prolog.make_compound(Handler,
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
                                                        var newState = PrologUtilities.prologToJS(NewState);
                                                        Prolog.restore_state(checkpoint);
                                                        target.setState(newState);
                                                    }
                                                    else
                                                        Prolog.restore_state(checkpoint);
                                                });
                             }.bind(this);
                         }

                     }
                 };

             }};
