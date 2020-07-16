Prolog = require('proscript2');

var next_id = 0;

function isEventHandler(e)
{
    return e.startsWith("on");
}

Proactive = {render: function(url, module, container)
             {
                 console.log("Welcome to Proactive 2.0");
                 var Constants = require('./constants');
                 var PrologUtilities = require('./prolog_utilities');
                 var MessageService = require('./message_service');
                 classes = {};

                 function make()
                 {
                     Prolog.hard_reset();
                     Prolog.define_foreign(".", require('./dot'));
                     Prolog.define_foreign("on_server", require('./on_server')(url));
                     Prolog.define_foreign("get_this", require('./get_this'));
                     Prolog.define_foreign("media_size", require('./media_size'));
                     Prolog.define_foreign("get_ticks", require('./get_ticks'));
                     Prolog.define_foreign("bubble_event", require('./bubble_event'));
                     Prolog.consult_url(url + "boilerplate.pl", function() {});
                     Prolog.consult_url(url + "component/" + module, function()
                                        {
                                            var checkpoint = Prolog.save_state();
                                            var Components = Prolog.make_variable();
                                            var Goal = Prolog.make_compound(Prolog.make_functor(Prolog.make_atom("get_components"), 1), [Components]);
                                            // Note that we cannot us callSynchronously() here because that is a method of React.Component, and we are just in Proactive here
                                            // Fortunately, we dont need to clean much up - get_components/1 does not create any blobs
                                            var rc = Prolog.call({}, Goal);
                                            var newClasses = {};
                                            if (rc == 1)
                                            {
                                                Prolog.forEach(Components,
                                                               function(Module)
                                                               {
                                                                   if (this.classes[Prolog.atom_chars(Module)] == undefined)
                                                                       newClasses[Prolog.atom_chars(Module)] = defineProactiveComponent(Prolog.atom_chars(Module));
                                                                   else
                                                                       newClasses[Prolog.atom_chars(Module)] = this.classes[Prolog.atom_chars(Module)];
                                                               },
                                                               function() {}
                                                              );
                                                this.classes = newClasses;
                                                ReactDOM.render(React.createElement(classes[module], null, []), container);
                                            }
                                            else if (rc == 4)
                                            {
                                                console.log("Exception getting components:" + Prolog.portray(Prolog.get_exception()));
                                            }
                                            else
                                                console.log("Failed to get components: " + rc);
                                            Prolog.restore_state(checkpoint);

                                        });
                 }
                 make();
                 MessageService.connect(url, module, function(Message)
                                        {
                                            if (Prolog.is_compound(Message) && Prolog.term_functor(Message) == Constants.systemFunctor)
                                            {
                                                var SystemMessage = Prolog.term_arg(Message, 0);
                                                if (Prolog.is_compound(SystemMessage) && Prolog.term_functor(SystemMessage) == Constants.consultedFunctor)
                                                {
                                                    make();
                                                }
                                            }
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
                             if (Prolog.exists_predicate(Prolog.make_atom(module), Constants.getInitialStateFunctor))
                             {
                                 var State = Prolog.make_variable();
                                 var rc = this.callSynchronously(module, Constants.getInitialStateFunctor, [this.props, State], function(rc)
                                                                 {
                                                                     if (rc == 1)
                                                                     {
                                                                         this.state = PrologUtilities.prologToJS(Prolog.deref(State));
                                                                     }
                                                                     else
                                                                     {
                                                                         this.state = {}
                                                                     }
                                                                 });
                             }
                             else
                             {
                                 this.state = {}
                             }
                         }

                         prepareEnvironment(args, env)
                         {
                             for (var i = 0; i < args.length; i++)
                             {
                                 if (args[i] == this.props)
                                 {
                                     var Props =  Prolog.make_blob("dict", {portray: PrologUtilities.portray_dict, data: this.props});
                                     env.blobs.push(Props);
                                     args[i] = Props;
                                 }
                                 else if (args[i] == this.state)
                                 {
                                     var State = Prolog.make_blob("dict", {portray: PrologUtilities.portray_dict, data: this.state});
                                     env.blobs.push(State);
                                     args[i] = State;
                                 }
                             }
                             var This = Prolog.make_blob("widget", this);
                             env.blobs.push(This);
                             env._this = This;
                         }

                         releaseEnvironment(env)
                         {
                             for (var i = 0; i < env.blobs.length; i++)
                             {
                                 if (Prolog.is_blob(env.blobs[i], "widget"))
                                     Prolog.release_blob("widget", env.blobs[i]);
                                 else if (Prolog.is_blob(env.blobs[i], "dict"))
                                     Prolog.release_blob("dict", env.blobs[i]);
                                 else
                                     console.log("Warning: Unexpected blob in environment");
                             }
                         }

                         callSynchronously(module, Functor, args, handler)
                         {
                             var env = {blobs: []};
                             var checkpoint = Prolog.save_state();
                             this.prepareEnvironment(args, env);
                             var Goal = Prolog.make_compound(Constants.crossModuleCallFunctor,
                                                             [Prolog.make_atom(module), Prolog.make_compound(Functor, args)]);
                             var rc = Prolog.call(env, Goal);
                             var result = handler.call(this, rc);
                             Prolog.restore_state(checkpoint);
                             this.releaseEnvironment(env);
                             return result;
                         }

                         callAsynchronously(module, Term, extraArgs, handler)
                         {
                             // Note that the second arg here is a Prolog term. This means you can callAsynchronously(foo(bar), [baz]) to get foo(bar, baz)
                             // Also note that it is not safe for callAsynchronously() to push and pop the machine state because of the way the callback may be called
                             // For example, suppose that your callAsynchronously() call ends up invoking a second one. We push state 1, then state 2.
                             // The callbacks are run in reverse order, though - outermost first - so we (try to) pop state 1 first, leading to memory corruption
                             // This means that you must save the state at the very first call to callAsynchronously() yourself, and subsequent calls should
                             // just be treated as a single long invocation, without saving any states.
                             var env = {blobs: []};
                             var Functor;
                             var args = [];
                             this.prepareEnvironment(extraArgs, env);
                             if (Prolog.is_atom(Term))
                             {
                                 Functor = Prolog.make_functor(Term, extraArgs.length);
                                 args = extraArgs;
                             }
                             else if (Prolog.is_compound(Term))
                             {
                                 args = new Array[extraArgs.length + Prolog.term_functor_arity(Term)];
                                 Functor = Prolog.term_functor(Term);
                                 var i = 0;
                                 for (i = 0; i < Prolog.term_functor_arity(Term); i++)
                                     args[i] = Prolog.term_arg(Term, i);
                                 for (var j = 0; j < extraArgs.length; j++)
                                     args[i++] = extraArgs[j];
                             }
                             var Goal = Prolog.make_compound(Constants.crossModuleCallFunctor,
                                                             [Prolog.make_atom(module), Prolog.make_compound(Functor, args)]);
                             var rc = Prolog.execute(env, Goal, function(success)
                                                     {
                                                         handler.call(this, success);
                                                         this.releaseEnvironment(env);
                                                     }.bind(this));
                         }

                         make_dict(data)
                         {
                             return Prolog.make_blob("dict", {portray: PrologUtilities.portray_dict, data: data});
                         }

                         render()
                         {
                             var Form = Prolog.make_variable();
                             return this.callSynchronously(module, Constants.renderFunctor, [this.state, this.props, Form], function(rc)
                                                           {
                                                               switch(rc)
                                                               {
                                                                   case  2: console.log("Warning: render/3 was nondet"); /* Fall through */
                                                                   case  1: return this.nodeToDOM(Prolog.deref(Form));
                                                                   case  4: console.log("Exception in render/3:" + Prolog.portray(Prolog.get_exception())); /* Fall through */
                                                                   default: return React.createElement('div', null, `Failed to render component`);
                                                               }
                                                           });
                         }

                         listToDOM(List, dom)
                         {
                             while (Prolog.is_compound(List) && Prolog.term_functor(List) == Constants.listFunctor)
                             {
                                 var Head = Prolog.term_arg(List, 0);
                                 if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == Constants.listFunctor)
                                     this.listToDOM(Head, dom);
                                 else
                                     dom.push(this.nodeToDOM(Head));
                                 List = Prolog.term_arg(List, 1);
                             }
                             if (List != Constants.emptyListAtom)
                                 console.log("Bad list in listToDOM");
                         }

                         nodeToDOM(Term)
                         {
                             if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.elementFunctor)
                             {
                                 var tag = Prolog.atom_chars(Prolog.term_arg(Term, 0));
                                 var children = [];
                                 this.listToDOM(Prolog.term_arg(Term, 2), children);
                                 if (classes[tag] !== undefined)
                                 {
                                     // This is a Prolog-defined Proactive class
                                     var attributes = this.attributesToJS(Prolog.term_arg(Term, 1), true);
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
                                     if (children.length == 0)
                                         return React.createElement(tag, attributes);
                                     else
                                         return React.createElement(tag, attributes, children);
                                 }

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

                         attributesToJS(Term, pure)
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
                             return map;
                         }

                         makeEventHandler(Term)
                         {
                             var target = this;
                             while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.thisFunctor)
                             {
                                 var Blob = Prolog.term_arg(Term, 0);
                                 target = Prolog.get_blob("widget", Blob);
                                 Term = Prolog.term_arg(Term, 1);
                             }
                             var handler = PrologUtilities.prologToJS(Term);
                             return function(e)
                             {
                                 var PrologEvent = PrologUtilities.make_event(e);
                                 var NewState = Prolog.make_variable();
                                 var Handler = PrologUtilities.jsToProlog(handler);
                                 var checkpoint = Prolog.save_state();
                                 this.callAsynchronously(target.module, Handler, [PrologEvent, this.state, this.props, NewState], function(success)
                                                         {
                                                             if (success)
                                                             {
                                                                 var newState = PrologUtilities.prologToJS(Prolog.deref(NewState));
                                                                 target.setState(newState);
                                                             }
                                                             Prolog.restore_state(checkpoint);
                                                         });
                             }.bind(this);
                         }
                     }
                 };
             }};
