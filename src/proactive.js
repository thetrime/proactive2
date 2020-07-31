Prolog = require('proscript2');

var next_id = 0;

function isEventHandler(e)
{
    return e.startsWith("on");
}

function getExternalComponent(e)
{
    var o = externalClasses;
    if (e instanceof Array)
    {
        for (var i = 0; i < e.length; i++)
        {
            o = o[e[i]];
            if (o === undefined)
                return o;
        }
        return o;
    }
    else
        return o[e];
}

var pendingEvents = [];
var currentlyProcessingEvents = false;
var externalClasses = {};

function dispatchGlobalEvent()
{
    currentlyProcessingEvents = true;
    var event = pendingEvents.shift();
    event.caller.dispatchAnEvent(event);
}

Proactive = {installComponents: function(name, object)
             {
                 if (typeof object == "function")
                 {
                     var shim = {};
                     shim[name] = object;
                     Object.assign(externalClasses, shim);
                 }
                 else
                     Object.assign(externalClasses, object);
             },
             render: function(url, module, container)
             {
                 console.log("Welcome to Proactive 2.0");
                 var Constants = require('./constants');
                 var PrologUtilities = require('./prolog_utilities');
                 var MessageService = require('./message_service');
                 proactiveClasses = {};
                 specials = {'MessageListener': require('./message_listener')(MessageService)};
                 function make()
                 {
                     Prolog.hard_reset();
                     Prolog.define_foreign(".", require('./dot'));
                     Prolog.define_foreign("_on_server", require('./on_server')(url));
                     Prolog.define_foreign("get_this", require('./get_this'));
                     Prolog.define_foreign("media_size", require('./media_size'));
                     Prolog.define_foreign("get_ticks", require('./get_ticks'));
                     Prolog.define_foreign("bubble_event", require('./bubble_event'));
                     Prolog.define_foreign("t7_now", require('./t7_now'));
                     Prolog.consult_url(url + "boilerplate.pl", function()
                                        {
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
                                                                       var modules = [];
                                                                       Prolog.forEach(Components,
                                                                                      function(Module)
                                                                                      {
                                                                                          modules.push(Prolog.atom_chars(Module));
                                                                                      },
                                                                                      function() {}
                                                                                     );
                                                                       Prolog.restore_state(checkpoint);
                                                                       for (var i = 0; i < modules.length; i++)
                                                                       {
                                                                           if (this.proactiveClasses[modules[i]] == undefined)
                                                                               newClasses[modules[i]] = defineProactiveComponent(modules[i]);
                                                                           else
                                                                               newClasses[modules[i]] = this.proactiveClasses[modules[i]];
                                                                       };
                                                                       this.proactiveClasses = newClasses;
                                                                       ReactDOM.render(React.createElement(proactiveClasses[module], null, []), container);
                                                                   }
                                                                   else if (rc == 4)
                                                                   {
                                                                       console.log("Exception getting components:" + Prolog.portray(Prolog.get_exception()));
                                                                       Prolog.clear_exception();
                                                                   }
                                                                   else
                                                                       console.log("Failed to get components: " + rc);

                                                               });
                                        });
                 }
                 make();
                 MessageService.connect(url, module, function(SystemMessage)
                                        {
                                            if (Prolog.is_compound(SystemMessage) && Prolog.term_functor(SystemMessage) == Constants.consultedFunctor)
                                            {
                                                make();
                                            }
                                        });

                 function defineProactiveComponent(module)
                 {
                     return class extends React.Component
                     {
                         constructor(props)
                         {
                             super(props);
                             this.blob = Prolog.make_blob("widget", this);
                             this.id = next_id++;
                             this.module = module;
                             this.state = {};
                             if (Prolog.exists_predicate(Prolog.make_atom(module), Constants.getInitialStateFunctor))
                                 this.mounted = false;
                             else
                                 this.mounted = true;
                         }

                         componentWillUnmount()
                         {
                             Prolog.release_blob("widget", this.blob);
                         }

                         componentDidMount()
                         {

                             if (Prolog.exists_predicate(Prolog.make_atom(module), Constants.getInitialStateFunctor))
                             {
                                 this.queueEvent({atom: "getInitialState"},
                                                 function(State) { return [this.props, State]}.bind(this),
                                                 function(success)
                                                 {
                                                     this.mounted = true;
                                                     if (!success)
                                                         this.setState({});
                                                 }.bind(this));
                             }
                             else
                                 this.mounted = true;
                         }

                         
                         // This assumes we only care about props set from Prolog
                         propsChanged(before, after)
                         {
                             var keys = Object.keys(after);
                             var checkpoint = Prolog.save_state()
                             for (var i = 0; i < keys.length; i++)
                             {
                                 var key = keys[i];
                                 var newValue = after[key];
                                 var oldValue = before[key];
                                 if (!Prolog.unify(PrologUtilities.jsToProlog(newValue), PrologUtilities.jsToProlog(oldValue)))
                                 {
                                     Prolog.restore_state(checkpoint);
                                     return true;
                                 }
                             }
                             Prolog.restore_state(checkpoint);
                             return false;
                         }

                         componentDidUpdate(prevProps)
                         {
                             if (Prolog.exists_predicate(Prolog.make_atom(module), Constants.getInitialStateFunctor))
                             {
                                 var keys = Object.keys(this.props);
                                 var checkpoint = Prolog.save_state();
                                 for (var i = 0; i < keys.length; i++)
                                 {
                                     var key = keys[i];
                                     var newValue = this.props[key];
                                     var oldValue = prevProps[key];
                                     if (!Prolog.unify(PrologUtilities.jsToProlog(newValue, true), PrologUtilities.jsToProlog(oldValue, true)))
                                     {
                                         Prolog.restore_state(checkpoint);
                                         this.queueEvent({atom: "getInitialState"},
                                                         function(State) { return [this.props, State]}.bind(this),
                                                         function(success)
                                                         {
                                                             if (!success)
                                                                 this.setState({});
                                                         }.bind(this));
                                         return;
                                     }
                                 }
                                 // Props have not meaningfully changed
                                 Prolog.restore_state(checkpoint);
                             }
                         }

                         queueEvent(handler, getArgs, afterEvent)
                         {
                             pendingEvents.push({caller: this,
                                                 handler: handler,
                                                 getArgs: getArgs,
                                                 afterEvent: afterEvent});
                             if (!currentlyProcessingEvents)
                                 dispatchGlobalEvent();

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
                             env._this = this.blob;
                         }

                         releaseEnvironment(env)
                         {
                             for (var i = 0; i < env.blobs.length; i++)
                             {
                                 if (Prolog.is_blob(env.blobs[i], "dict"))
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
                             Prolog.clear_exception();
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
                                 var arity = extraArgs.length + Prolog.term_functor_arity(Term);
                                 args = new Array(arity);
                                 Functor = Prolog.make_functor(Prolog.term_functor_name(Term), arity);
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
                                                         Prolog.clear_exception();
                                                         this.releaseEnvironment(env);
                                                     }.bind(this));
                         }

                         render()
                         {
                             if (!this.mounted)
                                 return React.createElement('div', null, `Loading...`);
                             var Form = Prolog.make_variable();
                             return this.callSynchronously(module, Constants.renderFunctor, [this.state, this.props, Form], function(rc)
                                                           {
                                                               switch(rc)
                                                               {
                                                                   case  2: console.log("Warning: render/3 was nondet"); /* Fall through */
                                                                   case  1: return this.listToDOM(Prolog.deref(Form), [])
                                                                   case  4: console.log("Exception in render/3:\n" + Prolog.portray(Prolog.get_exception())); /* Fall through */
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
                             return dom;
                         }

                         nodeToDOM(Term)
                         {
                             if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.elementFunctor)
                             {
                                 var Tag = Prolog.term_arg(Term, 0);
                                 var tag;
                                 if (Prolog.is_atom(Tag))
                                 {
                                     tag = Prolog.atom_chars(Tag);
                                 }
                                 else if (Prolog.is_compound(Tag) && Prolog.term_functor(Tag) == Constants.listFunctor)
                                 {
                                     tag = [];
                                     Prolog.forEach(Tag, function(i) { tag.push(Prolog.atom_chars(i)); }, function(e) { console.log("Bad list in tag: " + Prolog.portray(e));});
                                 }
                                 var children = [];
                                 this.listToDOM(Prolog.term_arg(Term, 2), children);
                                 if (specials[tag] !== undefined)
                                 {
                                     return React.createElement(specials[tag], specials[tag].attributesToJS(module, Prolog.term_arg(Term, 1)), 0);
                                 }
                                 if (proactiveClasses[tag] !== undefined)
                                 {
                                     // This is a Prolog-defined Proactive class. We want to convert the attributes to an object where each entry is of the form
                                     // <string> : <PrologValue>
                                     var attributes = {};
                                     PrologUtilities.forEachAttribute(Prolog.term_arg(Term, 1), function(name, Value)
                                                                      {
                                                                          attributes[name] = PrologUtilities.prologToPrologValue(Value);
                                                                      });
                                     return React.createElement.apply(this, [proactiveClasses[tag], attributes].concat(children));
                                 }
                                 else if (getExternalComponent(tag) !== undefined)
                                 {
                                     // This is an external class. We want to convert the attributes to an object where each entry is of the form
                                     // <string> : <JSValue>
                                     // In addition to the DOM version, we also make a few extra conversions here, handling class(Path) and selector(Reference)
                                     var attributes = {};
                                     PrologUtilities.forEachAttribute(Prolog.term_arg(Term, 1), function(name, Value)
                                                                      {
                                                                          if (name.startsWith('on'))
                                                                              attributes[name] = this.makeEventHandler(Value);
                                                                          else if (Prolog.is_compound(Value) && Prolog.term_functor(Value) == Constants.selectorFunctor)
                                                                          {
                                                                              var selector = Prolog.atom_chars(Prolog.term_arg(Value, 0));
                                                                              attributes[name] = function() { return document.querySelector(selector); };
                                                                          }
                                                                          else if (Prolog.is_compound(Value) && Prolog.term_functor(Value) == Constants.booleanFunctor)
                                                                          {
                                                                              attributes[name] = Prolog.atom_chars(Prolog.term_arg(Value, 0)) == "true";
                                                                          }
                                                                          else if (Prolog.is_compound(Value) && Prolog.term_functor(Value) == Constants.dateFunctor)
                                                                          {
                                                                              attributes[name] = new Date(Prolog.numeric_value(Prolog.term_arg(Value, 0)),
                                                                                                          Prolog.numeric_value(Prolog.term_arg(Value, 1)) - 1,
                                                                                                          Prolog.numeric_value(Prolog.term_arg(Value, 2)),
                                                                                                          Prolog.numeric_value(Prolog.term_arg(Value, 3)),
                                                                                                          Prolog.numeric_value(Prolog.term_arg(Value, 4)),
                                                                                                          Prolog.numeric_value(Prolog.term_arg(Value, 5)),
                                                                                                          Prolog.numeric_value(Prolog.term_arg(Value, 6)));
                                                                          }
                                                                          else if (Prolog.is_compound(Value) && Prolog.term_functor(Value) == Constants.classFunctor)
                                                                          {
                                                                              var ClassName = Prolog.term_arg(Value, 0);
                                                                              var path;
                                                                              if (Prolog.is_atom(ClassName))
                                                                              {
                                                                                  path = Prolog.atom_chars(ClassName);
                                                                              }
                                                                              else if (Prolog.is_compound(ClassName) && Prolog.term_functor(ClassName) == Constants.listFunctor)
                                                                              {
                                                                                  path = [];
                                                                                  Prolog.forEach(ClassName, function(i) { path.push(Prolog.atom_chars(i)); }, function(e) { console.log("Bad list in class: " + Prolog.portray(e));});
                                                                              }
                                                                              attributes[name] = getExternalComponent(path);
                                                                          }
                                                                          else
                                                                          {
                                                                              attributes[name] = PrologUtilities.prologToJSValue(Value);
                                                                          }
                                                                      }.bind(this));
                                     return React.createElement.apply(this, [getExternalComponent(tag), attributes].concat(children));
                                 }
                                 else
                                 {
                                     // This is a plain old HTML object. We want to convert the attributes to an object where each entry is of the form
                                     // <string> : <JSValue>
                                     var attributes = {};
                                     PrologUtilities.forEachAttribute(Prolog.term_arg(Term, 1), function(name, Value)
                                                                      {
                                                                          if (name.startsWith('on'))
                                                                              attributes[name] = this.makeEventHandler(Value);
                                                                          else
                                                                              attributes[name] = PrologUtilities.prologToJSValue(Value);
                                                                      }.bind(this));
                                     return React.createElement.apply(this, [tag, attributes].concat(children));
                                 }
                             }
                             else if (Term == Constants.emptyListAtom)
                             {
                                 // [] -> null
                                 return null;
                             }
                             else if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.curlyFunctor && Prolog.term_arg(Term, 0) == Constants.nullAtom)
                             {
                                 // {null} -> null
                                 return null;
                             }
                             else if (Prolog.is_constant(Term))
                             {
                                 return Prolog.portray(Term);
                             }
                             else
                             {
                                 //console.log("Unexpected DOM: " + Prolog.portray(Term));
                             }
                             return null;
                         }

                         processEvent(handler, event)
                         {
                             this.queueEvent(handler, function(NewState) { return [PrologUtilities.jsToProlog(event), this.state, this.props, NewState]}.bind(this));
                         }

                         dispatchAnEvent(event)
                         {
                             var checkpoint = Prolog.save_state();
                             var NewState = Prolog.make_variable();
                             var Handler = PrologUtilities.jsToProlog(event.handler);
                             this.callAsynchronously(this.module, Handler, event.getArgs(NewState), function(success)
                                                     {
                                                         if (event.afterEvent)
                                                             event.afterEvent(success);
                                                         if (success)
                                                         {
                                                             // This may trigger a Prolog goal (ie render/3), but it must be synchronous so when it is done we know
                                                             // that the WAM is in the same state as it was before we called it
                                                             this.setPrologState(NewState);
                                                         }
                                                         Prolog.restore_state(checkpoint);
                                                         if (pendingEvents.length == 0)
                                                             currentlyProcessingEvents = false;
                                                         else
                                                             dispatchGlobalEvent();
                                                     });
                         }

                         setPrologState(NewState)
                         {
                             // This is a convenience function to set the state based on a Prolog value rather than a JS Object
                             // Once it exits, it does not hold any references to NewState
                             // It is guaranteed to leave the WAM in the same state it was in when it started - all goals called are synchronous
                             var newState = {};
                             NewState = Prolog.deref(NewState);
                             if (Prolog.is_dict(NewState))
                             {
                                 PrologUtilities.forEachDictEntry(NewState, function(name, Value)
                                                                  {
                                                                      newState[name] = PrologUtilities.prologToPrologValue(Value);
                                                                  });
                             }
                             else if (Prolog.is_compound(NewState) && Prolog.term_functor(NewState) == Constants.curlyFunctor)
                             {
                                 // Perhaps this should not be allowed. It lets you write an output state as either:
                                 //   * a dict, as in _{foo: bar, ...}
                                 //   * a curly, as in {foo: bar, ...}
                                 // which is flexbile, but perhaps more rope than we want
                                 PrologUtilities.forEachCurlyEntry(NewState, function(name, Value)
                                                                   {
                                                                       newState[name] = PrologUtilities.prologToPrologValue(Value);
                                                                   });
                             }
                             // React merges states, but the merge is shallow, meaning that if we have any dicts in the old state they will just get clobbered by the new.
                             // This means we have to be a bit clever
                             this.setState((prevState) =>
                                           {
                                               var keys = Object.keys(prevState);
                                               for (var i = 0; i < keys.length; i++)
                                               {
                                                   var key = keys[i];
                                                   if (prevState[key] !== null &&
                                                       prevState[key].dict !== undefined &&
                                                       newState[key] !== undefined &&
                                                       newState[key] !== null &&
                                                       newState[key].dict !== undefined)
                                                   {
                                                       newState[key] = PrologUtilities.mergeDicts(prevState[key], newState[key]);
                                                   }
                                               }
                                               return newState;
                                           });
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
                             var handler = PrologUtilities.prologToPrologValue(Term);
                             return function(e)
                             {
                                 this.processEvent(handler, PrologUtilities.make_event(e));
                             }.bind(this);
                         }
                     }
                 };
             }};
