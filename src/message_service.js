"use strict";

var Prolog = require('proscript2');
var PrologUtilities = require('./prolog_utilities');
var Constants = require('./constants');
var qOp = Prolog.create_options();
Prolog.set_option(qOp, Prolog.make_atom("quoted"), Prolog.make_atom("true"));

var ws = null;
var listeners = {};

module.exports = {connect: function(url, rootModule, systemCallback)
                  {
                      var listenURI = "ws" + url.substring(4) + "listen";
                      ws = new WebSocket(listenURI);
                      ws.state = "disconnected";
                      ws.onmessage = function(event)
                      {
                          console.log("Event: " + event.data);
                          var Message = Prolog.string_to_local_term(event.data);
                          if (Prolog.is_compound(Message) && Prolog.term_functor(Message) == Constants.systemFunctor)
                          {
                              var SystemMessage = Prolog.term_arg(Message, 0);
                              systemCallback(SystemMessage);
                          }
                          else if (Prolog.is_compound(Message) && Prolog.term_functor(Message) == Constants.userFunctor)
                          {
                              var UserMessage = Prolog.term_arg(Message, 0);
                              var id = Prolog.numeric_value(Prolog.term_arg(Message, 1));
                              console.log("Dispatching event for " + id);
                              if (listeners[id] != null)
                                  listeners[id].callback(UserMessage);
                              else
                                  console.log("Message for non-existent listener " + id);
                          }
                          Prolog.free_local(Message);
                      };
                      ws.onopen = function()
                      {
                          console.log("Connected");
                          ws.send(Prolog.format_term(qOp, 1200, Prolog.make_atom(rootModule)));
                          ws.state = "connected";
                          console.log("Registering listeners");
                          var ids = Object.keys(listeners);
                          for (var i = 0; i < ids.length; i++)
                          {
                              var id = ids[i];
                              var clazz = listeners[id].clazz;
                              var discriminator = listeners[id].discriminator;
                              ws.send("register_for(" +
                                      Prolog.format_term(qOp, 1200, PrologUtilities.jsToProlog(clazz)) + ", " +
                                      Prolog.format_term(qOp, 1200, PrologUtilities.jsToProlog(discriminator)) + "," +
                                      Prolog.format_term(qOp, 1200, Prolog.make_integer(id)) + ").");
                          }
                          console.log("Registration complete");

                      }
                      ws.onerror = function(event)
                      {
                          ws.state = "disconnected";
                          // FIXME: Indicate that we are not listening anymore. Attempt to reconnect
                          console.log("WS error: " + event);
                          ws.close();
                      }
                  },
                  register: function(clazz, discriminator, id, callback)
                  {
                      listeners[id] = {discriminator: discriminator,
                                       clazz: clazz,
                                       callback: callback};
                      if (ws.state == "connected")
                      {
                          ws.send("register_for(" +
                                  Prolog.format_term(qOp, 1200, PrologUtilities.jsToProlog(clazz)) + ", " +
                                  Prolog.format_term(qOp, 1200, PrologUtilities.jsToProlog(discriminator)) + "," +
                                  Prolog.format_term(qOp, 1200, Prolog.make_integer(id)) + ").");
                      }

                  },
                  deregister: function(id)
                  {
                      listeners[id] = null;
                      ws.send("deregister(" + Prolog.format_term(qOp, 1200, Prolog.make_integer(id)) + ").");
                  }};

