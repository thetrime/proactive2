"use strict";

var Prolog = require('proscript2');
var PrologUtilities = require('./prolog_utilities');
var qOp = Prolog.create_options();
Prolog.set_option(qOp, Prolog.make_atom("quoted"), Prolog.make_atom("true"));

module.exports = {connect: function(url, module, callback)
                  {
                      var listenURI = "ws" + url.substring(4) + "listen";
                      var ws;
                      ws = new WebSocket(listenURI);
                      ws.state = "new";
                      ws.cleanup = function()
                      {
                          // FIXME: Indicate that we are not listening anymore. Attempt to reconnect
                          ws.close();
                      };
                      ws.onmessage = function(event)
                      {
                          console.log("Event: " + event.data);
                          var Message = Prolog.string_to_local_term(event.data);
                          callback(Message);
                          Prolog.free_local(Message);
                      };
                      ws.onopen = function()
                      {
                          ws.send(Prolog.format_term(qOp, 1200, Prolog.make_atom(module)));
                      }
                      ws.onerror = function(event)
                      {
                          // FIXME: Indicate that we are not listening anymore. Attempt to reconnect
                          console.log("WS error: " + event);
                          ws.close();
                      }
                  }};
