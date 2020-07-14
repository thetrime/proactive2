"use strict";

var Prolog = require('proscript2');
var Constants = require('./constants');


var qOp = null;
var foreign = 0;

function indicateBusy()
{
    // FIXME: Implement
}

function indicateReady()
{
    // FIXME: Implement
}

module.exports = function(url)
{
    var goalURI = "ws" + url.substring(4) + "/goal";
    return function(Goal)
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
}
