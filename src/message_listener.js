"use strict";

var id = 0;
Prolog = require('proscript2');
var Constants = require('./constants');
var PrologUtilities = require('./prolog_utilities');

function makeMessageHandler(Term, module)
{
    var target = null;
    while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.thisFunctor)
    {
        var Blob = Prolog.term_arg(Term, 0);
        target = Prolog.get_blob("widget", Blob);
        Term = Prolog.term_arg(Term, 1);
    }
    var handler = PrologUtilities.prologToJS(Term);
    return function(Message)
    {
        target.processEvent(handler, PrologUtilities.prologToJS(Message));
    }
}


module.exports = function(MessageService)
{
    return class extends React.Component
    {
        constructor(props)
        {
            super(props);
            this.id = id++;
        }

        static attributesToJS(module, Term)
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
                    if (name == "onMessage")
                    {
                        map["onMessage"] = makeMessageHandler(Value, module);
                    }
                    else if (name == "discriminator")
                    {
                        map["discriminator"] = {compound: {name: ":", args: [{atom: module}, PrologUtilities.prologToJS(Value)]}};
                    }
                    else
                        map[name] = PrologUtilities.prologToJS(Value);
                }
            }
            return map;
        }

        render()
        {
            return null;
        }

        componentDidMount()
        {
            // Register with the message service
            MessageService.register(this.props['class'], this.props['discriminator'], this.id, this.messageReceived.bind(this));
        }

        componentDidUpdate(prevProps)
        {
            if (Prolog.unify(PrologUtilities.jsToProlog(this.props['class']), PrologUtilities.jsToProlog(prevProps['class'])) &&
                Prolog.unify(PrologUtilities.jsToProlog(this.props['discriminator']), PrologUtilities.jsToProlog(prevProps['discriminator'])))
            {
                // No material change. Just do nothing
                return;
            }
            // Update registrations with the message service
            MessageService.deregister(this.id);
            MessageService.register(this.props['class'], this.props['discriminator'], this.id, this.messageReceived.bind(this));
        }

        componentWillUnmount()
        {
            // Deregister from the message service
            MessageService.deregister(this.id);
        }

        messageReceived(message)
        {
            if (this.props.onMessage != null)
                this.props.onMessage(message);
            else
                console.log("Listener has no onMessage defined");
        }
    }
}
