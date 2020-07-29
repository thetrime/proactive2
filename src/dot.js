"use strict";

var Prolog = require('proscript2');
var Constants = require('./constants');
var Errors = require('./errors');
var PrologUtilities = require('./prolog_utilities');


function isNull(t)
{
    return (Prolog.is_compound(t) && Prolog.term_functor(t) == Constants.curlyFunctor && Prolog.term_arg(t, 0) == Constants.nullAtom);
}

function Null(value)
{
    return Prolog.unify(value, Prolog.make_compound(Constants.curlyFunctor, [Constants.nullAtom]));
}

module.exports = function(State, Key, Value)
{
    if (isNull(State))
    {
        return Prolog.unify(Value, Prolog.make_compound(Constants.curlyFunctor, [Constants.nullAtom]));
    }
    if (Prolog.is_blob(State, "dict"))
    {
        if (Prolog.is_atom(Key))
        {
            var key = Prolog.atom_chars(Key);
            var value = Prolog.get_blob("dict", State).data[key];
            if (value === undefined)
                return Null(Value);
            else
                return Prolog.unify(Value, PrologUtilities.jsToProlog(value));
        }
    }
    // Also handle the case where we use . with a {} term from an internally-generated state
    if (State == Constants.emptyCurlyAtom)
        return Null(Value);
    if (Prolog.is_dict(State))
    {
        State = Prolog.term_arg(State, 1);
        // Then try again
    }
    if (Prolog.is_compound(State) && Prolog.term_functor(State) == Constants.curlyFunctor)
    {
        var Term = Prolog.term_arg(State, 0);
        if (Term == Constants.nullAtom)
            return Null(Value);
        while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.dictFunctor)
        {
            var Head = Prolog.term_arg(Term, 0);
            console.log("Head is " + Prolog.portray(Head));
            Term = Prolog.term_arg(Term, 1);
            if (Prolog.unify(Key, Prolog.term_arg(Head, 0)))
                return Prolog.unify(Value, Prolog.term_arg(Head, 1));
        }
        // Handle tail
        if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.dictPairFunctor)
        {
            if (Prolog.unify(Key, Prolog.term_arg(Term, 0)))
                return Prolog.unify(Value, Prolog.term_arg(Term, 1));
        }
        return Null(Value);
    }
    return Errors.typeError(Constants.prologStateAtom, State);
}
