"use strict";

var Prolog = require('proscript2');
var Constants = require('./constants');
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
    if (!Prolog.is_blob(State, "dict"))
    {
        // FIXME: Errors does not exist. Neither does Constants.
        return Errors.typeError(Constants.prologStateAtom, State);
    }
    if (Prolog.is_atom(Key))
    {
        var key = Prolog.atom_chars(Key);
        var value = Prolog.get_blob("dict", State).data[key];
        if (value === undefined)
            return Null(Value);
        else
            return Prolog.unify(Value, PrologUtilities.jsToProlog(value));
    }
    throw new Error("Oops2");
    return 0;
}
