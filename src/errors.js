"use strict";

var Prolog = require('proscript2');
var Constants = require('./constants');


module.exports.typeError = function(expected, actual)
{
    Prolog.set_exception(Prolog.make_compound(Constants.errorFunctor, [Prolog.make_compound(Constants.typeErrorFunctor, [expected, actual]), Prolog.make_variable()]));
    return 0;
}


module.exports.systemError = function(message)
{
    Prolog.set_exception(Prolog.make_compound(Constants.errorFunctor, [Prolog.make_compound(Constants.systemErrorFunctor, [message]), Prolog.make_variable()]));
    return 0;
}

module.exports.permissionError = function(op, type, cause)
{
    Prolog.set_exception(Prolog.make_compound(Constants.errorFunctor, [Prolog.make_compound(Constants.permissionErrorFunctor, [op, type, cause]), Prolog.make_variable()]));
    return 0;
}
