"use strict";

var Prolog = require('proscript2');
var Constants = require('./constants');

module.exports = function(Now)
{
    var date = new Date();
    return Prolog.unify(Now, Prolog.make_compound(Constants.t7_functor,
                                                  [Prolog.make_integer(date.getFullYear()),
                                                   Prolog.make_integer(date.getMonth() + 1),
                                                   Prolog.make_integer(date.getDate()),
                                                   Prolog.make_integer(date.getHours()),
                                                   Prolog.make_integer(date.getMinutes()),
                                                   Prolog.make_integer(date.getSeconds()),
                                                   Prolog.make_integer(date.getMilliseconds())]));
}
