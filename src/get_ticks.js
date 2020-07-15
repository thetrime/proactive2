"use strict";

var Prolog = require('proscript2');

module.exports = function(Ticks)
{
    return Prolog._nify(Prolog.make_float(performance.now()), Ticks);
}
