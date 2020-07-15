"use strict";

var Prolog = require('proscript2');

module.exports = function(Width, Height)
{
    return Prolog.unify(Width, Prolog.make_integer(document.body.clientWidth)) && Prolog.unify(Height, Prolog.make_integer(document.body.clientHeight));
}
