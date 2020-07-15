"use strict";

var Prolog = require('proscript2');

module.exports = function(This)
{
    return Prolog.unify(This, this._this);
}
