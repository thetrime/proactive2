"use strict";

var Prolog = require('proscript2');

module.exports = function(This)
{
    console.log("get_this/1:");
    console.log(this);
    return Prolog.unify(This, this._this);
}
