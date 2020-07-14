"use strict";

var Prolog = require('proscript2');

module.exports = {getInitialStateFunctor: Prolog.make_functor(Prolog.make_atom("getInitialState"), 2),
                  renderFunctor: Prolog.make_functor(Prolog.make_atom("render"), 3),
                  crossModuleCallFunctor: Prolog.make_functor(Prolog.make_atom(":"), 2),
                  elementFunctor: Prolog.make_functor(Prolog.make_atom("element"), 3),
                  equalsFunctor: Prolog.make_functor(Prolog.make_atom("="), 2),
                  listFunctor: Prolog.make_functor(Prolog.make_atom("."), 2),
                  dictFunctor: Prolog.make_functor(Prolog.make_atom(","), 2),
                  dictPairFunctor: Prolog.make_functor(Prolog.make_atom(":"), 2),
                  curlyFunctor: Prolog.make_functor(Prolog.make_atom("{}"), 1),
                  emptyListAtom: Prolog.make_atom("[]"),
                  nullAtom: Prolog.make_atom("null")};