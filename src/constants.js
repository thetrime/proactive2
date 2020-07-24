"use strict";

var Prolog = require('proscript2');

module.exports = {getInitialStateFunctor: Prolog.make_functor(Prolog.make_atom("getInitialState"), 2),
                  getInitialStateAtom: Prolog.make_atom("getInitialState"),
                  renderFunctor: Prolog.make_functor(Prolog.make_atom("render"), 3),
                  crossModuleCallFunctor: Prolog.make_functor(Prolog.make_atom(":"), 2),
                  elementFunctor: Prolog.make_functor(Prolog.make_atom("element"), 3),
                  equalsFunctor: Prolog.make_functor(Prolog.make_atom("="), 2),
                  listFunctor: Prolog.make_functor(Prolog.make_atom("."), 2),
                  dictFunctor: Prolog.make_functor(Prolog.make_atom(","), 2),
                  dictPairFunctor: Prolog.make_functor(Prolog.make_atom(":"), 2),
                  errorFunctor: Prolog.make_functor(Prolog.make_atom("error"), 1),
                  typeErrorFunctor: Prolog.make_functor(Prolog.make_atom("type_error"), 2),
                  systemErrorFunctor: Prolog.make_functor(Prolog.make_atom("system_error"), 1),
                  permissionErrorFunctor: Prolog.make_functor(Prolog.make_atom("permission_error"), 3),
                  curlyFunctor: Prolog.make_functor(Prolog.make_atom("{}"), 1),
                  emptyCurlyAtom: Prolog.make_atom("{}"),
                  emptyListAtom: Prolog.make_atom("[]"),
                  nullAtom: Prolog.make_atom("null"),
                  cutFunctor: Prolog.make_functor(Prolog.make_atom("cut"), 1),
                  exceptionFunctor : Prolog.make_functor(Prolog.make_atom("exception"), 1),
                  thisFunctor : Prolog.make_functor(Prolog.make_atom("$this"), 2),
                  failAtom: Prolog.make_atom("fail"),
                  abortedAtom: Prolog.make_atom("$aborted"),
                  systemFunctor: Prolog.make_functor(Prolog.make_atom("system"), 1),
                  userFunctor: Prolog.make_functor(Prolog.make_atom("user"), 2),
                  consultedFunctor: Prolog.make_functor(Prolog.make_atom("consulted"), 1),
                  classFunctor: Prolog.make_functor(Prolog.make_atom("class"), 1),
                  booleanFunctor: Prolog.make_functor(Prolog.make_atom("boolean"), 1),
                  selectorFunctor: Prolog.make_functor(Prolog.make_atom("selector"), 1)};

