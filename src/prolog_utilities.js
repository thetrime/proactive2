var Prolog = require('proscript2');
var Constants = require('./constants');


module.exports = {jsListToProlog: function(js)
                  {
                      var list = [];
                      for (var i = 0; i < js.length; i++)
                          list.push(jsToProlog(js[i]));
                      return list;
                  },

                  jsToProlog: function(js)
                  {
                      if (js == null)
                          return this.Null();
                      if (js.atom !== undefined)
                          return Prolog.make_atom(js.atom);
                      if (js.integer !== undefined)
                          return Prolog.make_integer(js.integer);
                      else if (js.compound !== undefined)
                          return Prolog.make_compound(Prolog.make_functor(Prolog.make_atom(js.compound.name), js.compound.args.length), this.jsListToProlog(js.compound.args));
                  },
                  Null: function()
                  {
                      return Prolog.make_compound(Constants.curlyFunctor, [Constants.nullAtom]);
                  }};
