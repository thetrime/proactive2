var Prolog = require('proscript2');
var Constants = require('./constants');

function jsToProlog(js)
{
    if (js == null)
        return this.Null();
    if (js.atom !== undefined)
        return Prolog.make_atom(js.atom);
    if (js.integer !== undefined)
        return Prolog.make_integer(js.integer);
    else if (js.compound !== undefined)
        return Prolog.make_compound(Prolog.make_functor(Prolog.make_atom(js.compound.name), js.compound.args.length), this.jsListToProlog(js.compound.args));
};

function jsListToProlog(js)
{
    var list = [];
    for (var i = 0; i < js.length; i++)
        list.push(jsToProlog(js[i]));
    return list;
};

module.exports = {jsListToProlog: jsListToProlog,
                  jsToProlog: jsToProlog,
                  Null: function()
                  {
                      return Prolog.make_compound(Constants.curlyFunctor, [Constants.nullAtom]);
                  },

                  portray_dict: function(options, precedence)
                  {
                      var output = "{";
                      var keys = Object.keys(this.data);
                      for (var i = 0 ; i < keys.length; i++)
                      {
                          output += Prolog.format_term(options, precedence, Prolog.make_compound(Constants.dictPairFunctor, [Prolog.make_atom(keys[i]),
                                                                                                                             jsToProlog(this.data[keys[i]])]));
                          if (i+1 < keys.length)
                              output += ",";
                      }
                      return output + "}";
                  }
                 };
