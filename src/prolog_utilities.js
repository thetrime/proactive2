var Prolog = require('proscript2');
var Constants = require('./constants');

function jsToProlog(js)
{
    if (js == null)
        return Null();
    else if (js.atom !== undefined)
        return Prolog.make_atom(js.atom);
    else if (js.integer !== undefined)
        return Prolog.make_integer(js.integer);
    else if (js.compound !== undefined)
        return Prolog.make_compound(Prolog.make_functor(Prolog.make_atom(js.compound.name), js.compound.args.length), jsListToProlog(js.compound.args));
    else if (js.widget !== undefined)
    {
        var Blob = Prolog.make_blob("widget", js.widget);
        if (this.blobs !== undefined)
            this.blobs.push(Blob);
        return Blob;
    }
    else
    {
        return Prolog.make_atom("<unknown>");
    }
};

function jsListToProlog(js)
{
    var list = [];
    for (var i = 0; i < js.length; i++)
        list.push(jsToProlog(js[i]));
    return list;
};

function Null()
{
    return Prolog.make_compound(Constants.curlyFunctor, [Constants.nullAtom]);
};

function listToJS(Term)
{
    var list = [];
    var Tail = Term;
    while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.listFunctor)
    {
        list.push(prologToJS(Prolog.term_arg(Tail, 0)));
        Tail = Prolog.term_arg(Tail, 1);
    }
    if (Tail != Constants.emptyListAtom)
        console.log("Warning: improper list: " + Prolog.portray(Term));
    return list;

}

function prologToJS(Term, vars)
{
    if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.curlyFunctor)
    {
        return dictEntriesToJS(Prolog.term_arg(Term, 0));
    }
    else if (Prolog.is_atom(Term))
    {
        return {atom: Prolog.atom_chars(Term)};
    }
    else if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.listFunctor)
    {
        return {list: listToJS(Term)};
    }
    else if (Prolog.is_compound(Term))
    {
        var args = [];
        for (var i = 0; i < Prolog.term_functor_arity(Term); i++)
        {
            args.push(prologToJS(Prolog.term_arg(Term, i)));
        }
        return {compound: {name: Prolog.atom_chars(Prolog.term_functor_name(Term)), args: args}}
    }
    else if (Prolog.is_integer(Term))
    {
        return {integer: Prolog.numeric_value(Term)};
    }
    else if (Prolog.is_blob(Term, "widget"))
    {
        return {widget: Prolog.get_blob("widget", Term)};
    }
    else
    {
        console.log("No JS for " + Prolog.portray(Term));
    }
};

function dictEntriesToJS(Term)
{
    var map = {};
    while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.dictFunctor)
    {
        var Head = Prolog.term_arg(Term, 0);
        Term = Prolog.term_arg(Term, 1);
        if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == Constants.dictPairFunctor)
        {
            var name = Prolog.atom_chars(Prolog.term_arg(Head, 0));
            var value = prologToJS(Prolog.term_arg(Term, 1));
            map[name] = value;
        }
    }
    // The tail of a dict is just any non-,/2 term
    if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.dictPairFunctor)
    {
        var name = Prolog.atom_chars(Prolog.term_arg(Term, 0));
        var value = prologToJS(Prolog.term_arg(Term, 1));
        map[name] = value;
    }
    return map;
};


module.exports = {jsListToProlog: jsListToProlog,
                  jsToProlog: jsToProlog,
                  Null: Null,
                  prologToJS: prologToJS,
                  dictEntriesToJS: dictEntriesToJS,

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
