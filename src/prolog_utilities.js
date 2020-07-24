var Prolog = require('proscript2');
var Constants = require('./constants');

function jsToProlog(js, nullIfBad)
{
    if (js == null)
        return Null();
    if (js === undefined)
        return Null();
    else if (js.atom !== undefined)
        return Prolog.make_atom(js.atom);
    else if (js.integer !== undefined)
        return Prolog.make_integer(js.integer);
    else if (js.compound !== undefined)
    {
        var args = [];
        for (var i = 0; i < js.compound.args.length; i++)
            args.push(jsToProlog(js.compound.args[i]));
        return Prolog.make_compound(Prolog.make_functor(Prolog.make_atom(js.compound.name), js.compound.args.length), args);
    }
    else if (js.widget !== undefined)
    {
        var Blob = Prolog.make_blob("widget", js.widget);
        if (this.blobs !== undefined)
            this.blobs.push(Blob);
        return Blob;
    }
    else if (js.list !== undefined)
    {
        var List = Constants.emptyListAtom;
        for (var i = js.list.length-1; i >= 0; i--)
            List = Prolog.make_compound(Constants.listFunctor, [jsToProlog(js.list[i]), List]);
        return List;
    }
    else if (js.variable !== undefined)
    {
        return Prolog.make_variable();
    }
    else if (nullIfBad)
    {
        return Null();
    }
    else
    {
        console.log("Cannot convert");
        console.log(js);
        console.trace();
        return Prolog.make_atom("<unknown>");
    }
};

function Null()
{
    return Prolog.make_compound(Constants.curlyFunctor, [Constants.nullAtom]);
};

function listToJS(Term, decodeValueFn)
{
    var list = [];
    var Tail = Term;
    while (Prolog.is_compound(Tail) && Prolog.term_functor(Tail) == Constants.listFunctor)
    {
        list.push(decodeValueFn(Prolog.term_arg(Tail, 0)));
        Tail = Prolog.term_arg(Tail, 1);
    }
    if (Tail != Constants.emptyListAtom)
        console.log("Warning: improper list: " + Prolog.portray(Term));
    return list;
}

// This returns a normal-looking JS object. It is not possible to recover the Prolog representation
function prologToJSNative(Term)
{
    Term = Prolog.deref(Term);
    if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.curlyFunctor)
    {
        if (Prolog.term_arg(Term, 0) == Constants.nullAtom)
            return null;
        return dictEntriesToJS(Prolog.term_arg(Term, 0), prologToJSNative);
    }
    if (Term == Constants.emptyCurlyAtom)
    {
        return {};
    }
    else if (Prolog.is_atom(Term))
    {
        return Prolog.atom_chars(Term);
    }
    else if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.listFunctor)
    {
        return listToJS(Term, prologToJSNative);
    }
    else if (Prolog.is_integer(Term))
    {
        return Prolog.numeric_value(Term);
    }
    else if (Prolog.is_float(Term))
    {
        return Prolog.numeric_value(Term);
    }
    else if (Prolog.is_blob(Term, "widget"))
    {
        return Prolog.get_blob("widget", Term);
    }
    else
    {
        console.log("Warning: Cannot convert " + Prolog.portray(Term) + " to a native JS object");
        return null;
    }
}

// This serializes Prolog as JS. The object contains enough info to recover the original Prolog representation (except for variables)
function prologToJS(Term)
{
    Term = Prolog.deref(Term);
    if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.curlyFunctor)
    {
        if (Prolog.term_arg(Term, 0) == Constants.nullAtom)
            return null;
        return dictEntriesToJS(Prolog.term_arg(Term, 0), prologToJS);
    }
    if (Term == Constants.emptyCurlyAtom)
    {
        return {};
    }
    else if (Prolog.is_atom(Term))
    {
        return {atom: Prolog.atom_chars(Term)};
    }
    else if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.listFunctor)
    {
        return {list: listToJS(Term, prologToJS)};
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
    else if (Prolog.is_variable(Term))
    {
        return {variable: 0};
    }
    else
    {
        console.log("No JS for " + Prolog.portray(Term));
    }
};

function dictEntriesToJS(Term, decodeValueFn)
{
    var map = {};
    while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.dictFunctor)
    {
        var Head = Prolog.term_arg(Term, 0);
        Term = Prolog.term_arg(Term, 1);
        if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == Constants.dictPairFunctor)
        {
            var name = Prolog.atom_chars(Prolog.term_arg(Head, 0));
            var value = decodeValueFn(Prolog.term_arg(Head, 1));
            map[name] = value;
        }
    }
    // The tail of a dict is just any non-,/2 term
    if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.dictPairFunctor)
    {
        var name = Prolog.atom_chars(Prolog.term_arg(Term, 0));
        var value = decodeValueFn(Prolog.term_arg(Term, 1));
        map[name] = value;
    }
    return map;
};


module.exports = {jsToProlog: jsToProlog,
                  Null: Null,
                  prologToJS: prologToJS,
                  prologToJSNative: prologToJSNative,
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
                  },
                  make_event: function(e)
                  {
                      return {list: {compound: {name: "=", args: [{atom: "value"}, {atom: e.target.value}]}}};
                  }
                 };
