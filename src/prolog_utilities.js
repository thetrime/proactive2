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
        return js.widget;
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
    else if (js.dict !== undefined)
    {
        var Tag = jsToProlog(js.dict.tag);
        var Data;
        if (js.dict.entries.length == 0)
        {
            Data = Constants.emptyCurlyAtom;
        }
        else
        {
            Data = Prolog.make_compound(Constants.dictPairFunctor, [Prolog.make_atom(js.dict.entries[js.dict.entries.length-1].name),
                                                                    jsToProlog(js.dict.entries[js.dict.entries.length-1].value)]);
            for (var i = js.dict.entries.length-2; i >=0; i--)
                Data = Prolog.make_compound(Constants.commaFunctor, [Prolog.make_compound(Constants.dictPairFunctor, [Prolog.make_atom(js.dict.entries[i].name),
                                                                                                                      jsToProlog(js.dict.entries[i].value)])
                                                                     , Data]);
            Data = Prolog.make_compound(Constants.curlyFunctor, [Data]);
        }
        return Prolog.make_dict(Tag, Data)
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
        return Prolog.make_atom("<unknownx>");
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
function prologToJSValue(Term)
{
    Term = Prolog.deref(Term);
    if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.curlyFunctor && Prolog.term_arg(Term, 0) == Constants.nullAtom)
    {
        return null;
    }
    if (Term == Constants.emptyCurlyAtom)
    {
        return {};
    }
    if (Prolog.is_dict(Term))
    {
        var map = {};
        forEachDictEntry(Term, function(name, Value)
                         {
                             map[name] = prologToJSValue(Value);
                         });
        return map;
    }
    else if (Prolog.is_atom(Term))
    {
        return Prolog.atom_chars(Term);
    }
    else if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.listFunctor)
    {
        return listToJS(Term, prologToJSValue);
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
function prologToPrologValue(Term)
{
    Term = Prolog.deref(Term);
    if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.curlyFunctor && Prolog.term_arg(Term, 0) == Constants.nullAtom)
    {
        return null;
    }
    if (Term == Constants.emptyCurlyAtom)
    {
        return {};
    }
    if (Prolog.is_dict(Term))
    {
        var entries = [];
        forEachDictEntry(Term, function(name, Value)
                         {
                             entries.push({name: name, value:prologToPrologValue(Value)});
                         });
        return {dict: {entries: entries,
                       tag: prologToPrologValue(Prolog.dict_tag(Term))}};
    }
    else if (Prolog.is_atom(Term))
    {
        return {atom: Prolog.atom_chars(Term)};
    }
    else if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.listFunctor)
    {
        return {list: listToJS(Term, prologToPrologValue)};
    }
    else if (Prolog.is_compound(Term))
    {
        var args = [];
        for (var i = 0; i < Prolog.term_functor_arity(Term); i++)
        {
            args.push(prologToPrologValue(Prolog.term_arg(Term, i)));
        }
        return {compound: {name: Prolog.atom_chars(Prolog.term_functor_name(Term)), args: args}}
    }
    else if (Prolog.is_integer(Term))
    {
        return {integer: Prolog.numeric_value(Term)};
    }
    else if (Prolog.is_blob(Term, "widget"))
    {
        return {widget: Term};
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


function forEachAttribute(Term, fn)
{
    while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.listFunctor)
    {
        var Head = Prolog.term_arg(Term, 0);
        Term = Prolog.term_arg(Term, 1);
        if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == Constants.equalsFunctor)
        {
            var name = Prolog.atom_chars(Prolog.term_arg(Head, 0));
            var Value = Prolog.term_arg(Head, 1);
            fn(name, Value);
        }
        else
            console.log("Bad pair in forEachAttribute");
    }
    if (Term != Constants.emptyListAtom)
        console.log("Bad list in forEachAttribute");
};

function forEachDictEntry(Dict, fn)
{
    var Term = Prolog.term_arg(Dict, 1);
    if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.curlyFunctor)
        Term = Prolog.term_arg(Term, 0);
    else if (Term == Constants.emptyCurlyAtom)
    {
        // No entries, no problems.
        return;
    }
    else
    {
        console.log("Not a {}/1: " + Prolog.portray(Term));
        return;
    }
    forEachCurlyEntry(Term, fn);
}

function forEachCurlyEntry(Term, fn)
{
    if (Term == Constants.emptyCurlyAtom)
        return;
    while (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.commaFunctor)
    {
        var Head = Prolog.term_arg(Term, 0);
        Term = Prolog.term_arg(Term, 1);
        if (Prolog.is_compound(Head) && Prolog.term_functor(Head) == Constants.dictPairFunctor)
        {
            var name = Prolog.atom_chars(Prolog.term_arg(Head, 0));
            var Value = Prolog.term_arg(Head, 1);
            fn(name, Value);
        }
        else
            console.log("Bad pair in forEachDictEntry");
    }
    if (Prolog.is_compound(Term) && Prolog.term_functor(Term) == Constants.dictPairFunctor)
    {
        var name = Prolog.atom_chars(Prolog.term_arg(Term, 0));
        var Value = Prolog.term_arg(Term, 1);
        fn(name, Value);
    }
    else
        console.log("Bad pair in forEachDictEntry");
};

// This is pretty inefficient, but dicts tend to be relatively small. If we ever get any really big ones, consider
// refactoring this to build an intermediate hashmap. That will eliminate the inner loop, making it O(3n) rather than O(n*n)
function mergeDicts(oldDict, newDict)
{
    var resultEntries = oldDict.dict.entries;
    var keys = [];
    for (var i = 0; i < resultEntries.length; i++)
    {
        keys.push(resultEntries[i].name);
        for (var j = 0; j < newDict.dict.entries.length; j++)
        {
            if (resultEntries[i].name == newDict.dict.entries[j].name)
            {
                // Replace old with new.... unless the values are also both dicts
                if (resultEntries[i].value !== null &&
                    resultEntries[i].value.dict !== undefined &&
                    newDict.dict.entries[j].value !== null &&
                    newDict.dict.entries[j].value.dict !== undefined)
                    resultEntries[i].value = mergeDicts(resultEntries[i].value, newDict.dict.entries[j].value);
                else
                    resultEntries[i].value = newDict.dict.entries[j].value;
                break;
            }
        }
    }
    // Finally, add in all the new entries
    for (var j = 0; j < newDict.dict.entries.length; j++)
    {
        if (!keys.includes(newDict.dict.entries[j].name))
        {
            resultEntries.push(newDict.dict.entries[j]);
        }
    }

    return {dict: {tag: oldDict.dict.tag,
                   entries: resultEntries}};
}


module.exports = {jsToProlog: jsToProlog,
                  Null: Null,
                  prologToPrologValue: prologToPrologValue,
                  prologToJSValue: prologToJSValue,
                  mergeDicts: mergeDicts,
                  forEachAttribute: forEachAttribute,
                  forEachDictEntry: forEachDictEntry,
                  forEachCurlyEntry: forEachCurlyEntry,
                  portray_dict: function(options, precedence)
                  {
                      var output = "_{";
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
                      if (e instanceof Array)
                      {
                          return {list: [{compound: {name: "=", args: [{atom: "value"}, {list: e}]}}]};
                      }
                      if (e.target.value === undefined)
                          return {list: []};
                      else
                          return {list: [{compound: {name: "=", args: [{atom: "value"}, {atom: e.target.value}]}}]};
                  }
                 };
