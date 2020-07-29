## Props and State

There are several cases for how props and state are represented internally. This document tries to explain the rationale

### JSValue and PrologValue Objects
There are two functions in prolog_utilities for converting Prolog into stable (that is, permanent) Javascript representations.
The reason there are 2 kinds of objects is that when we pass props to a non-Proactive component, it must be able to understand it.

This means that for Bootstrap and DOM objects:
   * Props are always a Javascript object
   * Each Name=Value pair in the underlying vDOM object is converted to a Name: JSValue entry in the object
   * Each Value is one of:
      * A function, if Name begins with 'on'
      * An object, if Value is a dict
        * The object is encoded in the same way, except that we do not translate onXXX values to a function
      * A string, for all other cases

For Proactive objects, we want to be able to pass in arbitrary Prolog objects and have them turn up as actual Prolog objects. So, in this case:
   * Props are still a Javascript object
   * Each Name=Value pair in the underlying vDOM object is converted to a Name: PrologValue entry in the object
   * A Value is an object with one of these properties:
      * atom: <text value> - for atoms
      * list: <list of PrologValue> - for lists
      * integer: <integer value> - for integers
      * variable: <not defined> - for variables
      * compound: {name: <functor name>, args: <list of PrologValue>} - for terms
      * widget: <ref> - for blobs of type Widget
      * dict: <PrologValue>

The advantage of a PrologValue is that it can be converted back into Prolog deterministically without any information loss.

State is represented in the same way. This is to simplify handling of portraying the term - we portray props by iterating through all the keys and writing key:jsToProlog(value).
If State were stored as a single PrologValue then we would need to handle Props differently to State.