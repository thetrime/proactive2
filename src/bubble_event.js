/* bubble_event(+Handler, +Event)
   A common pattern in React is to to do something like <Child onFoo={this.handleFoo.bind(this)}/>
   Then in Child have <Button onClick={this.handleClick}/>
   and implement this as
   function handleClick(e)
   {
     ... do some stuff ..
     this.props.onFoo();
   }

   We cannot do this directly in Proactive, because by the time we are calling handleClick(e) we are in Prolog, and we cannot just call the javascript function handleFoo
   with the context set to the parent.

   Note that We CAN get this result if we simply have child as <Button onClick={Props.onFoo}/> because then the handler will be attached to javascript directly, and we do not
   end up in Prolog until we are running handleFoo()

   Sometimes, though, it is very handy to be able to call these callbacks from Prolog. What does this look like? Well, we have to call some foreign function that breaks
   out of Prolog to Javascript, and calls the function after setting up the 'this' pointer correctly. Fortunately, the handler will always be of the form
      '$this'(ReferenceToParent, Term)
   so this is not that hard to achieve

   The implementation is a bit (very) crazy because of the single-threaded nature of Javascript.

*/

var Prolog = require('proscript2');
var Constants = require('./constants');
var PrologUtilities = require('./prolog_utilities');

module.exports = function(Handler, Event)
{
    var checkpoint = Prolog.save_state();

    var target = Prolog.get_blob("widget", this._this);
    while (Prolog.is_compound(Handler) && Prolog.term_functor(Handler) == Constants.thisFunctor)
    {
        var Blob = Prolog.term_arg(Handler, 0);
        target = Prolog.get_blob("widget", Blob);
        Handler = Prolog.term_arg(Handler, 1);
    }

    var resume = Prolog._yield();
    var NewState = Prolog.make_variable();
    target.callAsynchronously(target.module, Handler, [Event, target.state, target.props, NewState], function(rc)
                              {
                                  if (rc)
                                      target.setState(PrologUtilities.prologToJS(Prolog.deref(NewState)));
                                  Prolog.restore_state(checkpoint);
                                  resume(rc);
                              });
    return 3; // YIELD
}
