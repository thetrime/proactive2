:-module(foo,[]).


%requires('LabelledField').
%requires('Title').

render(State, _Props, Form):-
        writeln(hello_from_prolog),
        {|jsx(Form)||
        <div style={width: '200px', height: '200px', background: 'red'}>
          This is a test: {State.counter}
          <button onClick={someEvent}>Click me!</button>
        </div>|}.


getInitialState(_, {counter: 0}).

someEvent(_Event, State, _Props, {counter: NewCounter}):-
        writeln(got_event),
        NewCounter is State.counter + 1.

