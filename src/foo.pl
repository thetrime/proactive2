:-module(foo,[]).


%requires('LabelledField').
requires('bar').

render(State, _Props, Form):-
        writeln(hello_from_prolog),
        writeln(State),

%        findall(Widget,
%                ( between(1, 5000, I),
%                  {|jsx(Widget)||<div>This is widget {I} of 5000</div>|}
%                ),
%                Widgets),
        {|jsx(Form)||
        <div style={width: '200px', height: '200px', background: 'red'}>
          This is a test: {State.counter}
          <bar cat="zin" cog={Unbound} islay="chonk"/>
          <Button onClick={someEvent}>Click me!</Button>

        </div>|}.


getInitialState(_, {counter: 0}).

someEvent(_Event, State, _Props, {counter: NewCounter}):-
        NewCounter is State.counter + 1.

