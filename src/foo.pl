:-module(foo,[]).


%requires('LabelledField').
requires('bar').

render(State, _Props, Form):-
        writeln(hello_from_foo(State)),
%        findall(Widget,
%                ( between(1, 5000, I),
%                  {|jsx(Widget)||<div>This is widget {I} of 5000</div>|}
%                ),
%                Widgets),
        {|jsx(Form)||
        <div style={width: '200px', height: '200px', background: 'red'}>
          This is a test: {State.counter}
          <bar cat="zin" cog={Unbound} islay="chonk" test={splunge}/>
          <bar dinner="biscuits" test={this.splunge}/>
          <input value={State.input_data} onChange={this.ftang}/>
          <Button onClick={someEvent}>Click me!</Button>
        </div>|},
        writeln(state=State.input_data).


getInitialState(_, {counter: 0,
                    input_data: 'cat'}).

someEvent(_Event, State, _Props, {counter: NewCounter}):-
        findall(X,
                on_server(??member(X, [a,b,c,State])),
                Xs),
        writeln(Xs),
        NewCounter is State.counter + 1.


splunge(_Event, _State, _Props, {islay: squashed}):-
        on_server(writeln(foo)).

ftang(Event, _State, _Props, {input_data: Value}):-
        writeln(got_event(Event)),
        memberchk(value=Value, Event).

