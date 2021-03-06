:-module(foo,[]).


%requires('LabelledField').
requires('bar').

render(State, _Props, Form):-
        writeln(hello_from_foo(State)),
        Max = 5,
        findall(Widget,
                ( between(1, Max, I),
                  {|jsx(Widget)||<span>This is widget {I} of {Max}</span>|}
                ),
                Widgets),
        {|jsx(Form)||
        <div style={_{width: '200px', height: '200px', background: 'red'}}>
          This is a test: {State.counter}
          {Widgets}
          <bar cat="zin" cog={Unbound} islay="chonk" test={splunge}/>
          <bar dinner="biscuits" test={this.splunge}/>
          <input value={State.input_data} onChange={this.ftang}/>
          <MessageListener class="test_message" discriminator={my_discriminator} onMessage={this.onMessage}/>
          <Button onClick={someEvent}>Click me!</Button>
        </div>|},
        writeln(input_data=State.input_data),
        writeln(dict=State.a_dict),
        writeln(tricky=State.a_dict.van).


getInitialState(_, _{counter: X,
                     input_data: 'cat',
                     a_dict: _{van: blue,
                               tree: large}}):-
        on_server(get_counter(X)).

get_counter(3).

someEvent(_Event, State, _Props, _{counter: NewCounter}):-
        findall(X,
                on_server(??member(X, [a,b,c,State])),
                Xs),
        writeln(Xs),
        NewCounter is State.counter + 1.


splunge(_Event, _State, _Props, _{islay: squashed}):-
        on_server(writeln(foo)).

ftang(Event, _State, _Props, _{input_data: Value}):-
        writeln(got_event(Event)),
        memberchk(value=Value, Event).


onMessage(Event, _State, _Props, _{input_data: Data}):-
        writeln(message_received(Event)),
        ??memberchk(baz=Data, Event).

my_discriminator(Message):-
        on_server(??memberchk(foo=bar, Message)).