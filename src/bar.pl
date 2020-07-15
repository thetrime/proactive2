:-module(bar,[]).

render(_State, Props, Form):-
        writeln(hello_from_bar:Props),
        {|jsx(Form)||
        <div>This is a bar<Button onClick={Props.test}>Bar</Button></div>|}.

splunge(_Event, State, Props, {}):-
        writeln(bar(State, Props)).
