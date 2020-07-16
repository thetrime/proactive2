:-module(bar,[]).

render(_State, Props, Form):-
        writeln(hello_from_bar:Props),
        {|jsx(Form)||
        <div>This is a bar<Button onClick={clickHandler}>Bar</Button></div>|}.

splunge(_Event, State, Props, {}):-
        writeln(bar(State, Props)).


clickHandler(Event, _State, Props, {}):-
        bubble_event(Props.test, [extra=zin|Event]).