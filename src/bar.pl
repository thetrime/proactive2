:-module(bar,[]).

render(State, Props, Form):-
        writeln(hello_from_bar:Props),
        {|jsx(Form)||
        <div>This is a bar</div>|}.