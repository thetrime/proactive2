:-module(bar,[]).

render(_, _, Form):-
        {|jsx(Form)||
        <div>This is a bar</div>|}.