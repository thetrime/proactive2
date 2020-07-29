:-module(bar,[]).

render(_State, Props, Form):-
        writeln(hello_from_bar:Props),
        {|jsx(Form)||
        <div>This is a foo bar<Button onClick={clickHandler}>Bar</Button>
%          <MessageListener class="other_class" discriminator="other_discriminator"/>
        </div>|}.

splunge(_Event, State, Props, _{}):-
        writeln(bar(State, Props)).


clickHandler(Event, _State, Props, _{}):-
        bubble_event(Props.test, [extra=zin|Event]).

other_discriminator(_).

getInitialState(_, _{}):-
        on_server(??sleep(2)).