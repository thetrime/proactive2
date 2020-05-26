Proactive = {render: function(url, module, container)
             {
                 // FIXME: Implement this
                 //   Prolog.load_module(url);
                 class ProactiveForm extends React.Component
                 {
                     constructor(props)
                     {
                         super(props);
                         // this.prolog_props = <<< Make a Prolog equivalent of props. It wont change
                         // this.state = <<< Call getInitialstate(this.prolog_props, X) and assign this.state = X
                     }


                     render()
                     {
                         // FIXME: Implement this instead
                         // var Form = new Prolog.variable;
                         // Prolog.call(module + ":render", [this.prolog_props, this.prolog_state, Form]);
                         // return toJSON(Form);


                         var ButtonGroup = ReactBootstrap.ButtonGroup,
                             Button  = ReactBootstrap.Button;

                         return (React.createElement('div', null, [
                             React.createElement(ButtonGroup, null, [
                                 React.createElement(Button, null, `Left`),
                                 React.createElement(Button, null, `Middle`),
                                 React.createElement(Button, null, `Right`)])]));
                     }
                 };
                 ReactDOM.render(React.createElement(ProactiveForm, null, []), container);
             }}
