:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

:-http_handler(root('proactive/'), serve_form, [prefix]).
:-http_handler(root('assets/'), http_reply_from_files(assets, []), [prefix]).

main:-
        http_server(http_dispatch, [port(8880)]).

serve_form(Request):-
        memberchk(path_info(FormId), Request),
        subtract(Request, [path(_)], R1),
        parse_url(URL, [path('/react')|R1]),
        format(atom(Bootstrap), 'Proactive.render("~w", "~w", document.getElementById("container"))', [URL, FormId]),

        HTML = element(html, [], [element(head, [], [element(script, [src='https://unpkg.com/react/umd/react.production.min.js', crossorigin=anonymous], []),
                                                     element(script, [src='https://unpkg.com/react-dom/umd/react-dom.production.min.js', crossorigin=anonymous], []),
                                                     element(script, [src='/assets/proactive.js'], []),
                                                     element(link, [rel=stylesheet,
                                                                    href='https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css',
                                                                    integrity='sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh',
                                                                    crossorigin=anonymous], [])]),
                                  element(body, [], [element(div, [id=container], []),
                                                     element(script, [src='https://unpkg.com/react-bootstrap@next/dist/react-bootstrap.min.js'], []),
                                                     element(script, [type='text/javascript'], [Bootstrap])])]),
        format(current_output, 'Content-type: text/html~n~n', []),
        html_write(current_output, HTML, []).