-module(ec_example_handler).

-export([handle/2]).

handle(_Headers, _ReqBody) ->
    {ok, 200, [{<<"content-type">>, <<"text/html">>}], <<"Hello There!">>}.
