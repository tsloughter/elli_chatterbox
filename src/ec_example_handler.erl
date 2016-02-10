-module(ec_example_handler).

-export([handle/3]).

handle(_Method, _Path, _Req) ->
    {ok, 200, [{<<"content-type">>, <<"text/html">>}], <<"Hello There!">>}.
