-module(ec_example_handler).

-export([handle/3]).

handle(_Method, _Path, _Req) ->
    {200, [{<<"content-type">>, <<"text/html">>}], <<"Hello There!">>}.
