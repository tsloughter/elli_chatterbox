-module(elli_handler).

-export([handle/2,
         handle_event/3]).

-include_lib("elli/include/elli.hrl").

handle(Req, _Args) ->
    {ok, Handler} = application:get_env(elli_chatterbox, handler),
    Headers = elli_request:headers(Req),
    ReqBody = elli_request:body(Req),
    {ok, Status, RespHeaders, RespBody} = Handler:handle(Headers, ReqBody),
    {Status, RespHeaders, RespBody}.

handle_event(_, _, _) -> ok.
