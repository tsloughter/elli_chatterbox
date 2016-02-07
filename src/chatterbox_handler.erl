-module(chatterbox_handler).

-include_lib("chatterbox/include/http2.hrl").

-export([spawn_handle/4,
         handle/4]).

spawn_handle(Pid, StreamId, Headers, ReqBody) ->
    Handler = fun() ->
        handle(Pid, StreamId, Headers, ReqBody)
    end,
    spawn_link(Handler).

handle(ConnPid, StreamId, Headers, ReqBody) ->
    {ok, Handler} = application:get_env(elli_chatterbox, handler),
    {ok, Status, RespHeaders, RespBody} = Handler:handle(Headers, ReqBody),
    ResponseHeaders = [{<<":status">>, integer_to_binary(Status)} | RespHeaders],
    http2_connection:send_headers(ConnPid, StreamId, ResponseHeaders),
    http2_connection:send_body(ConnPid, StreamId, RespBody).
