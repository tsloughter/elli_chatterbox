-module(chatterbox_handler).

-include_lib("elli_chatterbox/include/elli_chatterbox.hrl").
-include_lib("chatterbox/include/http2.hrl").

-export([spawn_handle/4,
         handle/4]).

spawn_handle(Pid, StreamId, Headers, ReqBody) ->
    Handler = fun() ->
        handle(Pid, StreamId, Headers, ReqBody)
    end,
    spawn_link(Handler).

handle(ConnPid, StreamId, Headers, ReqBody) ->
    Path = proplists:get_value(<<":path">>, Headers),
    Method = proplists:get_value(<<":method">>, Headers),
    {ok, {RawPath, URL, URLArgs}} = elli_http:parse_path({abs_path, Path}),
    Req = #ec_req{method = Method,
                  path = URL,
                  args = URLArgs,
                  raw_path = RawPath,
                  version = {2,0},
                  headers = Headers,
                  body = ReqBody,
                  stream_id = StreamId,
                  conn_pid = ConnPid},
    {ok, Handler} = application:get_env(elli_chatterbox, handler),
    {Status, RespHeaders, RespBody} = Handler:handle(Method, URL, Req),
    ResponseHeaders = [{<<":status">>, integer_to_binary(Status)} | RespHeaders],
    http2_connection:send_headers(ConnPid, StreamId, ResponseHeaders),
    http2_connection:send_body(ConnPid, StreamId, RespBody).
