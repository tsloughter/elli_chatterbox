-module(chatterbox_handler).

-behaviour(http2_stream).

-export([
         init/2,
         on_receive_request_headers/2,
         on_send_push_promise/2,
         on_receive_request_data/2,
         on_request_end_stream/1
        ]).

-include_lib("elli_chatterbox/include/elli_chatterbox.hrl").
-include_lib("chatterbox/include/http2.hrl").

-record(cb_static, {
          req_headers=[],
          body = <<>>,
          connection_pid :: pid(),
          stream_id :: stream_id()
         }).

init(ConnPid, StreamId) ->
    %% You need to pull settings here from application:env or something
    {ok, #cb_static{connection_pid=ConnPid,
                    stream_id=StreamId}}.

on_receive_request_headers(Headers, State) ->
    {ok, State#cb_static{req_headers=Headers}}.

on_send_push_promise(Headers, State) ->
    {ok, State#cb_static{req_headers=Headers}}.

on_receive_request_data(Bin, State)->
    {ok, State#cb_static{body=Bin}}.

on_request_end_stream(State=#cb_static{connection_pid=ConnPid,
                                       stream_id=StreamId}) ->
    Headers = State#cb_static.req_headers,
    ReqBody = State#cb_static.body,
    Method = proplists:get_value(<<":method">>, Headers),
    Path = proplists:get_value(<<":path">>, Headers),
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
    http2_connection:send_body(ConnPid, StreamId, RespBody),

    {ok, State}.
