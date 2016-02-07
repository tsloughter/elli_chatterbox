-module(elli_chatterbox_http).

-export([start_link/4]).
-export([accept/4]).

-include_lib("chatterbox/include/http2_socket.hrl").

start_link(Server, {ssl, ListenSocket}, Options, Callback) ->
    proc_lib:start_link(?MODULE, accept, [Server, ListenSocket, Options, Callback]).

accept(Server, ListenSocket, Options, Callback) ->
    ok = proc_lib:init_ack({ok, self()}),
    case ssl:transport_accept(ListenSocket, accept_timeout(Options)) of
        {ok, Socket} ->
            gen_server:cast(Server, accepted),
            ok = ssl:ssl_accept(Socket),
            case ssl:negotiated_protocol(Socket) of
                {ok, <<"h2">>} ->
                    ok = ssl:setopts(Socket, [{active, once}]),
                    {ok, ServerPid} = http2_connection:start_link(self(), server),
                    gen_server:enter_loop(http2_socket,
                                          [],
                                          #http2_socket_state{
                                            type = server,
                                            http2_pid=ServerPid,
                                            socket={ssl, Socket}
                                           });
                _ ->
                    elli_http:keepalive_loop({ssl, Socket}, Options, Callback)
            end;
        {error, timeout} ->
            ?MODULE:accept(Server, ListenSocket, Options, Callback);
        {error, econnaborted} ->
            ?MODULE:accept(Server, ListenSocket, Options, Callback);
        {error, {tls_alert, _}} ->
            ?MODULE:accept(Server, ListenSocket, Options, Callback);
        {error, closed} ->
            ok;
        {error, Other} ->
            exit({error, Other})
    end.

accept_timeout(Opts) ->
    proplists:get_value(accept_timeout, Opts).
