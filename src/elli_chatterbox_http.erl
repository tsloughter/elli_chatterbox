-module(elli_chatterbox_http).

-export([start_link/6,
         accept/6]).

-include_lib("chatterbox/include/http2_socket.hrl").

start_link(Server, Version, Transport, ListenSocket, Options, Callback) ->
    proc_lib:start_link(?MODULE, accept, [Server, Version, Transport, ListenSocket, Options, Callback]).

%% With ssl we can negotiate either http1 or http2
%% Without ssl we must listen for either http1 or http2 only, the functions accept_http1 and accept_http2
accept(Server, Version, Transport, ListenSocket, Options, Callback) ->
    ok = proc_lib:init_ack({ok, self()}),
    case socket_accept(Transport, ListenSocket, Server, accept_timeout(Options)) of
        {ok, Socket} when Transport =:= ssl ->
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
        {ok, Socket} when Version =:= http1 ->
            elli_http:keepalive_loop({plain, Socket}, Options, Callback);
        {ok, Socket} when Version =:= http2 ->
            {ok, ServerPid} = http2_connection:start_link(self(), server),
            gen_server:enter_loop(http2_socket,
                                  [],
                                  #http2_socket_state{
                                    type = server,
                                    http2_pid=ServerPid,
                                    socket={gen_tcp, Socket}
                                   });
        {error, timeout} ->
            ?MODULE:accept(Server, Version, Transport, ListenSocket, Options, Callback);
        {error, econnaborted} ->
            ?MODULE:accept(Server, Version, Transport, ListenSocket, Options, Callback);
        {error, {tls_alert, _}} ->
            ?MODULE:accept(Server,Version, Transport,  ListenSocket, Options, Callback);
        {error, closed} ->
            ok;
        {error, Other} ->
            exit({error, Other})
    end.

socket_accept(ssl, ListenSocket, Server, Timeout) ->
    case ssl:transport_accept(ListenSocket, Timeout) of
        {ok, Socket} ->
            gen_server:cast(Server, accepted),
            ok = ssl:ssl_accept(Socket),
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end;
socket_accept(gen_tcp, ListenSocket, Server, Timeout) ->
    case gen_tcp:accept(ListenSocket, Timeout) of
        {ok, Socket} ->
            gen_server:cast(Server, accepted),
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

accept_timeout(Opts) ->
    proplists:get_value(accept_timeout, Opts).
