-module(elli_chatterbox).
-behaviour(gen_server).
-include_lib("elli/include/elli.hrl").

%% API
-export([start_link/0,
         start_link/1,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type req() :: #req{}.
-export_type([req/0, body/0, headers/0]).

-record(state, {socket :: elli_tcp:socket(),
                acceptors :: non_neg_integer(),
                open_reqs :: non_neg_integer(),
                options :: [{_, _}],
                transport :: ssl | gen_tcp,
                version :: http1 | http2,
                callback :: callback()}).

start_link() ->
    CertFile = application:set_env(elli_chatterbox, certfile, filename:join(code:priv_dir(elli_chatterbox), "localhost.crt")),
    KeyFile = application:set_env(elli_chatterbox, keyfile, filename:join(code:priv_dir(elli_chatterbox), "localhost.key")),
    start_link([{ssl, true},
                {certfile, CertFile},
                {keyfile, KeyFile}]).

start_link(Opts) ->
    application:set_env(chatterbox, content_handler, chatterbox_handler),
    case proplists:get_value(name, Opts) of
        undefined ->
            gen_server:start_link(?MODULE, [Opts], []);
        Name ->
            gen_server:start_link(Name, ?MODULE, [Opts], [])
    end.

stop(S) ->
    gen_server:call(S, stop).


init([Opts]) ->
    %% Use the exit signal from the acceptor processes to know when
    %% they exit
    process_flag(trap_exit, true),

    Handler = proplists:get_value(handler, Opts, ec_example_handler),

    Callback       = elli_callback,
    CallbackArgs   = [Handler],
    IPAddress      = proplists:get_value(ip, Opts, {0,0,0,0}),
    Port           = proplists:get_value(port, Opts, 8080),
    MinAcceptors   = proplists:get_value(min_acceptors, Opts, 100),

    UseSSL         = proplists:get_value(ssl, Opts, false),
    KeyFile        = proplists:get_value(keyfile, Opts),
    CertFile       = proplists:get_value(certfile, Opts),
    Transport      = case UseSSL of true -> ssl; false -> gen_tcp end,
    {SockOpts, Version} = case UseSSL of
                               true ->
                                   {[{keyfile, KeyFile},
                                    {certfile, CertFile},
                                    {honor_cipher_order, false},
                                    {versions, ['tlsv1.2']},
                                    {next_protocols_advertised, [<<"h2">>]}], all};
                               false ->
                                  {[], proplists:get_value(default_http_version, Opts, http1)}
                           end,

    AcceptTimeout  = proplists:get_value(accept_timeout, Opts, 10000),
    RequestTimeout = proplists:get_value(request_timeout, Opts, 60000),
    HeaderTimeout  = proplists:get_value(header_timeout, Opts, 10000),
    BodyTimeout    = proplists:get_value(body_timeout, Opts, 30000),
    MaxBodySize    = proplists:get_value(max_body_size, Opts, 1024000),

    Options = [{accept_timeout, AcceptTimeout},
               {request_timeout, RequestTimeout},
               {header_timeout, HeaderTimeout},
               {body_timeout, BodyTimeout},
               {max_body_size, MaxBodySize}],

    {ok, Socket} = Transport:listen(Port, [binary,
                                           {ip, IPAddress},
                                           {reuseaddr, true},
                                           {backlog, 32768},
                                           {packet, raw},
                                           {active, false}
                                           | SockOpts
                                          ]),

    Acceptors = ets:new(acceptors, [private, set]),
    StartAcc  = fun() ->
                    Pid = elli_chatterbox_http:start_link(self(), Version, Transport, Socket, Options, {Callback, CallbackArgs}),
                    ets:insert(Acceptors, {Pid})
                end,
    [StartAcc() || _ <- lists:seq(1, MinAcceptors)],

    {ok, #state{socket = Socket,
                acceptors = Acceptors,
                open_reqs = 0,
                options = Options,
                transport = Transport,
                version = Version,
                callback = {Callback, CallbackArgs}}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(accepted, State) ->
    {noreply, start_add_acceptor(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ack, _Pid, {ok, _Pid}}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, {error, emfile}}, State) ->
    error_logger:error_msg("No more file descriptors, shutting down~n"),
    {stop, emfile, State};
handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, remove_acceptor(State, Pid)};
handle_info({'EXIT', Pid, Reason}, State) ->
    %% Does this matter since it could be just closing the http2 connection?
    error_logger:error_msg("ElliChatterbox request (pid ~p) unexpectedly " "crashed:~n~p~n", [Pid, Reason]),
    {noreply, remove_acceptor(State, Pid)}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% What if it was killed before sending the 'accepted' message?
remove_acceptor(State, Pid) ->
    ets:delete(State#state.acceptors, Pid),
    State#state{open_reqs = State#state.open_reqs - 1}.

start_add_acceptor(State) ->
    Pid = elli_chatterbox_http:start_link(self(), State#state.version, State#state.transport, State#state.socket,
                                          State#state.options, State#state.callback),
    ets:insert(State#state.acceptors, {Pid}),
    State#state{open_reqs = State#state.open_reqs + 1}.
