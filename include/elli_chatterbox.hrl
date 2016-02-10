-type method() :: 'OPTIONS' | 'GET' | 'HEAD' | 'POST' |
                  'PUT' | 'DELETE' | 'TRACE' | binary().

-type vsn() :: {0,9} | {1,0} | {1,1} | {2,0}.

-record(ec_req, {method    :: method(),
                 path      :: [binary()],
                 args      :: [{binary(), any()}],
                 raw_path  :: binary(),
                 version   :: vsn(),
                 headers   :: [{binary(), binary() | string()}],
                 body      :: binary() | iolist(),
                 pid       :: pid(),
                 stream_id :: integer(),
                 conn_pid  :: pid(),
                 socket    :: undefined | inet:socket() | ssl:sslsocket()}).
