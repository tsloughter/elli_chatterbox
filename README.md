elli_chatterbox
=====

Elli for http1 and chatterbox for http2.

Build
-----

    $ rebar3 compile

Run
----

```erlang
$ rebar3 shell
1> application:ensure_all_started(elli_chatterbox).
{ok,[elli,syntax_tools,compiler,goldrush,lager,hpack,
     chatterbox,elli_chatterbox]}
2> elli_chatterbox:start_link([]).
```

Open [https://localhost:8080/](https://localhost:8080/).
