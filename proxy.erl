%% 简单高速加密的 socks5 代理服务器
%%
%% Client - 客户端：如浏览器
%% Front  - 代理前端：从浏览器等客户端接收数据，简单加密后转发给后端
%% Back   - 代理后端：从前端接收数据，解密后等同于Socks5服务器
%% Remote - 远程服务器：需要请求的目标服务器
%%
%% Client 与 Front  为本地通信
%% Back   与 Remote 为本地通信
%% Front  与 Back   为异地加密通信
%%
%% 加密算法为 字节 异或 0x66 (01100110)

-module(proxy).
-export([
    start/0
]).

-export([
    back_start/0,
    back_accept/1,
    back_process/1,

    front_start/0,
    front_accept/1,
    front_process/1,
    forward/4,
    flip/1,
    flip_recv/2, flip_recv/3,
    flip_send/2
]).

-define(FRONT_PORT, 5050).
-define(BACK_PORT, 5051).
-define(CONNECT_TIMEOUT, 5000).

%% socks constants
-define(IPV4, 1).
-define(DOMAIN, 3).
-define(CONNECT, 1).
-define(VERSION, 5).

flip(L) ->
    flip(L, 16#66).

flip(L, V) ->
    R = flip(L, V, []),
    list_to_binary(lists:reverse(R)).

flip(<<X, L/binary>>, V, R) ->
    flip(L, V, [X bxor V|R]);

flip(<<>>, _, R) ->
    R.

flip_recv(Client, Length) ->
    flip_recv(Client, Length, infinity).

flip_recv(Client, Length, Timeout) ->
    {ok, Data} = gen_tcp:recv(Client, Length, Timeout),
    {ok, flip(Data)}.

flip_send(Client, Data) ->
    gen_tcp:send(Client, flip(Data)).

forward(flip_recv, Client, Remote, From) ->
    try
        {ok, Packet} = flip_recv(Client, 0),
        ok = gen_tcp:send(Remote, Packet)
    catch
        Error:Reason ->
            io:format("Client: ~p ~p ~p.~n", [Client, Error, Reason]),
            From ! {close},
            exit({Error, Reason})
    end,
    forward(flip_recv, Client, Remote, From);

forward(flip_send, Client, Remote, From) ->
    try
        {ok, Packet} = gen_tcp:recv(Client, 0),
        ok = flip_send(Remote, Packet)
    catch
        Error:Reason ->
            io:format("Client: ~p ~p ~p.~n", [Client, Error, Reason]),
            From ! {close},
            exit({Error, Reason})
    end,
    forward(flip_send, Client, Remote, From).

%% back，接受front的连接与认证，并接收加密的包解密后当作普通包处理。
%% back -- remote，Raw
%% remote -- back，Raw
%% back -- front，Flip
back_start() ->
    {ok, Socket} = gen_tcp:listen(?BACK_PORT, [{reuseaddr, true},
                                               {active, false},
                                               binary]),
    back_accept(Socket).

back_accept(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    spawn(?MODULE, back_process, [Client]),
    back_accept(Socket).

back_process(Front) ->
    try
        From = self(),

        {ok, <<"abcd1234">>} = flip_recv(Front, 8),
        flip_send(Front, <<"ok">>),

        {ok, Remote} = socks5_handshake(Front),
        spawn(?MODULE, forward, [flip_recv, Front, Remote, From]),
        spawn(?MODULE, forward, [flip_send, Remote, Front, From]),

        receive
            {close} ->
                gen_tcp:close(Front),
                gen_tcp:close(Remote)
        end
    catch
        _:_ ->
            gen_tcp:close(Front)
    end.


%% front，与back连接采用自定义协议，与客户端连接采用socks5
%% client -- front，socks5 接收
%% front -- back，Flip
%% back -- front，Flip
%% front -- client，Raw
front_start() ->
    {ok, Socket} = gen_tcp:listen(?FRONT_PORT, [{reuseaddr, true},
                                                {active, false},
                                                binary]),
    front_accept(Socket).

front_accept(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    spawn(?MODULE, front_process, [Client]),
    front_accept(Socket).

front_process(Client) ->
    try
        From = self(),

        {ok, Back} = gen_tcp:connect("127.0.0.1", ?BACK_PORT, [{active, false},
                                                                 binary]),
        flip_send(Back, <<"abcd1234">>),
        {ok, <<"ok">>} = flip_recv(Back, 2),

        spawn(?MODULE, forward, [flip_send, Client, Back, From]),
        spawn(?MODULE, forward, [flip_recv, Back, Client, From]),

        receive
            {close} ->
                gen_tcp:close(Client),
                gen_tcp:close(Back)
        end
    catch
        _:_ ->
            gen_tcp:close(Client)
    end.


start() ->
    spawn(?MODULE, back_start, []),
    spawn(?MODULE, front_start, []),
    receive
        {close} ->
            exit({done})
    end.

socks5_handshake(Client) ->
    {ok, <<?VERSION:8, Nmethods:8>>} = flip_recv(Client, 2),
    {ok, _Methods} = flip_recv(Client, Nmethods),
    ok = flip_send(Client, <<5, 0>>),

    {ok, <<?VERSION:8, ?CONNECT:8, _Rsv:8, AType:8>>} = flip_recv(Client, 4),
    case AType of
        ?IPV4 ->
            {ok, <<AddressBin:32, Port:16>>} = flip_recv(Client, 6),
            Address = list_to_tuple(binary_to_list(<<AddressBin:32>>)),
            io:format("~p ~p.~n", [Client, Address]);
        ?DOMAIN ->
            {ok, <<DomainLen:8>>} = flip_recv(Client, 1),
            {ok, DomainBin} = flip_recv(Client, DomainLen),
            Domain = binary_to_list(DomainBin),
            io:format("~p ~p.~n", [Client, Domain]),
            {ok, Address} = inet:getaddr(Domain, inet),
            {ok, <<Port:16>>} = flip_recv(Client, 2)
    end,
    {ok, Remote} = gen_tcp:connect(Address, Port, [{active, false}, binary], ?CONNECT_TIMEOUT),
    ok = flip_send(Client, <<5, 0, 0, 1, 0:32, 0:16 >>),
    {ok, Remote}.
