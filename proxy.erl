%%% 支持Socks5协议的高速加密通信的代理服务器脚本
-module(proxy).
-export([
    start/0
]).

-export([
    back_start/0, back_start/1,
    back_accept/1,
    back_process/1,

    front_start/1,
    front_accept/3,
    front_process/3,
    forward/3,

    flip/1,
    flip_recv/2, flip_recv/3,
    flip_send/2,

    heart/0
]).

-define(FRONT_PORT, '8780').
-define(BACK_PORT, '8781').
-define(CONNECT_TIMEOUT, 5000).

-define(PASSWORD, "abcd1234").
-define(PASSWORD_LENGTH, length(?PASSWORD)).

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

forward(Client, Remote, From) ->
    try
        {ok, Packet} = gen_tcp:recv(Client, 0),
        ok = flip_send(Remote, Packet)
    catch
        Error:Reason ->
            From ! {close},
            exit({Error, Reason})
    end,
    forward(Client, Remote, From).

heart() ->
    timer:sleep(10000),
    io:format(".~n"),
    heart().

back_start() ->
    back_start(['0.0.0.0', ?BACK_PORT]).

back_start([Port]) ->
    back_start(['0.0.0.0', Port]);
back_start(Args) ->
    % prevent disconnect when run in ssh
    spawn(?MODULE, heart, []),
    [BackAddressStr, BackPortStr] = atoms_to_lists(Args, []),
    BackPort = list_to_integer(BackPortStr),
    {ok, BackAddress} = inet:getaddr(BackAddressStr, inet),
    io:format("back listen at ~s:~p.~n", [BackAddressStr, BackPort]),
    {ok, Socket} = gen_tcp:listen(BackPort, [{reuseaddr, true},
                                             {active, false},
                                             {ifaddr, BackAddress},
                                             binary]),
    back_accept(Socket).


back_accept(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    spawn(?MODULE, back_process, [Client]),
    back_accept(Socket).

back_process(Front) ->
    try
        From = self(),

        {ok, <<?PASSWORD>>} = flip_recv(Front, ?PASSWORD_LENGTH, ?CONNECT_TIMEOUT),

        {ok, Remote} = socks5_handshake(Front),
        spawn(?MODULE, forward, [Front, Remote, From]),
        spawn(?MODULE, forward, [Remote, Front, From]),

        receive
            {close} ->
                gen_tcp:close(Front),
                gen_tcp:close(Remote)
        end
    catch
        Error:Reason ->
            io:format("~p ~p ~p ~p.~n", [Front, Error, Reason, erlang:get_stacktrace()]),
            gen_tcp:close(Front)
    end.

atoms_to_lists(L, R) ->
    lists:reverse(atoms_to_lists2(L, R)).

atoms_to_lists2([X|L], R) ->
    atoms_to_lists2(L, [atom_to_list(X)|R]);
atoms_to_lists2([], R) ->
    R.

front_start([BackAddress]) ->
    front_start([BackAddress, '8781']);
front_start([BackAddress, BackPort]) ->
    front_start([BackAddress, BackPort, '127.0.0.1', '8780']);
front_start(Args) ->
    [BackAddressStr, BackPortStr, FrontAddressStr, FrontPortStr] = atoms_to_lists(Args, []),
    FrontPort = list_to_integer(FrontPortStr),
    BackPort  = list_to_integer(BackPortStr),
    {ok, FrontAddress} = inet:getaddr(FrontAddressStr, inet),
    {ok, BackAddress}  = inet:getaddr(BackAddressStr, inet),
    io:format("front listen at ~s:~p.~n", [FrontAddressStr, FrontPort]),
    io:format("back address is ~s:~p.~n", [BackAddressStr, BackPort]),
    {ok, Socket} = gen_tcp:listen(FrontPort, [{reuseaddr, true},
                                              {active, false},
                                              {ifaddr, FrontAddress},
                                              binary]),
    front_accept(Socket, BackAddress, BackPort).

front_accept(Socket, BackAddress, BackPort) ->
    {ok, Client} = gen_tcp:accept(Socket),
    spawn(?MODULE, front_process, [Client, BackAddress, BackPort]),
    front_accept(Socket, BackAddress, BackPort).

front_process(Client, BackAddress, BackPort) ->
    try
        From = self(),

        {ok, Back} = gen_tcp:connect(BackAddress,
                                     BackPort,
                                     [{active, false}, binary],
                                     ?CONNECT_TIMEOUT),
        ok = flip_send(Back, <<?PASSWORD>>),

        spawn(?MODULE, forward, [Client, Back, From]),
        spawn(?MODULE, forward, [Back, Client, From]),

        receive
            {close} ->
                gen_tcp:close(Client),
                gen_tcp:close(Back)
        end
    catch
        Error:Reason ->
            io:format("~p ~p ~p ~p.~n", [Client, Error, Reason, erlang:get_stacktrace()]),
            gen_tcp:close(Client)
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
    {ok, Remote} = gen_tcp:connect(Address,
                                   Port,
                                   [{active, false}, binary],
                                   ?CONNECT_TIMEOUT),
    ok = flip_send(Client, <<5, 0, 0, 1, 0:32, 0:16 >>),
    {ok, Remote}.

start() ->
    spawn(?MODULE, back_start, [['0.0.0.0', '8781']]),
    spawn(?MODULE, front_start, [['127.0.0.1', '8781', '127.0.0.1', '8780']]),
    receive
        {close} ->
            exit({done})
    end.
