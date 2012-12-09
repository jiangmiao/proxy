%%% 支持Socks5协议的高速加密通信的代理服务器脚本
-module(proxy).
-export([
    start/0,
    back_start/0, back_start/1,
    front_start/1
]).

-export([
    back_process/1,
    front_process/3,
    forward/3,

    heart/0
]).

-export([
    back_pool_fill_loop/2,
    back_pool_process/1,
    back_pool_waiting/2
]).

-define(FRONT_PORT, '8780').
-define(BACK_PORT, '8781').
-define(CONNECT_TIMEOUT, 5000).
-define(POOL_SIZE, 10).

-define(PASSWORD, "abcd1234").
-define(PASSWORD_LENGTH, length(?PASSWORD)).

-ifdef(debug).
-define(LOG(Format, Values), io:format(Format, Values)).
-else.
-define(LOG(Format, Values), true).
-endif.

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

        {ok, Remote} = back_socks5_handshake(Front),

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

back_pool_new(BackAddress, BackPort) ->
    Pid = spawn(?MODULE, back_pool_process, [queue:new()]),
    register(back_pool, Pid),
    spawn(?MODULE, back_pool_fill_loop, [BackAddress, BackPort]).

%% 不断的向 back_pool 放入 Back 相关的 Pid 直到 POOL_SIZE 个
back_pool_fill_loop(BackAddress, BackPort) ->
    try
        back_pool ! {self(), len},
        receive
            {ok, Length} ->
                if
                    Length < ?POOL_SIZE ->
                        {ok, Back} = front_connect_to_back(BackAddress, BackPort),
                        Pid = spawn(?MODULE, back_pool_waiting, [Back, self()]),
                        gen_tcp:controlling_process(Back, Pid),
                        inet:setopts(Back, [{active, once}]),
                        back_pool ! {self(), in, Pid},
                        receive _ -> ok end;
                    true ->
                        timer:sleep(200)
                end;
            Other ->
                throw(Other)
        end
    catch
        _:Reason ->
            io:format("connect ~p:~p failed (~p). wait 2 seconds.~n", [BackAddress, BackPort, Reason]),
            timer:sleep(2000)
    end,
    back_pool_fill_loop(BackAddress, BackPort).

%% 当 Back 收到 tcp_closed 则自动变成无效
back_pool_waiting(Back, OldPid) ->
    receive
        {tcp_closed, _} ->
            back_pool_waiting(empty, OldPid);
        {Process, FrontPid, get} ->
            if
                Back == empty ->
                    Process ! {FrontPid, get, {empty}};
                true ->
                    inet:setopts(Back, [{active, false}]),
                    gen_tcp:controlling_process(Back, OldPid),
                    Process ! {FrontPid, get, {ok, Back}}
            end;
        Other ->
            io:format("unexpected message receive ~p.~n", [Other]),
            gen_tcp:close(Back),
            back_pool_waiting(empty, OldPid)
    end.

back_pool_process(Queue) ->
    receive
        {FrontPid, out} ->
            case queue:out(Queue) of
                {{value, Pid}, Queue2} ->
                    ?LOG("out pool size: ~p.~n", [queue:len(Queue2)]),
                    Pid ! {self(), FrontPid, get},
                    back_pool_process(Queue2);
                {empty, _} ->
                    FrontPid ! {empty},
                    back_pool_process(Queue)
            end;
        {FrontPid, in, Back} ->
            Queue2 = queue:in(Back, Queue),
            ?LOG("in pool size: ~p.~n", [queue:len(Queue2)]),
            FrontPid ! {ok},
            back_pool_process(Queue2);
        {FrontPid, len} ->
            FrontPid ! {ok, queue:len(Queue)},
            back_pool_process(Queue);
        {FrontPid, get, {ok, Back}} ->
            FrontPid ! {ok, Back},
            back_pool_process(Queue);
        {FrontPid, get, {empty}} ->
            self() ! {FrontPid, out},
            back_pool_process(Queue)
    end.

front_get_back(BackAddress, BackPort) ->
    back_pool ! {self(), out},
    receive
        {ok, Back} ->
            {ok, Back};
        {empty} ->
            front_connect_to_back(BackAddress, BackPort)
    end.

front_connect_to_back(BackAddress, BackPort) ->
    ?LOG("connect to back.~n", []),
    {ok, Back} = gen_tcp:connect(BackAddress,
                                 BackPort,
                                 [{active, false}, binary],
                                 ?CONNECT_TIMEOUT),
    ok = flip_send(Back, <<?PASSWORD>>),
    {ok, Back}.

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
    back_pool_new(BackAddress, BackPort),
    front_accept(Socket, BackAddress, BackPort).

front_accept(Socket, BackAddress, BackPort) ->
    {ok, Client} = gen_tcp:accept(Socket),
    spawn(?MODULE, front_process, [Client, BackAddress, BackPort]),
    front_accept(Socket, BackAddress, BackPort).

front_process(Client, BackAddress, BackPort) ->
    try
        From = self(),

        {ok, Back} = front_get_back(BackAddress, BackPort),
        {ok, Endpoint} = front_socks5_handshake(Client),
        flip_send(Back, Endpoint),

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

back_socks5_handshake(Client) ->
    {ok, <<AType:8>>} = flip_recv(Client, 1),
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
    {ok, Remote}.

%% 前端完成 socks5 协议的握手，始终立刻返回“成功”。避免二次请求节约时间。
front_socks5_handshake(Client) ->
    {ok, <<?VERSION:8, Nmethods:8>>} = gen_tcp:recv(Client, 2),
    {ok, _Methods} = gen_tcp:recv(Client, Nmethods),
    ok = gen_tcp:send(Client, <<5, 0>>),

    {ok, <<?VERSION:8, ?CONNECT:8, _Rsv:8, AType:8>>} = gen_tcp:recv(Client, 4),
    case AType of
        ?IPV4 ->
            {ok, Address} = gen_tcp:recv(Client, 4),
            {ok, Port} = gen_tcp:recv(Client, 2),
            Endpoint = <<AType:8, Address/binary, Port/binary>>;
        ?DOMAIN ->
            {ok, <<DomainLen:8>>} = gen_tcp:recv(Client, 1),
            {ok, DomainBin} = gen_tcp:recv(Client, DomainLen),
            {ok, Port} = gen_tcp:recv(Client, 2),
            Endpoint = <<AType:8, DomainLen:8, DomainBin/binary, Port/binary>>
    end,
    ok = gen_tcp:send(Client, <<5, 0, 0, 1, 0:32, 0:16 >>),
    {ok, Endpoint}.

start() ->
    spawn(?MODULE, back_start, [['0.0.0.0', '8781']]),
    spawn(?MODULE, front_start, [['127.0.0.1', '8781', '127.0.0.1', '8780']]),
    receive
        {close} ->
            exit({done})
    end.
