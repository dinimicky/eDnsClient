%% @author ezonghu
%% @doc @todo Add description to dnsClient.


-module(dnsClient).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("kernel/src/inet_dns.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4,
		 start/2, 
		 stop/1,
		 create_q_process/2]).

start(a, CallRate) ->
	start_link(CallRate, {127,0,0,1}, "ims.vodafone.pt", a).
start_link(CallRate, Addr, Domain, Type)->
	gen_server:start_link(?MODULE, [CallRate, Addr, Domain, Type], []).

stop(Pid) ->
	gen_server:call(Pid, stop).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {timer = undefined, callrate = 0, addr, domain, type}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([CallRate, Addr, Domain, Type]) ->
	{ok, TRef} = timer:send_interval(1000, start_to_send),
    {ok, #state{timer = TRef, callrate = CallRate, addr = Addr, domain = Domain, type = Type}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(stop, _From, State)->
	{stop, shutdown, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(start_to_send, #state{addr = Addr, domain = Domain, type = Type, callrate = CallRate} = State)->
	spawn(?MODULE, create_q_process, [CallRate, [Domain, Addr, Type]]),
	{noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, #state{timer=TRef} = _State) ->
	timer:cancel(TRef),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
create_q_process(Number, [Domain, NS, Type]=_Args) ->
	T1=erlang:now(),
	{ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    Query = inet_dns:encode(
    #dns_rec{
        header = #dns_header{
            id = crypto:rand_uniform(1,16#FFFF),
            opcode = 'query',
            rd = true
        },
        qdlist = [#dns_query{
            domain = Domain,
            type = Type,
            class = in
        }]
    }),
	q(Query, NS, Socket, Number),
	io:format("spend ~p ms~n", [timer:now_diff(erlang:now(), T1)/1000]),
	gen_udp:close(Socket).
q(_Query, _NS, _Socket, 0) ->
	ok;
q(Query, NS, Socket, Number) ->
    gen_udp:send(Socket, NS, 53, Query),
    {ok, {NS, _P, _Reply}} = gen_udp:recv(Socket, 0, 1000),
	q(Query, NS, Socket, Number - 1).


