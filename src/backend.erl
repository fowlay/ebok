%% @author erarafo
%% @doc @todo Add description to backend.


-module(backend).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, load/0, save/0, stop/0, book/6, tell/0]).

-define(SERVER, ?MODULE).
-define(PERSISTENT, "ebok.txt").

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

load() ->
    gen_server:call(?SERVER, load).

save() ->
    gen_server:call(?SERVER, save).

stop() ->
    gen_server:call(?SERVER, stop).

book(earnings, Year, Month, Day, Amount, Comment) ->
    Ores = trunc(Amount*100),
    gen_server:call(?SERVER, {book, earnings, {Year, Month, Day}, Ores, Comment}),
    ok;

book(cost, Year, Month, Day, Amount, Comment) ->
    Ores = trunc(Amount*100),
    gen_server:call(?SERVER, {book, cost, {Year, Month, Day}, Ores, Comment}),
    ok.

tell() ->
    gen_server:call(?SERVER, tell).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state,
        {dict=orddict:new(),
         file=?PERSISTENT
         }).

-record(key,
        {seq :: integer(),
         date :: calendar:date(),
         type :: cost|earnings|accrual
        }).

-record(value,
        {ores :: integer(),
         comment="" :: string()
        }
       ).

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
init([]) ->
    {ok, #state{}}.


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

handle_call({book, earnings, {Y, M, D}, Amount, Comment}, _From, #state{dict=Dict}=State) ->
    Seq = {Y, get_next_number(Dict, Y)},
    Key = #key{seq=Seq, date={M, D}, type=earnings},
    CommentJoined = string:join(Comment, " "),
    NewDict = orddict:store(Key, #value{ores=Amount, comment=CommentJoined}, Dict),
    {reply, ok, State#state{dict=NewDict}};

handle_call({book, cost, {Y, M, D}, Amount, Comment}, _From, #state{dict=Dict}=State) ->
    Seq = {Y, get_next_number(Dict, Y)},
    Key = #key{seq=Seq, date={M, D}, type=cost},
    CommentJoined = string:join(Comment, " "),
    NewDict = orddict:store(Key, #value{ores=Amount, comment=CommentJoined}, Dict),
    {reply, ok, State#state{dict=NewDict}};

handle_call(tell, _From, State) ->
    {reply, State, State};

handle_call(save, _From, #state{file=File, dict=Dict}=State) ->
    try
        {ok, Stream} = file:open(File, [write]),
        orddict:fold(
          fun(K, V, _Acc) ->
                  io:fwrite(Stream, "~p.~n", [{K, V}])
          end,
          ignore,
          Dict),
        file:close(Stream),
        {reply, ok, State}
    catch
        X:Y:Stack ->
            {reply, {nok, {X, Y, Stack}}, State}
    end;

handle_call(load, _From, #state{file=File}=State) ->
    try
        {ok, Stream} = file:open(File, [read]),
        Dict = dict_from_stream(Stream, orddict:new()),
        {reply, ok, State#state{dict=Dict}}
    catch
        X:Y:Stack ->
            {reply, {nok, {X, Y, Stack}}, State}
    end;

handle_call(stop, _From, State) ->
    Reply = stopped,
    {stop, normal, Reply, State};

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
terminate(Reason, _State) ->
    ebok:respond("~p stopping, reason: ~p", [?MODULE, Reason]),
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

get_next_number(Dict, Y) ->
    1 + orddict:fold(
      fun(#key{seq={U, M}}, _V, Acc) when U =:= Y andalso M > Acc ->
              M;
         (_, _V, Acc) ->
              Acc
      end,
      0,
      Dict).

dict_from_stream(Stream, Dict) ->
    case io:read(Stream, "") of
        eof ->
            Dict;
        {ok, {K, V}} ->
            NewDict = orddict:store(K, V, Dict),
            dict_from_stream(Stream, NewDict)
    end.



