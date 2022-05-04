%% @author erarafo
%% @doc @todo Add description to ebok.


-module(ebok).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([ebok/1]).

-export([respond/1, respond/2]).

ebok(_Args) ->
    %% io:fwrite("args: ~p~n", [Args]),
    _Pid = start(),
    wait_for_termination().

start() ->
    {ok, Pid} = gen_server:start(?MODULE, [self()], []),
    Pid.

wait_for_termination() ->
    receive stop -> ok end,
    respond("terminate!").






%% ====================================================================
%% Behavioural functions
%% ====================================================================
-define(TIMEOUT_ZERO, 0).

-record(state,
        {master :: pid(),
         year :: integer()|undefined
         }).

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
init([Master]) ->
    {ok, _Pid} = backend:start(),
    State = #state{master = Master},
    {ok, State, ?TIMEOUT_ZERO}.


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

handle_info(timeout, State) ->
    Lexemes = get_input(),
    respond("Input: ~p", [Lexemes]),
    try
        dispatch(Lexemes, State)
    catch
        X:Y:Stack ->
            respond("caught: ~p, reason: ~p, stack:~n~p", [X, Y, Stack]),
            {noreply, State, ?TIMEOUT_ZERO}
    end;

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
terminate(_Reason, #state{master = Master}) ->
    stopped = backend:stop(),
    Master ! stop,
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

respond(String) ->
    respond("~s", [String]).

respond(Format, Data) ->
    io:fwrite(Format++"~n", Data).

get_input() ->
    RawLine = io:get_line("> "),
    Line = string:trim(RawLine),
    string:lexemes(Line, " ").

dispatch(["q"], State) ->
    {stop, normal, State};

dispatch(["h"], State) ->
    respond("~p", [State]),
    BackendState = backend:tell(),
    respond("backend: ~p", [BackendState]),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["l"], State) ->
    Result = backend:load(),
    respond("result: ~p", [Result]),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["s"], State) ->
    Result = backend:save(),
    respond("result: ~p", [Result]),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch([Action, _MonthS, _DayS, _AmountS], #state{year = undefined}) when
  Action =:= "e" orelse Action =:= "c" ->
    throw(year_is_not_set);

dispatch(["e", MonthS, DayS, AmountS], #state{year = Year} = State) ->
    Month = list_to_integer(MonthS),
    Day = list_to_integer(DayS),
    Amount = string_to_float(AmountS),
    backend:book(earnings, Year, Month, Day, Amount),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["c", MonthS, DayS, AmountS], #state{year = Year} = State) ->
    Month = list_to_integer(MonthS),
    Day = list_to_integer(DayS),
    Amount = string_to_float(AmountS),
    backend:book(cost, Year, Month, Day, Amount),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["y", Year], State) ->
    {noreply, State#state{year = list_to_integer(Year)}, ?TIMEOUT_ZERO};

dispatch(_, State) ->
    respond("not understood"),
    {noreply, State, ?TIMEOUT_ZERO}.


string_to_float(X) ->
    case string:to_float(X) of
        {error, no_float} ->
            string_to_float(X++".0");
        {U, []} when is_float(U) ->
            U;
        {U, Rest} when is_float(U) ->
            throw({bad_number, {X, Rest}});
        {error, Reason} ->
            throw({bad_number, {X, Reason}});
        _ ->
            string_to_float(X++".0")
    end.

