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

-define(VAT, 25.0).


help() ->
    ["  c MON DAY AMT COMMENT...         book cost",
     "  e MON DAY AMT COMMENT...         book earnings",
     "  a MON DAY SIGNED_AMT COMMENT...  book accrual",
     "  B                                book print",
     "  S                                summary",
     "  y YEAR                           specify year",
     "  h                                this help",
     "  v INCR                           verbosity change",
     "  l                                load",
     "  s                                save",
     "  q                                quit"
     ].

ebok([Dir]) ->
    start(Dir),
    wait_for_termination().

start(Dir) ->
    case gen_server:start(?MODULE, [self(), Dir], []) of
        {error, _} ->
            self() ! stop;
        {ok, _Pid} ->
            ok
    end.

wait_for_termination() ->
    receive
        stop ->
            init:stop()
    end.

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-define(TIMEOUT_ZERO, 0).

-record(state,
        {master :: pid(),
         year=current_year() :: integer(),
         verbose=0 :: integer()
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
init([Master, Dir]) ->
    case backend:start(self(), Dir, ?VAT) of
        {error, Reason} ->
            respond("fatal: ~p", [Reason]),
            {stop, Reason};
        {ok, _Pid} ->
            State = #state{master=Master},
            {ok, State, ?TIMEOUT_ZERO}
    end.


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
    Lexemes = get_input(State#state.year),
    if
        State#state.verbose > 0 ->
            respond("Input: ~p", [Lexemes]);
        true -> ok
    end,
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
terminate(_Reason, #state{master=Master}) ->
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

get_input(Year) ->
    Prompt = lists:flatten(io_lib:format("~w> ", [Year])),
    RawLine = io:get_line(user, Prompt),
    Line = string:trim(RawLine),
    string:lexemes(Line, " ").

dispatch(["h"], #state{verbose=Verbose}=State) ->
    %% help
    lists:foreach(fun(S) -> respond(S) end, help()),
    
    if
        Verbose > 0 ->
            respond("~p", [State]),
            BackendState = backend:tell(),
            respond("backend: ~p", [BackendState]);
        true ->
            ok
    end,
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["q"], State) ->
    %% quit
    {stop, normal, State};

dispatch(["v", Incr], #state{verbose=Verbose}=State) ->
    %% update verbosity
    NewVerbose = Verbose + list_to_integer(Incr),
    backend:set_verbose(NewVerbose),
    {noreply, State#state{verbose=NewVerbose}, ?TIMEOUT_ZERO};

dispatch(["l"], State) ->
    %% load
    Result = backend:load(),
    respond("result: ~p", [Result]),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["s"], State) ->
    %% save
    Result = backend:save(),
    respond("result: ~p", [Result]),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["e", MonthS, DayS, AmountS|Comment], #state{year=Year}=State) ->
    %% book earnings
    Month = list_to_integer(MonthS),
    Day = list_to_integer(DayS),
    Amount = string_to_float(AmountS),
    backend:book(earnings, Year, Month, Day, Amount, Comment),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["c", MonthS, DayS, AmountS|Comment], #state{year=Year}=State) ->
    %% book a cost
    Month = list_to_integer(MonthS),
    Day = list_to_integer(DayS),
    Amount = string_to_float(AmountS),
    backend:book(cost, Year, Month, Day, Amount, Comment),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["a", MonthS, DayS, AmountS|Comment], #state{year=Year}=State) ->
    %% book accrual
    Month = list_to_integer(MonthS),
    Day = list_to_integer(DayS),
    Amount = string_to_float(AmountS),
    backend:book(accrual, Year, Month, Day, Amount, Comment),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["S"], #state{year=Year}=State) ->
    %% print summary
    Summary = backend:summary(Year),
    print_summary(Summary),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["B"], #state{year=Year}=State) ->
    %% print book
    Book = backend:get_book(Year),
    print_book(Book),
    {noreply, State, ?TIMEOUT_ZERO};

dispatch(["y", Year], State) ->
    %% set year
    {noreply, State#state{year=list_to_integer(Year)}, ?TIMEOUT_ZERO};

dispatch(_, State) ->
    respond("not understood"),
    {noreply, State, ?TIMEOUT_ZERO}.


string_to_float(X) ->
    try
        K = list_to_integer(X),
        float(K)
    catch
        error:badarg ->
            list_to_float(X)
    end.

print_book(B) ->
    lists:foreach(
      fun({{Year, J}, {Mon, Day}, Type, Sek, Comment}) ->
              respond("~3w  ~4w-~2..0w-~2..0w  ~10s ~10.2f  ~s~n",
                      [J, Year, Mon, Day, Type, Sek, Comment])
      end,
      B).
              

print_summary(R) ->
    respond("summary:~n"
            "earnings net: ~.2f~n"
            "    outg VAT: ~.2f~n"
            "    cost net: ~.2f~n"
            "     inc VAT: ~.2f~n",
            [float(maps:get(earningsNet, R, 0)),
             float(maps:get(outgVat, R, 0)),
             float(maps:get(costNet, R, 0)),
             float(maps:get(incVat, R, 0))
            ]).

current_year() ->
    {{Year, _, _}, _} =
        calendar:system_time_to_local_time(
          erlang:system_time(seconds), seconds),
    Year.
