%% @author erarafo
%% @doc @todo Add description to backend.


-module(backend).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/3,
         load/0,
         save/0,
         stop/0,
         book/6,
         get_book/1,
         summary/1,
         set_verbose/1,
         tell/0]).

-define(SERVER, ?MODULE).
-define(FILE_PREFIX, "ebok-").

start(Master, Dir, Vat) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [Master, Dir, Vat], []).

load() ->
    gen_server:call(?SERVER, load).

save() ->
    gen_server:call(?SERVER, save).

stop() ->
    gen_server:call(?SERVER, stop).

book(earnings, Year, Month, Day, Amount, Comment) ->
    %% Amount is SEK, VAT included
    Sek = Amount,
    gen_server:call(?SERVER, {book, earnings, {Year, Month, Day}, Sek, Comment});

book(cost, Year, Month, Day, Amount, Comment) ->
    Sek = Amount,
    gen_server:call(?SERVER, {book, cost, {Year, Month, Day}, Sek, Comment});

book(accrual, Year, Month, Day, Amount, Comment) ->
    Sek = Amount,
    gen_server:call(?SERVER, {book, accrual, {Year, Month, Day}, Sek, Comment}).

get_book(Year) ->
    gen_server:call(?SERVER, {get_book, Year}).

summary(Year) ->
    gen_server:call(?SERVER, {summary, Year}).

set_verbose(Verbose) ->
    gen_server:call(?SERVER, {set_verbose, Verbose}).

tell() ->
    gen_server:call(?SERVER, tell).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state,
        {master :: pid(),
         dict=orddict:new(),
         dir="" :: string(),
         verbose=0 :: integer(),
         vat=0.0 :: float()
        }).

-type year() :: integer().
-type month() :: 1..12.
-type day() :: 1..31.

-record(key,
        {seq :: {year(), integer()}
        }).

-record(value,
        {date :: {month(), day()},
         type :: cost|earnings|accrual,
         sek :: float(),
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
init([Master, Dir, Vat]) ->
    case filelib:is_dir(Dir) of
        false ->
            {stop, {not_a_directory, Dir}};
        true ->
            {ok, #state{master=Master, dir=Dir, vat=Vat}}
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

handle_call({book, earnings, {Y, M, D}, Amount, Comment}, _From, State) ->
    do_book(earnings, {Y, M, D}, Amount, Comment, State);

handle_call({book, cost, {Y, M, D}, Amount, Comment}, _From, State) ->
    do_book(cost, {Y, M, D}, Amount, Comment, State);

handle_call({book, accrual, {Y, M, D}, Amount, Comment}, _From, State) ->
    do_book(accrual, {Y, M, D}, Amount, Comment, State);

handle_call({get_book, Year}, _From, #state{dict=Dict}=State) ->
    Result =
        lists:reverse(
          orddict:fold(
            fun(#key{seq={Y, _}}, _, Acc) when Y =/= Year ->
                    Acc;
               (#key{seq=Seq}, #value{date=Date, type=Type, sek=Amount, comment=Comment}, Acc) ->
                    [{Seq, Date, Type, Amount, Comment}|Acc]
            end,
            [],
            Dict)),
    {reply, Result, State};

handle_call({summary, Year}, _From, #state{dict=Dict, vat=Vat}=State) ->
    VatFrac = Vat/(100.0 + Vat),
    NetFrac = 100.0/(100.0 + Vat),
    Map =
        orddict:fold(
          fun(#key{seq={Y, _}}, _, Acc) when Y =/= Year ->
                  Acc;
             
             (#key{}, #value{type=earnings, sek=Sek}, Acc) ->
                  maps_acc(outgVat, VatFrac*Sek,
                           maps_acc(earningsNetNoAccrual, NetFrac*Sek, 
                                    maps_acc(earningsNet, NetFrac*Sek, Acc)));
             
             (#key{}, #value{type=cost, sek=Sek}, Acc) ->
                  maps_acc(incVat, VatFrac*Sek,
                           maps_acc(costNet, NetFrac*Sek, Acc));
             
             %% handle non-default VAT costs here when needed
             
             (#key{}, #value{type=accrual, sek=Sek}, Acc) ->
                  maps_acc(earningsNet, Sek, Acc)
          end,
          #{},
          Dict),
    {reply, Map, State};

handle_call({set_verbose, NewVerbose}, _From, #state{verbose=Verbose}=State) ->
    {reply, {Verbose, NewVerbose}, State#state{verbose=NewVerbose}};

handle_call(tell, _From, State) ->
    {reply, State, State};

handle_call(save, _From, #state{dir=Dir, dict=Dict}=State) ->
    Path = filename:join(Dir, get_basename(Dir, 1)),
    try
        {ok, Stream} = file:open(Path, [write]),
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

handle_call(load, _From, #state{dir=Dir}=State) ->
    try
        Path = filename:join(Dir, get_basename(Dir, 0)),
        {ok, Stream} = file:open(Path, [read]),
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

do_book(Type, {Y, M, D}, Amount, Comment, #state{dict=Dict}=State) ->   
    Seq = {Y, get_next_number(Dict, Y)},
    Key = #key{seq=Seq},
    CommentJoined = string:join(Comment, " "),
    NewDict = orddict:store(Key, #value{date={M, D}, type=Type, sek=Amount, comment=CommentJoined}, Dict),
    {reply, ok, State#state{dict=NewDict}}.


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
terminate(Reason, State) ->
    verbose(State, "~p stopping, reason: ~p", [?MODULE, Reason]).


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

maps_acc(Key, Amount, Map) ->
    NewAmount = maps:get(Key, Map, 0) + Amount,
    maps:put(Key, NewAmount, Map).


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

get_basename(Dir, Incr) ->
    case file:list_dir(Dir) of
        {ok, []} ->
            ?FILE_PREFIX ++ "0001";
        {ok, Names} ->
            Highest =
            lists:foldl(
              fun(S, A) ->
                      K =
                          list_to_integer(
                            lists:filter(fun(C) -> C >= $0 andalso C =< $9 end, S)),
                      if K > A -> K; true -> A end
              end,
              0,
              Names),
            lists:flatten(io_lib:format("~s~4..0w", [?FILE_PREFIX, Highest+Incr]))                       
    end.

verbose(State, Format, Data) ->
    if
        State#state.verbose < 1 ->
            ok;
        true ->
            io:fwrite(Format++"~n", Data)
    end.
