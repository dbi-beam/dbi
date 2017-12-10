-module(dbi_delayed).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/2,
    do_query/2,
    do_query/3,
    stats/1
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("dbi.hrl").

-define(WAIT_FOR_QUERIES, 500). % ms

-record(state, {
    queue = queue:new() :: ?QUEUE_TYPE(),
    conn :: atom(),
    query_errors = 0 :: non_neg_integer(),
    query_ok = 0 :: non_neg_integer()
}).

%-----------------------------------------------------------------------------
% api calls

start_link(Ref, Conn) ->
    gen_server:start_link({local, Ref}, ?MODULE, [Conn], []).

do_query(Ref, Query, Args) ->
    gen_server:cast(Ref, {'query', Query, Args}).

do_query(Ref, Query) ->
    do_query(Ref, Query, []).

stats(Ref) ->
    gen_server:call(Ref, stats).

%-----------------------------------------------------------------------------
% gen_server calls

init([Conn]) ->
    Self = self(),
    spawn_link(fun() -> worker(Self) end),
    {ok, #state{conn=Conn}}.

handle_call(stats, _From, #state{queue=Queue}=State) ->
    Stats = [
        {size, queue:len(Queue)},
        {query_error, State#state.query_errors},
        {query_ok, State#state.query_ok}
    ],
    {reply, Stats, State};

handle_call(get, _From, #state{queue=Queue}=State) ->
    {Data, NewQueue} = queue:out(Queue),
    {reply, Data, State#state{queue=NewQueue}};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({result, ok}, #state{query_ok=QOK}=State) ->
    {noreply, State#state{query_ok=QOK+1}};

handle_cast({result, error}, #state{query_errors=QERR}=State) ->
    {noreply, State#state{query_errors=QERR+1}};

handle_cast({'query', Query, Args}, #state{queue=Queue, conn=Conn}=State) ->
    NewQueue = queue:in({Conn, Query, Args}, Queue),
    {noreply, State#state{queue=NewQueue}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    Conn ! stop,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%-----------------------------------------------------------------------------
% internal functions

worker(Ref) ->
    case gen_server:call(Ref, get) of
    empty ->
        timer:sleep(?WAIT_FOR_QUERIES),
        worker(Ref);
    {value, {Conn, Query, Args}} ->
        case catch dbi:do_query(Conn, Query, Args) of
            {ok, _, _} -> gen_server:cast(Ref, {result, ok});
            _ -> gen_server:cast(Ref, {result, error})
        end,
        worker(Ref)
    end.
