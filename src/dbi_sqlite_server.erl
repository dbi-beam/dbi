-module(dbi_sqlite_server).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start_link/0,

    open_database/2,
    close_database/1,
    get_database/1,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include("dbi.hrl").

-record(state, {
    databases = dict:new() :: ?DICT_TYPE()
}).

%%-----------------------------------------------------------------------------
%% API functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

open_database(Name, Database) ->
    gen_server:cast(?MODULE, {open, Name, Database}).

close_database(Name) ->
    gen_server:cast(?MODULE, {close, Name}).

get_database(Name) ->
    gen_server:call(?MODULE, {get, Name}).

%%-----------------------------------------------------------------------------
%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({get, Name}, _From, #state{databases=DB}=State) ->
    {reply, dict:find(Name, DB), State};

handle_call(_Msg, _From, State) ->
    {reply, {error, enoimpl}, State}.

handle_cast({open, Name, Database}, #state{databases=DB}=State) ->
    NewDB = case esqlite3:open(Database) of
        {ok, Conn} -> dict:store(Name, Conn, DB);
        {error, _Error} -> DB
    end,
    {noreply, State#state{databases=NewDB}};

handle_cast({close, Name}, #state{databases=DB}=State) ->
    NewDB = case dict:find(Name, DB) of
        error -> DB;
        {ok, Database} ->
            esqlite3:close(Database),
            dict:erase(Name, DB)
    end,
    {noreply, State#state{databases=NewDB}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
