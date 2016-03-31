%%%-------------------------------------------------------------------
%% @doc seaweedfs top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(seaweedfs_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(MasterUrl)  when is_list (MasterUrl)->
    supervisor:start_link({local, ?SERVER}, ?MODULE, MasterUrl).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(MasterUrl) ->
    {ok, { {one_for_all, 0, 1}, [
				 {seaweedfs,
				  {seaweedfs, start_link, [MasterUrl]},
				  permanent,
				  5000,
				  worker,
				  [seaweedfs]}
				]} }.

%%====================================================================
%% Internal functions
%%====================================================================
