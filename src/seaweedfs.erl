-module (seaweedfs).
-behaviour (gen_server).

-include_lib ("seaweedfs.hrl").

% behaviour apis
-export ([init/1, terminate/2, handle_call/3, handle_cast/2]).

-export ([start_link/1, assign/1, assign/2, delete_fid/2]).

-record (state, {
	   master_url
	  }).

start_link (MasterUrl) when is_list (MasterUrl) ->
    gen_server:start_link (?MODULE, [{master_url, MasterUrl}], []).


init (Args) ->
    MasterUrl = proplists:get_value(master_url, Args),
    {ok, #state{ master_url = MasterUrl }}.

terminate (_, _) ->
    ok.

handle_call(assign, _From, State) ->
    {reply, dir_assign (State#state.master_url, undefined), State};

% @doc assign with ttl
% Supported TTL format
% The TTL is in the format of one integer number followed by one unit. The unit can be 'm', 'h', 'd', 'w', 'M', 'y'.
%
%Supported TTL format examples:
%
%    3m: 3 minutes
%    4h: 4 hours
%    5d: 5 days
%    6w: 6 weeks
%    7M: 7 months
%    8y: 8 years
handle_call({assign, TTL}, _From, State) when is_list (TTL) ->
    dir_assign (State#state.master_url, TTL ).

handle_cast ({delete_fid, Fid}, State)  when is_list (Fid) ->
    hackney:delete("http://" ++ State#state.master_url ++ "/" ++ Fid),
    {noreply, State};
handle_cast (_, State) ->
    {noreply, State}.

dir_assign (MasterUrl, TTL)->
    case TTL of
	undefined ->
	    {ok, 200, _, Body} = hackney:request (get, "http://" ++ MasterUrl ++ "/dir/assign");
	_ ->
	    {ok, 200, _, Body} = hackney:request (get, "http://" ++ MasterUrl ++ "/dir/assign?ttl=" ++ TTL)
    end,
    {ok, Data} = hackney:body (Body),
    seaweedfs_utils:file_cxt_from_json (Data).

assign (Pid, TTL) ->
    gen_server:call (Pid, {assign, atom_to_list(TTL)}).

assign (Pid) ->
    gen_server:call (Pid, assign).

delete_fid (Pid, Fid) ->
    gen_server:call (Pid, {delete_fid, Fid}).
