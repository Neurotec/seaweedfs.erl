-module  (seaweedfs_download).
-behaviour (gen_server).

-include_lib ("seaweedfs.hrl").

-export ([init/1, terminate/2, handle_call/3, handle_cast/2]).

-export ([start_link/1, read_all/1, stream/2, stop/1]).


-record (state, {
	   body,
	   file_cxt
	   }).
start_link (FileCxt) ->
    gen_server:start_link(?MODULE, FileCxt, []).


init (FileCxt) ->
    {ok, 200, _, Body} = hackney:get (seaweedfs_utils:file_url (FileCxt)),
    {ok, #state{body=Body, file_cxt=FileCxt}}.

stop (Pid) ->
    gen_server:cast (Pid, stop).

terminate (_, _) ->
    ok.

handle_call (read_all, _From, State) ->
    {reply, hackney:body (State#state.body), State};
handle_call ({stream, MaxLength}, _From, State) ->
    {reply, read_body (MaxLength, State#state.body, <<>>), State}.

handle_cast (stop, State) ->
    {noreply, State}.

%% @doc read all file at once
read_all (Pid) ->
    gen_server:call (Pid, read_all).

%% @doc read by chunks of MaxLength
stream (Pid, MaxLength) ->
    gen_server:call (Pid, {stream, MaxLength}).


%% @private
read_body(MaxLength, Ref, Acc) when MaxLength > byte_size(Acc) ->
    case hackney:stream_body(Ref) of
        {ok, Data} ->
            read_body(MaxLength, Ref, << Acc/binary, Data/binary >>);
        done ->
            {ok, Acc};
        {error, Reason} ->
            {error, Reason}
    end.

