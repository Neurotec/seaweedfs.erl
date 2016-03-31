-module (seaweedfs_upload).
-behaviour (gen_server).

-include_lib ("seaweedfs.hrl").

% behaviour api
-export ([init/1, terminate/2, handle_call/3, handle_cast/2]).

-export ([start/1, stop/1, write/2, write_file/2]).
-export ([fid/1]).

-record (state, {
	   file_cxt,
	   file_url,
	   client_ref
	  }).



start (FileCxt = #file_cxt{}) ->
    gen_server:start (?MODULE, FileCxt, []).

init (FileCxt) ->
    FileUrl = seaweedfs_utils:file_url (FileCxt),

    {ok, ClientRef} = hackney:request (put, FileUrl, [], stream_multipart, []),
    ok = hackney:send_multipart_body (ClientRef, {part, <<"file">>}),
    
    {ok, #state{
	    file_cxt = FileCxt,
	    file_url = FileUrl,
	    client_ref = ClientRef
	   }}.

terminate (_, _) ->
    ok.

handle_call ({write, Bytes}, _From, State) ->
    ok = hackney:send_multipart_body (State#state.client_ref, {part_bin, Bytes}),
    {reply, ok, State};
handle_call ({write_file, Path}, _From, State) ->
    ok = hackney:send_multipart_body (State#state.client_ref, {file, Path}),
    {reply, ok, State};
handle_call (fid, _From, State) ->
    {reply, State#state.file_cxt#file_cxt.fid, State}.

handle_cast (stop, State) ->
    ok = hackney:send_multipart_body (State#state.client_ref, {part, eof}),
    ok = hackney:send_multipart_body (State#state.client_ref,  eof),
    {stop, normal, State}.


%% @doc get file id
fid (Pid) ->
    gen_server:call (Pid, fid).

%% @doc write to file
write (Pid, Bytes) ->
    gen_server:call (Pid, {write, Bytes}).

%% @doc write all file
write_file (Pid, Path) ->
    gen_server:call (Pid, {write_file, Path}).

stop (Pid) ->
    gen_server:cast (Pid, stop).



	       
