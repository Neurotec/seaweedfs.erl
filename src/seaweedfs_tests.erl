-module (seaweedfs_tests).
-include_lib ("eunit/include/eunit.hrl").
-include_lib ("seaweedfs.hrl").

assign_test () ->
    hackney:start (),
    {ok, Pid} = seaweedfs:start_link ("localhost:9333"),
    seaweedfs:assign (Pid).


upload_and_download_test() ->
    hackney:start (),
    {ok, Pid} = seaweedfs:start_link ("localhost:9333"),
    File = seaweedfs:assign (Pid),
    {ok, FileRef} = seaweedfs_upload:start (File),
    Data = <<"meromero">>,
    seaweedfs_upload:write (FileRef, Data),
    seaweedfs_upload:stop (FileRef),
    {ok, Ref} = seaweedfs_download:start_link (File),
    {ok, Data} = seaweedfs_download:read_all (Ref),
    seaweedfs_download:stop (Ref).


write_file_test () ->
    hackney:start (),
    {ok, Pid} = seaweedfs:start_link ("localhost:9333"),
    File = seaweedfs:assign (Pid),
    {ok, FileRef} = seaweedfs_upload:start (File),
    seaweedfs_upload:write_file (FileRef, <<"./LICENSE">>),
    seaweedfs_upload:stop (FileRef).
