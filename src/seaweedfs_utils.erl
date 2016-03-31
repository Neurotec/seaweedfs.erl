-module (seaweedfs_utils).

-include_lib ("seaweedfs.hrl").

-export ([file_cxt_from_json/1, file_url/1]).

file_cxt_from_json (Data) ->
    {Decode} = jiffy:decode (Data),
    #file_cxt{
       count=proplists:get_value (<<"count">>, Decode), 
       fid=proplists:get_value (<<"fid">>, Decode), 
       url=proplists:get_value (<<"url">>, Decode), 
       publicUrl=proplists:get_value (<<"publicUrl">>, Decode)}.


file_url (FileCxt = #file_cxt{}) ->
    "http://" ++ binary:bin_to_list(FileCxt#file_cxt.publicUrl) ++ "/" ++ binary:bin_to_list(FileCxt#file_cxt.fid).
