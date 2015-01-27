-module(crypto_misc).
-export([sha256/1]).

-export([md5/1, md5_file/1]).

sha256(Data) ->
    ds_misc:to_hex(crypto:hash(sha256, io_lib:format("~w", [Data]))).

md5(S) ->        
    ds_misc:to_hex(erlang:md5(S)).

md5_file(File) ->
    %% io:format("md5 ~s~n", [File]),
    {ok, Bin} = file:read_file(File),
    md5(Bin).
