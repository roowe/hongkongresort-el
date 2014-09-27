-module(crypto_misc).
-export([sha256/1]).

-export([md5/1]).

sha256(Data) ->
    ds_misc:to_hex(crypto:hash(sha256, io_lib:format("~w", [Data]))).

md5(S) ->        
    ds_misc:to_hex(erlang:md5(S)).
