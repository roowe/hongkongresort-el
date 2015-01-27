-module(csv_parse).
-export([parse/2]).
 
columns(Line) ->
	columns(<<>>, Line).
 
columns(Col, <<>>) -> 
	[Col];
columns(Col, <<$\t,Rest/binary>>) -> 
	[Col | columns(<<>>, Rest)];
columns(Col, <<C,Rest/binary>>) -> 
	columns(<<Col/binary, C>>, Rest).

parse(File, RecordFields) ->
    %% io:format("~ts ~n", [File]),
    {ok, Bin} = file:read_file(File),
    [FieldsBin, TypesBin, _CommentsBin|DataBin] = binary:split(Bin, [<<"\r\n"/utf8>>, <<"\n"/utf8>>], [global]),
    %% io:format("~w~n", [FieldsBin]),
    Fields = [binary_to_atom(FieldBin, utf8) || FieldBin <- columns(FieldsBin)],
    Types = columns(TypesBin),
    %% Comments = columns(CommentsBin),
    %% io:format("~p~n", [DataBin]),
    data(DataBin, Fields, Types, RecordFields).

data([], _, _, _) ->
    [];
data([<<"">>|DataBin], Fields, Types, RecordFields) ->
    data(DataBin, Fields, Types, RecordFields);
data([<<"#", _/binary>>|DataBin], Fields, Types, RecordFields) ->
    data(DataBin, Fields, Types, RecordFields);
data([Line|DataBin], Fields, Types, RecordFields) ->
    Columns = columns(Line),
    %% io:format("~p~n", [{length(Fields), length(Types), length(Columns)}]),
    List = zip3(Fields, Types, Columns),
    Result = [begin
                  %% case lists:keyfind(Field, 1, List) of
                  %%     false ->
                  %%         %% io:format("~p ~p~n", [List, Field]),
                  %%         undefined;
                  %%     {_, Type, Value} ->
                  %%         decoder(Type, Value)
                  %% end
                  {_, Type, Value} = lists:keyfind(Field, 1, List),
                  %% io:format("~p ~p~n", [Type, Value]),
                  decoder(Type, Value)
              end || Field <- RecordFields],
    [Result | data(DataBin, Fields, Types, RecordFields)].

decoder(<<"int">>, Value) ->
    erlang:binary_to_integer(Value);
decoder(<<"json">>, <<>>) ->
    {[]};
decoder(<<"json">>, Value0) ->
    %% 打patch，临时顶着
    Value = json_str(<<>>, Value0),
    %% io:format("json ~s~n", [Value]),
    jiffy:decode(Value);
decoder(_, Value) ->
    Value.


json_str(V, <<>>) ->
    V;
json_str(V, <<$",$",Rest/binary>>) -> 
    json_str(<<V/binary, "\"">>, Rest);
json_str(V, <<$",Rest/binary>>) -> 
    json_str(V, Rest);
json_str(V, <<C,Rest/binary>>) -> 
	json_str(<<V/binary, C>>, Rest).

%% 值可能为空
zip3([X | Xs], [Y | Ys], []) -> 
    [{X, Y, <<>>} | zip3(Xs, Ys, [])];
zip3([X | Xs], [Y | Ys], [Z | Zs]) -> 
    [{X, Y, Z} | zip3(Xs, Ys, Zs)];
zip3([], [], []) -> [].

