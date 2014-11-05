%% coding: utf-8
-module(erl_mysql).

%% 详细用法看测试代码
-export([insert/2, replace/2, 
         update/2, update/3,
         delete/1, delete/2, delete/3,
         select/1, select/2, select/3, select/4, select/5]).

%% TODO: maybe Values need support exp?
insert(Table, Args) ->
    insert2(<<"INSERT ">>, Table, Args).

replace(Table, Args) ->
    insert2(<<"REPLACE ">>, Table, Args).

insert2(Act, Table, FVList) when is_list(FVList) ->
    Names = make_list(FVList, fun({Name, _}) -> encode_field(Name) end),
    Values = [$(, make_list(FVList, fun({_, Val}) -> encode_val(Val) end), $)],
    make_insert_query(Act, Table, Names, Values);
insert2(Act, Table, {Fields, Records}) ->
    Names = make_list(Fields, fun encode_field/1),
    Values = make_list(Records, fun(Record) ->                                        
                                        [$(, make_list(Record, fun encode_val/1), $)]
                                end),
    make_insert_query(Act, Table, Names, Values).

make_insert_query(Act, Table, Names, Values) ->
    [Act, <<" INTO ">>, encode_field(Table),
     <<"(">>, Names, <<") VALUES ">>, Values].

%% 现知道的不足，以后也不打算支持
%% 1、只能操作单表，比如不支持 UPDATE a,b SET a.id=a.id*10 WHERE a.id=b.id
update(Table, Props) ->
    update(Table, Props, undefined).
update(Table, Props, Where) ->
    S1 = [<<"UPDATE ">>, encode_field(Table), <<" SET ">>],
    S2 = make_list(Props, fun({Field, Val}) ->
                                  [encode_field(Field), <<" = ">>, expr(Val)]
                          end),
    [S1, S2, where(Where)].

%% not support using
delete(Table) ->
    delete(Table, undefined, undefined).
delete(Table, WhereExpr) ->
    delete(Table, WhereExpr, undefined).
delete(Table, WhereExpr, Extras) ->
    S1 = [<<"DELETE FROM ">>, encode_field(Table)],
    S2 = [S1, where(WhereExpr)],
    if Extras =:= undefined ->
            S2;
       true ->
            [S2, extra_clause(Extras)]
    end.

select(Fields) ->
    select(undefined, Fields, undefined, undefined, undefined).

select(Fields, Tables) ->
    select(undefined, Fields, Tables, undefined, undefined).

select(Fields, Tables, WhereExpr) ->
    select(undefined, Fields, Tables, WhereExpr, undefined).

select(Fields, Tables, WhereExpr, Extras) ->
    select(undefined, Fields, Tables, WhereExpr, Extras).

select(Modifier, Fields, Tables, WhereExpr, Extras) ->
    S1 = <<"SELECT ">>,
    S2 = if Modifier =:= undefined -> 
                 S1;
            true ->
                 Modifier1 = 
                     case Modifier of
                         distinct -> 
                             'DISTINCT';
                         all -> 
                             'ALL';
                         Other -> 
                             Other
                     end,
                 [S1, convert(Modifier1), $\s]
         end,

    ListFun = fun(Val) -> 
                      expr2(Val)
              end,
    S3 = [S2, make_list(Fields, ListFun)],

    S4 = if
             Tables =:= undefined -> 
                 S3;
             true -> 
                 [S3, <<" FROM ">>, make_list(Tables, ListFun)]
         end,

    S5 = [S4, where(WhereExpr)],

    case extra_clause(Extras) of
        undefined -> 
            S5;
        Expr -> 
            [S5, Expr]
    end.


%% --------------------内部函数--------------------
where(undefined) ->
    [];
where(Expr) when is_binary(Expr) ->
    [$\s, <<"WHERE ", Expr/binary>>];
where(Exprs) when is_list(Exprs)->
    where(list_to_binary(Exprs));
where(Expr) when is_tuple(Expr) ->
    case expr(Expr) of
        undefined -> 
            [];
        Other -> 
            [<<" WHERE ">>, Other]
    end.

expr(undefined) -> 
    <<"NULL">>;
expr({Not, Expr}) when Not =:= 'not'; Not =:= '!' ->
    [<<"NOT ">>, verbatim_expr(Expr)];
expr({Table, Field}) when is_atom(Table), is_atom(Field) ->
    [encode_field(Table), $., encode_field(Field)];
expr({Expr1, as, Alias}) when is_atom(Alias) ->
    [expr2(Expr1), <<" AS ">>, encode_field(Alias)];
expr({call, FuncName, []}) ->
    [convert(FuncName), <<"()">>];
expr({call, FuncName, Params}) ->
    [convert(FuncName), $(, make_list(Params, fun param/1), $)];
expr({_, in, []}) -> 
    <<"0">>;
expr({Val, Op, Values}) when (Op =:= in orelse
                              Op =:= any orelse
                              Op =:= some) andalso is_list(Values) ->
    [expr2(Val), subquery_op(Op), make_list(Values, fun encode_val/1), $)];
expr({undefined, Op, Expr2}) when Op =:= 'and'; Op =:= 'not' ->
    expr(Expr2);
expr({Expr1, Op, undefined}) when Op =:= 'and'; Op =:= 'not' ->
    expr(Expr1);
expr({Expr1, Op, Expr2})  ->
    {B1, B2} = if (Op =:= 'and' orelse Op =:= 'or') ->
                       {verbatim_expr(Expr1), verbatim_expr(Expr2)};
                  true ->
                       {expr2(Expr1), expr2(Expr2)}
               end,
    [$(, B1, $\s, op(Op), $\s, B2, $)];

expr({list, Vals}) when is_list(Vals) ->
    [$(, make_list(Vals, fun encode_val/1), $)];
expr({Op, Exprs}) when is_list(Exprs) ->
    Res = [[expr(Expr)] || Expr <- Exprs ],
    [$(, string:join(Res,[$\s, op(Op), $\s]), $)];
expr(null) -> 
    <<"NULL">>;
%% we assume Val is field when is atom, so we will add `` in it.
expr(Val) when is_atom(Val) -> 
    encode_field(Val);
expr(Val) -> 
    encode_val(Val).

%% verbatim clauses
verbatim_expr(Expr) when is_list(Expr); is_binary(Expr) ->
    iolist_to_binary([$(, Expr, $)]);
verbatim_expr(Expr) -> 
    expr(Expr).

expr2(undefined) ->
    <<"NULL">>;
expr2(Expr) when is_atom(Expr) -> 
    encode_field(Expr);
expr2(Expr) ->
    expr(Expr).

op(Op) -> 
    convert(op1(Op)).
op1('and') -> 
    'AND';
op1('or') -> 
    'OR';
op1(like) -> 
    'LIKE';
op1(Op) -> 
    Op.

%% {call,search_people,[{age, 18}]} will equals search_people(age := 18)
param({Key, Value}) when is_atom(Key) ->
    [convert(Key), <<" := ">>, encode_val(Value)];
%% like COUNT(`id`) etc
param(Field) when is_atom(Field) ->
    encode_field(Field);
%% like COUNT(233) etc
param(Value) ->
    encode_val(Value).

subquery_op(in) -> 
    <<" IN (">>;
subquery_op(any) -> 
    <<" ANY (">>;
subquery_op(some) -> 
    <<" SOME (">>.

extra_clause([]) ->
    [];
extra_clause(undefined) -> 
    undefined;
extra_clause(Expr) when is_binary(Expr) ->
    [$\s, Expr];
%% it is a hack code, I assume it is a string.
extra_clause([Head|_] = Exprs) when is_integer(Head) ->
    [$\s, list_to_binary(Exprs)];
extra_clause([Expr|Rest]) when is_tuple(Expr);
                               is_list(Expr);
                               is_binary(Expr)->
    [extra_clause(Expr), extra_clause(Rest)];

extra_clause({limit, Num}) ->
    [<<" LIMIT ">>, encode_val(Num)];
extra_clause({limit, Offset, Num}) ->
    [<<" LIMIT ">>, encode_val(Offset), <<", ">> , encode_val(Num)];
extra_clause({group_by, ColNames}) ->
    [<<" GROUP BY ">>, make_list(ColNames, fun encode_field/1)];
extra_clause({group_by, ColNames, having, Expr}) ->
    [extra_clause({group_by, ColNames}), <<" HAVING ">>,
     expr(Expr)];
extra_clause({order_by, ColNames}) ->
    [<<" ORDER BY ">>,
     make_list(ColNames, fun
                             ({Name, Modifier}) when Modifier =:= 'asc' ->
                                 [expr(Name), $\s, convert('ASC')];
                             ({Name, Modifier}) when Modifier =:= 'desc' ->
                                 [expr(Name), $\s, convert('DESC')];
                             (Name) ->
                                 expr(Name)
                         end)].

convert(Val) when is_atom(Val)->
    atom_to_binary(Val, utf8).

make_list(Vals, ConvertFun) when is_list(Vals) ->
    string:join([[ConvertFun(Val)] || Val <- Vals],", ");
make_list(Val, ConvertFun) ->
    ConvertFun(Val).

encode_field(Field) when is_atom(Field) ->
    encode_field(convert(Field));
%% dirty fix
encode_field(<<"*">>=Field) ->
    Field;
encode_field(Field) ->
    backquote(Field).

%% @doc Encode a value as a binary to be embedded in
%% a SQL statement.
%%
%% This function can encode numbers, atoms values,
%% origin support date/time/datetime, I remove them.
%% binaries (which it escapes automatically).
encode_val(Val) when Val =:= undefined; Val =:= null ->
    <<"null">>;
encode_val(Val) when is_binary(Val) ->
    quote(Val);
encode_val(Val) when is_atom(Val) ->
    list_to_binary(quote(atom_to_list(Val)));
encode_val(Val) when is_list(Val) ->
    list_to_binary(quote(Val));
encode_val(Val) when is_integer(Val) ->
    integer_to_binary(Val);
encode_val(Val) when is_float(Val) ->
    list_to_binary(nicedecimal:format(Val));
encode_val(Val) ->
    {error, {unrecognized_value, {Val}}}.

%% for fields
backquote(String) when is_list(String);is_binary(String) ->
    iolist_to_binary([$`, String, $`]).

%% for values
quote(String) when is_list(String) ->
    [$' | lists:reverse([$' | quote(String, [])])];
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([$\0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([$\n | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([$\r | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([$' | Rest], Acc) ->
    quote(Rest, [$', $\\ | Acc]);
quote([$" | Rest], Acc) ->
    quote(Rest, [$", $\\ | Acc]);
quote([$\^Z | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).

