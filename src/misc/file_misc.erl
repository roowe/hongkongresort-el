-module(file_misc).
%% 文件操作简单封装

-export([recursive_copy/2,
         recursive_delete/1]).

-export([write_term_file/2]).

-export([lock_file/1]).

-include_lib("kernel/include/file.hrl").


recursive_copy(Src, Dest) ->
    %% Note that this uses the 'file' module and, hence, shouldn't be
    %% run on many processes at once.
    case filelib:is_dir(Src) of
        false ->
            case file:copy(Src, Dest) of
                {ok, _Bytes} -> 
                    ok;
                {error, enoent} -> 
                    ok; %% Path doesn't exist anyway
                {error, Err} -> 
                    {error, {Src, Dest, Err}}
            end;
        true  -> 
            case file:list_dir(Src) of
                {ok, FileNames} ->
                    case file:make_dir(Dest) of
                        ok ->
                            lists:foldl(
                              fun (FileName, ok) ->
                                      recursive_copy(
                                        filename:join(Src, FileName),
                                        filename:join(Dest, FileName));
                                  (_FileName, Error) ->
                                      Error
                              end, ok, FileNames);
                        {error, Err} ->
                            {error, {Src, Dest, Err}}
                    end;
                {error, Err} ->
                    {error, {Src, Dest, Err}}
            end
    end.

recursive_delete(Files) ->    
    lists:foldl(fun(Path,  ok) -> 
                        recursive_delete1(Path);
                    (_Path, {error, _Err} = Error) ->
                        Error
                end, ok, Files).
      

recursive_delete1(Path) ->
    case filelib:is_dir(Path) and not(is_symlink(Path)) of
        false -> 
            case file:delete(Path) of
                ok  -> 
                    ok;
                {error, enoent} ->
                    ok; %% Path doesn't exist anyway
                {error, Err} -> 
                    {error, {Path, Err}}
            end;
        true  -> 
            case file:list_dir(Path) of
                {ok, FileNames} ->
                    case lists:foldl(
                           fun (FileName, ok) ->
                                   recursive_delete1(
                                     filename:join(Path, FileName));
                               (_FileName, Error) ->
                                   Error
                           end, ok, FileNames) of
                        ok ->
                            case file:del_dir(Path) of
                                ok -> 
                                    ok;
                                {error, Err} -> 
                                    {error, {Path, Err}}
                            end;
                        {error, _Err} = Error ->
                            Error
                    end;
                {error, Err} ->
                    {error, {Path, Err}}
            end
    end.

%% is_dir(Dir) -> 
%%     filelib:is_dir(Dir).
%% remove list_to_binary([io_lib:format..]),不知道为啥要to_binary，本身就是io_list
write_term_file(FileName, Terms) ->
    file:write_file(FileName, [io_lib:format("~p.~n", [Term]) || Term <- Terms]).


is_symlink(File) ->
    case file:read_link(File) of
        {ok, _} -> 
            true;
        _ -> 
            false
    end.

lock_file(Path) ->
    case file:open(Path, [write, exclusive]) of
        {ok, Lock} ->
            ok = file:close(Lock);
        {error, eexist} ->
            {error, eexist}
    end.
                   
