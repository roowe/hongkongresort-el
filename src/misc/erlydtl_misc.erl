-module(erlydtl_misc).

-export([compile/0]).

-include("common.hrl").

compile() ->
    OutDir = out_dir(),
    TemplateDir = template_dir(),
    Options = [{doc_root, TemplateDir}, {out_dir, OutDir}],
    [compile(TemplateDir, File, Options) || File <- template_files()].

template_files() ->
    filelib:wildcard(filename:join(template_dir(), "*/*.djhtml")).

compile(TemplateDir, File, Options) ->
    TemplateDir = template_dir(),
    [_|Rest] = File -- TemplateDir,
    Module = list_to_atom(re:replace(lists:sublist(Rest, length(Rest) - length(".djhtml")), "/", "_", [global, {return, list}])),
    %%?DEBUG("Module ~p, File ~ts~n", [Module, File]),
    {ok, Module} = erlydtl:compile_file(File, Module, Options).


out_dir() ->
    filename:join(app_misc:root_dir(), "ebin").


template_dir() ->
    filename:join(app_misc:root_dir(), "priv/fe_release/views/gs_admin/").


