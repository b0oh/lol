-module(lol_compile).
-export([file/2]).
-export([read/1, parse/1, gen/1, compile/1, save/1]).

-record(st, {source, target, module, data, compiled, warns = [], errors = []}).

file(Source, Target) ->
    Mod = list_to_atom(filename:basename(Source, ".lol")),
    St = do([read, parse, gen, compile, save], #st{source = Source, target = Target, module = Mod}),
    return(St).


has_errors(#st{errors = []}) -> false;
has_errors(#st{})            -> true.

format_errors(_,        []) -> [];
format_errors(Filename, Es) -> [{Filename, lists:reverse(Es)}].

return(#st{source = Source, module = Mod, warns = Ws, errors = Es} = St) ->
    Filename = filename:basename(Source),
    Warns = format_errors(Filename, Ws),
    Errors = format_errors(Filename, Es),
    case has_errors(St) of
        true  -> {error, Errors, Warns};
        false -> {ok, Mod, Warns}
    end.

do([], St) -> St;
do([Action | As], St0) ->
    St1 = apply(?MODULE, Action, [St0]),
    case has_errors(St1) of
        true  -> St1;
        false -> do(As, St1)
    end.

read(#st{source = Source, errors = Es} = St) ->
    case file:read_file(Source) of
        {ok, Bin}      -> St#st{data = Bin};
        {error, Error} -> St#st{errors = [Error | Es]}
    end.

parse(#st{data = Bin, errors = Es} = St) ->
    case lol_parse:parse(Bin) of
        {ok, Exprs}    -> St#st{data = Exprs};
        {error, Error} -> St#st{errors = [Error | Es]}
    end.

gen(#st{data = Exprs, module = Mod, errors = Es} = St) ->
    case lol_gen:forms(Exprs, Mod) of
        {ok, Forms}    -> St#st{data = Forms};
        {error, Error} -> St#st{errors = [Error | Es]}
    end.

compile(#st{data = Forms, warns = Ws, errors = Es} = St) ->
    case compile:forms(Forms, [return]) of
        {ok, Mod, Beam, []} ->
            St#st{data = Beam, module = Mod};
        {ok, Mod, Beam, [{_, Warns}]} ->
            St#st{data = Beam, module = Mod, warns = lists:reverse(Warns, Ws)};
        {error, [{_, Errors}], []} ->
            St#st{errors = lists:reverse(Errors, Es)};
        {error, [{_, Errors}], [{_, Warns}]} ->
            St#st{errors = lists:reverse(Errors, Es), warns = lists:reverse(Warns, Ws)}
    end.

save(#st{target = Target, data = Data, errors = Es} = St) ->
    case file:write_file(Target, Data) of
        ok -> St;
        {error, Error} -> St#st{errors = [Error | Es]}
    end.
