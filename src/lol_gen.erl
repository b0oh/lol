-module(lol_gen).
-export([forms/2]).

-record(env, {bindings = []}).
-record(fn, {name, arity, export = false}).


add_binding(Binding, #env{bindings = Bs} = Env) ->
    Env#env{bindings = [Binding | Bs]}.

exports(#env{bindings = Bs}) ->
    [{Name, Arity} || #fn{name = Name, arity = Arity, export = true} <- Bs].


env([], Env) -> Env;
env([Expr | Exprs], Env0) ->
    case declare(Expr, Env0) of
        {error, Error} -> {error, Error};
        Env1 -> env(Exprs, Env1)
    end.


declare({list, _, [{symbol, _, defn}, {symbol, _, Name}, {list, _, Args} | _]}, Env) ->
    add_binding(#fn{name = Name, arity = length(Args), export = true}, Env);
declare({list, _, [{symbol, _, 'defn-'}, {symbol, _, Name}, {list, _, Args} | _]}, Env) ->
    add_binding(#fn{name = Name, arity = length(Args)}, Env);
declare({eof, _}, St) ->
    St;
declare(Expr, _) -> error_before(Expr).


forms(Exprs, Mod) ->
    case env(Exprs, #env{}) of
        {error, Error} -> {error, Error};
        Env ->
            Exports = exports(Env),
            Forms = [form(Expr) || Expr <- Exprs],
            {ok, [{attribute, 0, module, Mod}, {attribute, 0, export, Exports} | Forms]}
    end.


%% 1
form({fixnum, L, Integer}) -> {integer, L, Integer};
%% symbol
form({symbol, L, Symbol}) -> {var, L, Symbol};
%% "string"
form({string, _, _} = Expr) -> Expr;
%% (list ...)
form({list, L, [{symbol, _, list} | Exprs]}) -> list_form(fun form/1, L, Exprs);
%% (tuple ...)
form({list, L, [{symbol, _, tuple} | Exprs]}) -> {tuple, L, seq(Exprs)};
%% 'quoted
form({quote, _, Expr}) -> quote_form(Expr);
%% (defn[-] name (args ...) body)
form({list, L, [{symbol, _, Def}, {symbol, _, Name}, {list, _, Args} | Body]}) when Def =:=  defn;
                                                                                    Def =:= 'defn-' ->
    {function, L, Name, length(Args), [{clause, L, [form(Arg) || Arg <- Args], [], seq(Body)}]};
%% (lambda (args ...) body)
form({list, L, [{symbol, _, lambda}, {list, _, Args} | Body]}) ->
    {'fun', L, {clauses, [{clause, L, [form(Arg) || Arg <- Args], [], seq(Body)}]}};
%% (= x y)
%form({list, L, [{symbol, _, '='}, Left, Right]}) ->
%    {match, L, form(Left), form(Right)};
%% (let (bindings) (do body))
form({list, _, [{symbol, _, 'let'}, {list, _, Bindings}, {list, _, [{symbol, _, do} | Body]}]}) ->
    matches(Bindings) ++ seq(Body);
%% (let (bindings) body)
form({list, _, [{symbol, _, 'let'}, {list, _, Bindings}, Body]}) ->
    matches(Bindings) ++ [form(Body)];
%% (case expr (match body))
form({list, _, [{symbol, L, 'case'}, Expr | Clauses]}) ->
    case_form(L, Expr, Clauses);
%% (call fun)
form({list, L, [{symbol, _, call}, Fun]}) ->
    call_form(L, form(Fun), []);
%% (call fun (list args))
form({list, L, [{symbol, _, call}, Fun, {list, _, [{symbol, _, list} | Args]}]}) ->
    call_form(L, form(Fun), Args);
%% (call mod fun)
form({list, L, [{symbol, _, call}, Mod, Fun]}) ->
    call_form(L, {remote, L, form(Mod), form(Fun)}, []);
%% (call mod fun (list args))
form({list, L, [{symbol, _, call}, Mod, Fun, {list, _, [{symbol, _, list} | Args]}]}) ->
    call_form(L, {remote, L, form(Mod), form(Fun)}, Args);
%% ([mod:]fun args ...)
form({list, L, [{symbol, _, ModFun} | Args]}) ->
    Callable =
        case string:tokens(atom_to_list(ModFun), ":") of
            [Mod, Fun] -> {remote, L, {atom, L, list_to_atom(Mod)},
                                      {atom, L, list_to_atom(Fun)}};
            _          -> {atom, L, ModFun}
        end,
    call_form(L, Callable, Args);
%% ((expr) args ...)
form({list, L, [Fun | Args]}) ->
    call_form(L, form(Fun), Args);
%% eof
form({eof, L}) -> {eof, L};
form(Expr) -> error_before(Expr).

quote_form({symbol, L, Symbol}) -> {atom, L, Symbol};
quote_form({list, L, List}) -> list_form(fun quote_form/1, L, List);
quote_form(Expr) -> form(Expr).

list_form(_, L, []) -> {nil, L};
list_form(Former, _, [H | T]) ->
    L = line(H),
    {cons, L, Former(H), list_form(Former, L, T)}.

call_form(L, Callable, Args) ->
    {call, L, Callable, [form(Arg) || Arg <- Args]}.

seq(Exprs) ->
    lists:flatten([form(Expr) || Expr <- Exprs]).

matches([]) -> [];
matches([Left, Right | Bindings]) ->
    [{match, line(Left), form(Left), form(Right)} | matches(Bindings)].

case_form(L, Expr, Clauses) ->
     {'case', L, form(Expr), [case_clause_form(Clause) || Clause <- Clauses]}.

case_clause_form({list, L, [Match, {list, _, [{symbol, _, do} | Body]}]}) -> {clause, L, [form(Match)], [], seq(Body)};
case_clause_form({list, L, [Match, Body]}) -> {clause, L, [form(Match)], [], [form(Body)]}.


line({_, L, _}) -> L;
line({_, L})    -> L.


show({fixnum, _, Integer})    -> integer_to_list(Integer);
show({symbol, _, Symbol})     -> atom_to_list(Symbol);
show({string, _, String})     -> [$", String, $"];
show({list,   _, []})         -> "()";
show({list,   _, [Expr]})     -> [$(, show(Expr), $)];
show({list,   _, [Expr | _]}) -> [$(, show(Expr), " ...)"];
show({eof,    _})             -> "eof".

error_before(Expr) -> lol_parse:syntax_error(line(Expr), {error_before, show(Expr)}).
