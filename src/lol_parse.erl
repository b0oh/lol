-module(lol_parse).
-export([parse/1, format_error/1, syntax_error/2]).

-define(is_digit(C), C >= $0, C =< $9).
-define(is_space(C), C =:= $\r; C =:= $\s; C =:= $\t).
-define(is_delimeter(C), ?is_space(C); C =:= $(; C =:= $);
                                       C =:= $[; C =:= $];
                                       C =:= ${; C =:= $}; C =:= $"; C =:= $;; C =:= $\n).
-define(is_upper(C), C >= $A, C =< $Z).
-define(is_lower(C), C >= $a, C =< $z).
-define(is_alpha(C), ?is_upper(C); ?is_lower(C)).
-define(is_symbol_init(C), ?is_alpha(C); C =:= $<; C =:= $>; C =:= $*; C =:= $/; C =:= $_;
                               C =:= $+; C =:= $-; C =:= $=; C =:= $?; C =:= $!; C =:= $:).
-define(is_symbol_cont(C), ?is_symbol_init(C); ?is_digit(C)).


format_error({unexpected_char, Char})  -> ["unexpected character: '", Char, "'"];
format_error(string) -> "unterminated string";
format_error({unmatched_paren, Paren}) -> ["unmatched paren: '", Paren, "'"];
format_error({error_before, Expr}) -> ["syntax error before: '", Expr, "'"].


syntax_error(Line, Error) ->
    {error, {Line, ?MODULE, Error}}.


parse(FileName) when is_list(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    parse(Bin);
parse(Bin) when is_binary(Bin) ->
    parse(Bin, 1, []).


parse(<<C, Rest/binary>>, L, Acc) when ?is_space(C) -> parse(Rest, L, Acc);
parse(<<$\n, Rest/binary>>, L, Acc) -> parse(Rest, L + 1, Acc);
parse(<<>>, L, Acc) -> {ok, lists:reverse([{eof, L} | Acc])};
parse(Bin, L0, Acc) ->
    case parse_expr(Bin, L0) of
        {Expr, L1, Rest} -> parse(Rest, L1, [Expr | Acc]);
        {error, Error}   -> {error, Error}
    end.


parse_expr(<<C, Rest/binary>>, L) when ?is_space(C) -> parse_expr(Rest, L);
parse_expr(<<$\n, Rest/binary>>, L) -> parse_expr(Rest, L + 1);
parse_expr(<<$-, C, Rest/binary>> = Bin, L) when ?is_digit(C) -> parse_fixnum(Rest, L, Bin, 2);
parse_expr(<<C, Rest/binary>> = Bin, L) when ?is_digit(C) -> parse_fixnum(Rest, L, Bin, 1);
parse_expr(<<C, _/binary>> = Bin, L) when ?is_symbol_init(C) -> parse_symbol(Bin, L);
parse_expr(<<$", Bin/binary>>, L) -> parse_string(Bin, L);
parse_expr(<<$', Bin/binary>>, L) -> parse_quote(Bin, L);
parse_expr(<<$(, Bin/binary>>, L) -> parse_cons(Bin, L);
parse_expr(<<$[, Bin/binary>>, L) -> parse_list(Bin, L);
parse_expr(<<${, Bin/binary>>, L) -> parse_tuple(Bin, L);
parse_expr(<<C, _/binary>>, L) -> syntax_error(L, {unexpected_char, C}).


parse_fixnum(Bin, L, Orig, Bytes) ->
    Bits = parse_fixnum_bytes(Bin, Bytes) * 8,
    <<Fixnum:Bits, Rest/binary>> = Orig,
    {{fixnum, L, binary_to_integer(<<Fixnum:Bits>>)}, L, Rest}.

parse_fixnum_bytes(<<C, Rest/binary>>, Bytes) when ?is_digit(C) -> parse_fixnum_bytes(Rest, Bytes + 1);
parse_fixnum_bytes(<<C, _/binary>>, Bytes) when ?is_delimeter(C) -> Bytes;
parse_fixnum_bytes(<<>>, Bytes) -> Bytes.

parse_symbol(Bin, L) ->
    Bits = parse_symbol_bytes(Bin, 0) * 8,
    <<Symbol:Bits, Rest/binary>> = Bin,
    {{symbol, L, list_to_atom(binary_to_list(<<Symbol:Bits>>))}, L, Rest}.

parse_symbol_bytes(<<C, Rest/binary>>, Bytes) when ?is_symbol_cont(C) -> parse_symbol_bytes(Rest, Bytes + 1);
parse_symbol_bytes(<<C, _/binary>>, Bytes) when ?is_delimeter(C) -> Bytes;
parse_symbol_bytes(<<>>, Bytes) -> Bytes.


parse_string(Bin, L0) ->
    case parse_string_bytes(Bin, L0, 0) of
        {error, Error} -> {error, Error};
        {L1, Bytes} ->
            Bits = Bytes * 8,
            <<String:Bits, $", Rest/binary>> = Bin,
            {{string, L0, binary_to_list(<<String:Bits>>)}, L1, Rest}
    end.

parse_string_bytes(<<$\n, Rest/binary>>, L, B) -> parse_string_bytes(Rest, L + 1, B + 1);
parse_string_bytes(<<C,   Rest/binary>>, L, B) when C =/= $" -> parse_string_bytes(Rest, L, B + 1);
parse_string_bytes(<<$",     _/binary>>, L, B) -> {L, B};
parse_string_bytes(<<>>, L, _B) -> syntax_error(L, string).


parse_quote(Bin, L0) ->
    case parse_expr(Bin, L0) of
        {error, Error}   -> {error, Error};
        {Expr, L1, Rest} -> {{quote, L0, Expr}, L1, Rest}
    end.


parse_cons(Bin, L0) ->
    case parse_cons_acc(Bin, L0, []) of
        {error, Error}   -> {error, Error};
        {List, L1, Rest} -> {{list, L0, lists:reverse(List)}, L1, Rest}
    end.


parse_cons_acc(<<C,   Rest/binary>>, L, Acc) when ?is_space(C) -> parse_cons_acc(Rest, L, Acc);
parse_cons_acc(<<$\n, Rest/binary>>, L, Acc) -> parse_cons_acc(Rest, L + 1, Acc);
parse_cons_acc(<<$),  Rest/binary>>, L, Acc) -> {Acc, L, Rest};
parse_cons_acc(<<>>, L, _Acc) -> syntax_error(L, {unmatched_paren, "("});
parse_cons_acc(Bin, L0, Acc) ->
    case parse_expr(Bin, L0) of
        {error, Error}   -> {error, Error};
        {Expr, L1, Rest} -> parse_cons_acc(Rest, L1, [Expr | Acc])
    end.


parse_list(Bin, L0) ->
    case parse_list_acc(Bin, L0, []) of
        {error, Error}   -> {error, Error};
        {List, L1, Rest} -> {{list, L0, [{symbol, L0, list} | lists:reverse(List)]}, L1, Rest}
    end.

parse_list_acc(<<C,   Rest/binary>>, L, Acc) when ?is_space(C) -> parse_list_acc(Rest, L, Acc);
parse_list_acc(<<$\n, Rest/binary>>, L, Acc) -> parse_list_acc(Rest, L + 1, Acc);
parse_list_acc(<<$],  Rest/binary>>, L, Acc) -> {Acc, L, Rest};
parse_list_acc(<<>>, L, _Acc) -> syntax_error(L, {unmatched_paren, "["});
parse_list_acc(Bin, L0, Acc) ->
    case parse_expr(Bin, L0) of
        {error, Error}   -> {error, Error};
        {Expr, L1, Rest} -> parse_list_acc(Rest, L1, [Expr | Acc])
    end.


parse_tuple(Bin, L0) ->
    case parse_tuple_acc(Bin, L0, []) of
        {error, Error}    -> {error, Error};
        {Tuple, L1, Rest} -> {{list, L0, [{symbol, L0, tuple} | lists:reverse(Tuple)]}, L1, Rest}
    end.

parse_tuple_acc(<<C,   Rest/binary>>, L, Acc) when ?is_space(C) -> parse_tuple_acc(Rest, L, Acc);
parse_tuple_acc(<<$\n, Rest/binary>>, L, Acc) -> parse_tuple_acc(Rest, L + 1, Acc);
parse_tuple_acc(<<$},  Rest/binary>>, L, Acc) -> {Acc, L, Rest};
parse_tuple_acc(<<>>, L, _Acc) -> syntax_error(L, {unmatched_paren, "{"});
parse_tuple_acc(Bin, L0, Acc) ->
    case parse_expr(Bin, L0) of
        {error, Error}   -> {error, Error};
        {Expr, L1, Rest} -> parse_tuple_acc(Rest, L1, [Expr | Acc])
    end.
