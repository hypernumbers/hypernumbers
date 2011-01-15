%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%% @doc Type and error checking macros used by stdfuns modules.

%%% Even though a lot of macros here are simple wrappers around function calls,
%%% it makes sense to keep them that way to make the type checking and casting
%%% code stand out in stdfuns.

%%% TODO: This is not just about type checks anymore - rename.

-include("errvals.hrl").

-define(STRING_SIZE_LIMIT, 8192).

%%% Guards

-define(is_fn(X),      % Is atom a function name?
        (is_atom(X) andalso X =/= true andalso X =/= false)).

-define(is_funcall(X), % Is list a function call?
        (is_list(X) andalso X =/= [] andalso ?is_fn(hd(X)))).

-define(is_array(A),
        (is_tuple(A) andalso (element(1, A) == array))).

-define(is_range(A),
        (is_tuple(A) andalso (element(1, A) == range))).

-define(is_area(A),
        (?is_array(A) orelse ?is_range(A))).

-define(is_string(X),
        (( is_list(X) andalso (not(?is_funcall(X))) )
        orelse
        ( is_tuple(X) andalso element(1, X) == ustring))
       ).

-define(is_cellref(X),
        element(1, X) == cellref).

-define(is_rangeref(X),
        (is_tuple(X) andalso element(1, X) == rangeref)).

-define(is_zcellref(X),
        element(1, X) == zcellref).

-define(is_zrangeref(X),
        (is_tuple(X) andalso element(1, X) == zrangeref)).

-define(is_namedexpr(X),
        element(1, X) == namedexpr).

-define(is_errval(X),
        X == ?ERRVAL_NULL  orelse X == ?ERRVAL_DIV     orelse
        X == ?ERRVAL_VAL   orelse X == ?ERRVAL_REF     orelse
        X == ?ERRVAL_NAME  orelse X == ?ERRVAL_NUM     orelse
        X == ?ERRVAL_NA    orelse X == ?ERRVAL_CIRCREF orelse
        X == ?ERRVAL_AUTH  orelse X == ?ERRVAL_FORM    orelse
        X == ?ERRVAL_MOCHI). % THE MOCHI ERRVAL IS A BUG AND SHOULD BE REMOVED!

-define(is_operator(X),
        X == '+'; X == '*'; X == '/'; X == '-'; X == '^';
        X == '>'; X == '>='; X == '<'; X == '<='; X == '<>';
        X == '&'; X == '^^').

-define(is_blank(X), X == blank).

-define(is_date(X), is_record(X, datetime)).

%%% Collectors

%% implicit iteration when not in array context :(
%% a call to this macro has to be added to every single-value collector (e.g. ?number, ?bool &c)
-define(iinaa(X, Rules, Collector),
        if ?is_array(X) -> Collector(element(2, area_util:at(1, 1, X)), Rules);
           ?is_range(X) -> ?ERR_VAL;
           true         -> Collector(X, Rules)
        end).

-define(number(X, Rules),
        ?iinaa(X, Rules, fun(A_, B_) -> muin_col_DEPR:collect_number(A_, B_) end)).

-define(int(X, Rules),
        erlang:trunc(?number(X, Rules))).

-define(ints(Xs, Rules),
        lists:map(fun(X) -> erlang:trunc(X) end,
                  muin_col_DEPR:collect_numbers(Xs, Rules))).

-define(bool(X, Rules),
        ?iinaa(X, Rules, fun(A_, B_) -> muin_col_DEPR:collect_bool(A_, B_) end)).

-define(edate(X),
        if is_record(X, datetime) -> ok;
           true -> ?ERR_VAL
        end).

-define(utf8l(Ustr),
        ustring:to_utf8(Ustr)).

%% Ensure X is a string.
%% Using element() rather than pattern matching to avoid creating new names
%% in the current scope.
-define(estring(X),
        if element(1, X) == ustring andalso is_binary(element(2, X)) -> X; % STR
           is_list(X) -> X; % STR
           true -> ?ERR_VAL
        end).

%% Ensure Xs are all strings.
-define(estrings(Xs),
        foreach(fun(X) -> ?estring(X) end, Xs)).

-define(ensure_string_under_limit(Str),
        (fun() ->
                 Len = string:len(Str),
                 case Len < ?STRING_SIZE_LIMIT of
                     true -> Str;
                     false -> ?ERR_VAL
                 end
         end)()).
