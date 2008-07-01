%%%----------------------------------------------------------------------------
%%% @doc Date functions.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%%----------------------------------------------------------------------------

-module(stdfuns_date).
-export([date/1,
         datedif/1,
         datevalue/1,
         day/1,
         month/1,
         year/1]).
         
-include("typechecks.hrl").
-include("handy_macros.hrl").
-include("muin_records.hrl").

-define(else, true). % silly hack for ifs

date(Arg = [_, _, _]) ->
    [Year, Month, Day] = ?numbers(Arg, [cast_strings,
                                        cast_bools,
                                        zero_blanks]),
    #datetime{date = {Year, Month, Day}}.

datedif([V1, V2, V3]) ->
    [Start, End] = ?dates([V1, V2], [collect_strings]),
    ?ensure(muin_date:gt(End, Start), ?ERR_NUM),
    ?estring(V3),
    Unit = string:to_upper(?utf8l(V3)),
    datedif1(Start, End, Unit).
datedif1(Start, End, "Y") ->
    #datetime{date = {Startyr, Startmo, Startday}} = Start,
    #datetime{date = {Endyr, Endmo, Endday}} = End,
    Maxyrs = Endyr - Startyr,
    if Endmo > Startmo ->
            Maxyrs;
       Endmo == Startmo ->
            if Endday >= Startday ->
                    Maxyrs;
               ?else ->
                    Maxyrs - 1
            end;
       ?else ->
            Maxyrs - 1
    end;
datedif1(Start, End, "M") ->
    Startmos = 12 * muin_date:year(Start) + muin_date:month(Start),
    Endmos = 12 * muin_date:year(End) + muin_date:month(End),
    Maxmos = Endmos - Startmos,
    case muin_date:day(End) >= muin_date:day(Start) of
        true -> Maxmos;
        false -> Maxmos - 1
    end;
datedif1(Start, End, "D") ->
    muin_date:dtdiff(Start, End);
datedif1(Start, End, "MD") ->
    Startday = muin_date:day(Start),
    Endday = muin_date:day(End),
    if Endday >= Startday ->
            Endday - Startday;
       ?else ->
            31 - (Startday - Endday)
    end;
datedif1(Start, End, "YM") ->
    Startmo = muin_date:month(Start),
    Endmo = muin_date:month(End),
    if Endmo >= Startmo ->
            Endmo - Startmo;
       ?else ->
            12 - (Startmo - Endmo)
    end;
datedif1(Start, End, "YD") ->
    foldl(fun(X, Acc) ->
                  Acc + calendar:last_day_of_the_month(muin_date:year(Start), X)
          end,
          0,
          seq(muin_date:month(Start), muin_date:month(End))).

datevalue([V1]) ->
    ?date(V1, [collect_strings]).

day([Dt]) ->
    ?edate(Dt),
    muin_date:day(Dt).

month([Dt]) ->
    ?edate(Dt),
    muin_date:month(Dt).

year([Dt]) ->
    ?edate(Dt),
    muin_date:year(Dt).

%%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-include_lib("eunit/include/eunit.hrl").

datedif_test_() ->
    [
     %% Years
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "Y") == 1),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 16}},
                       "Y") == 0),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "Y") == 1),

     %% Months
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "M") == 12),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 16}},
                       "M") == 11),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "M") == 12),

     %% Days
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "D") == 365),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 16}},
                       "D") == 364),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "D") == 366),

     %% Days only
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "MD") == 0),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 16}},
                       "MD") == 30),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "MD") == 1),

     %% Months only
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 17}},
                       "YM") == 0),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 16}},
%%                        "YM") == 11),
     ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
                       #datetime{date = {1987, 8, 18}},
                       "YM") == 0),
     
     %% Days -- years ignorned
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 17}},
%%                        "YD") == 0),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 16}},
%%                        "YD") == 364),
%%      ?_assert(datedif1(#datetime{date = {1986, 8, 17}},
%%                        #datetime{date = {1987, 8, 18}},
%%                        "YD") == 1),

     ?_assert(true)
    ].
