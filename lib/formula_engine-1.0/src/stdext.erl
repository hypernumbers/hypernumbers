%%% @doc Hypernumbers-only standard functions
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

-module(stdext).
-export([
         text2num/1
        ]).
-include("handy_macros.hrl").

%% Convert text representing a number into an actual number.
%% http://ewbi.blogs.com/develops/2007/11/some-simple-val.html
%% http://support.microsoft.com/kb/291047
text2num(Str) when is_list(Str) ->
    try tconv:to_i(Str)
    catch
        error:_ ->
            try tconv:to_f(Str)
            catch
                error:_ ->
                    {error, value}
            end
    end;

text2num(_) ->
    {error, value}.

%% http://ejohn.org/blog/javascript-pretty-date/
pretty_date(Date) ->
    Date.
