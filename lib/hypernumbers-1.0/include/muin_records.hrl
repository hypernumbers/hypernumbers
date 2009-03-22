%%% @doc Various records used in Muin.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%% Runtime information: describes the context in which to run a formula.
-record(muin_rti, {
          site, path, col, row,   % target cell
          array_context = false   % array formula?
         }).

%% DateTime: fields are in the format expected by functions in the
%% calendar module in stdlib.
-record(datetime, {
          date = {0, 0, 0},   % year, month, day
          time = {0, 0, 0}    % hour, minute, second
        }).

%% col & row are either integers, or {offset, integer()}
-record(cellref, {
          col, row,       % pos_integer()|{offset, pos_integer()}
          path,           % string like "./" or "/foo/bar"
          text = ""       % token text (used in the parser for function names like ATAN2)
         }).

-record(rangeref, {
          path,         % like in #cellref{}
          tl, br        % coords like in #cellref | {col|row, pos_integer()|{offset, pos_integer()}}
         }).

-record(namedexpr, {
          path,
          text
         }).
