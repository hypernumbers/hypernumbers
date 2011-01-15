%%% @doc Various records used in Muin.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>

%% Runtime information: describes the context in which to run a formula.
-record(muin_rti, {
          site, path, col, row,   % target cell
          array_context = false,  % array formula?
          auth_req = nil
         }).


%% DateTime: fields are in the format expected by functions in the
%% calendar module in stdlib.
-record(datetime, {
          date = {1, 1, 1},   % year, month, day
          time = {0, 0, 0}    % hour, minute, second
        }).

%% col & row are either integers, or {offset, integer()}
-record(cellref, {
          col, row,       % pos_integer()|{offset, pos_integer()}
          path,           % string like "./" or "/foo/bar"
          text = ""       % token text (used in the parser for function names like ATAN2)
         }).

-record(zcellref, {
          zpath,                % a z-order path
          cellref = #cellref{}  % a standard #cellref{} record
         }).

%% super_util actually depends on rangeref order!!!
-record(rangeref, {
          type,          % finite|col|row
          path,          % like in #cellref{}
          tl, br,        % {Col, Row} | {col|row, pos_integer()|{offset, pos_integer()}}
          width, height, % pos_integer()|na
          text = ""      % token text
         }).

-record(zrangeref, {
          zpath,                 % a z-order path
          rangeref = #rangeref{} % a standard #rangeref{} record
         }).

-record(namedexpr, {
          path,
          text
         }).
