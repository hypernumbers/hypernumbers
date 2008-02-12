%%%-------------------------------------------------------------------
%%% File        : test_util.erl
%%% Author      : Gordon Guthrie <gordonguthrie@localhost>
%%% Description : 
%%%
%%% Created     : 15 Jun 2007 by Gordon Guthrie <gordonguthrie@localhost>
%%%-------------------------------------------------------------------
-module(test_util).

%% public exports
-export([
	 expected/2,
         expected2/2,
	 get_Arg/4,
	 read_excel_file/1,
         read_from_excel_data/3,
	 equal_to_digit/3,
	 excel_equal/2,
	 wait/0,
	 wait/1
	]).

-include("excel_errors.hrl").

%% scratch utility for running from the shell
-export([scratch_Arg/0]).

-define(FILEDIR, "../../../../excel_files/").
-define(EXCEL_IMPORT_FLOAT_PRECISION, 15).
-define(DEFAULT,1000000).


read_from_excel_data(Config,Suite,{Sheet,Row,Col})->
  io:format("Got to 1 Sheet is ~p Row is ~p and Col is ~p~n",[Sheet,Row,Col]),
  {value, Result} = lists:keysearch(Suite, 1, Config),
  Data = element(2, Result),
  Key={{sheet,Sheet},{row_index,Row},{col_index,Col}},
  Return=lists:keysearch(Key, 1, Data),
  {value, Result2}=Return,
  El=element(2, Result2),
  case El of
      {value, number, Number} -> {number,Number};
      {string,String}         -> {string,String};
      {formula,Formula}       -> {formula,Formula};
      {value,boolean,Boolean} -> {boolean,Boolean};
      {value,error,Error}     -> {error, Error};
      Other                   -> io:format("(in generatetest.rb - fix me Other is ~p~n",[Other])
end.


%% Checks that two floats are exactly the same up to
%% a certain number of decimal places.
equal_to_digit(F1,F2,DigitIdx) ->
  %% force any rogue integers to floats
  F1a=float(F1),
  F2a=float(F2),
  [As0,Bs0]=io_lib:fwrite("~.*f~.*f",[DigitIdx+1,F1a-erlang:trunc(F1a), 
                                        DigitIdx+1,F2a-erlang:trunc(F2a)]),
    As=string:substr(As0,1,DigitIdx+2), 
    Bs=string:substr(Bs0,1,DigitIdx+2),
    As==Bs.

%% Checks that two Excel values are equal.
excel_equal({number, F1}, {number, F2}) ->
    equal_to_digit(F1, F2, ?EXCEL_IMPORT_FLOAT_PRECISION);
excel_equal({formula, Fla1}, {formula, Fla2}) ->
    Fla1 == Fla2;
excel_equal({boolean,Boolean1},{boolean,Boolean2}) ->
    case Boolean1 of
        Boolean2 -> true;
        _        -> false
    end;
excel_equal({error,Error1},{number,ErrorVal}) ->
    Error2=make_err_val(ErrorVal),
    case Error1 of
        Error2 -> true;
        _    -> false
    end;
excel_equal({error,Error1},{error,Error2}) ->
    case Error1 of
        Error2 -> true;
        _    -> false
    end;
excel_equal({string,String1},{string,String2}) ->
    case String1 of
        String2 -> true;
        _       -> false
    end.

make_err_val(?ErrDiv0Int)  -> "#DIV/0!";
make_err_val(?ErrNAInt)    -> "#N/A";
make_err_val(?ErrNameInt)  -> "#NAME?";
make_err_val(?ErrNullInt)  -> "#NULL!";
make_err_val(?ErrNumInt)   -> "#NUM!";
make_err_val(?ErrRefInt)   -> "#REF!";
make_err_val(?ErrValueInt) -> "#VALUE!";
make_err_val(X)            -> X.

expected(Expected, Got) ->
    case Got of
        Expected ->
            io:format("SUCCESS~nExpected: ~p~nGot:     ~p~n",[Expected,Got]),
            {test, ok};
        _Else ->
            exit({"E:", Expected, "G:", Got})
    end.

expected2(Expected, Got) ->
    Result=excel_equal(Expected,Got),
    case Result of
        true ->
            io:format("SUCCESS~nExpected: ~p~nGot:     ~p~n",[Expected,Got]),
            {test, ok};
        false ->
                %%case file:open("C:/tmp/fails.txt", [append]) of
                %%  {ok, Id} ->
                %%    io:fwrite(Id, "E:~p G:~p~n", [Expected,Got]),
                %%    file:close(Id);
                %%_ ->
                %%  exit("file open error"),
                %%  error
                %%end,
            exit({fail, expected, Expected, got, Got})
    end.

%% Reads an Excel file under SVNROOT/testroot/excel_import_test/files
read_excel_file(Filename) ->
    filefilters:read(excel,?FILEDIR++Filename,"test-read_file.log", 
                     fun dump_cells/1).

get_Arg(IP,DomainAndPort,PathAndRef,FormatAndDecos)->
    Port=open_port({spawn,bash},[]),% get a spoof port
    port_close(Port),
    {arg,
     Port,
     {IP,60091},
     {headers,
      "keep-alive",
      "text/xml,application/xml,application/xhtml+xml,text/html;"++
      "q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5",
      DomainAndPort,
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      "Mozilla/5.0 (X11; U; Linux i686; en-GB; rv:1.8.0.5) "++
      "Gecko/20060731 Ubuntu/dapper-security Firefox/1.5.0.5",
      undefined,
      [],
      "300",
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      undefined,
      [{http_header,
	1,
	'Cache-Control',
	undefined,
	"max-age=0"},
       {http_header,
	9,
	'Accept-Charset',
	undefined,
	"ISO-8859-1,utf-8;q=0.7,*;q=0.7"},
       {http_header,
	10,
	'Accept-Encoding',
	undefined,
	"gzip,deflate"},
       {http_header,
	11,
	'Accept-Language',
	undefined,
	"en-gb,en;q=0.5"}]},
     {http_request,
      'GET',
      {abs_path,PathAndRef++"?"++FormatAndDecos},
      {1,1}},
     undefined,
     PathAndRef,
     FormatAndDecos,
     PathAndRef,
     "/opt/SVN/spriki/trunk/include/docroot",
     "/",
     "/opt/SVN/spriki/trunk/include/docroot"++PathAndRef,
     undefined,
     undefined,
     self(),
     [],
     [],
     [],
     PathAndRef}.

%% Default operation wait time, used for the test suites.
wait() ->
    internal_wait(?DEFAULT).

wait(N) ->
    internal_wait(?DEFAULT * N).

%%------------------------------------------------------------------------------
%% Just a helper functions
%%------------------------------------------------------------------------------
scratch_Arg()->
    Protocol="http://",
    Domain="127.0.0.1",
    IP={127,0,0,1},
    Port="9000",
    Path="/xxx/",
    Ref="a1",
    FormatAndDecos="hypernumber&format=xml",
    Site=Protocol++Domain++":"++Port,
    test_util:get_Arg(IP,Site,Path++Ref,FormatAndDecos).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% Cleans up the response from the excel spreadsheet file reader.
dump_cells(Tables) ->
    {value,{cell,Tid}}=lists:keysearch(cell,1,Tables),   
    io:format("Dumping Cell table"),
    Fun=fun(X,Y)->[X|Y] end,
    Cells=ets:foldl(Fun,[],Tid),
    Transform=fun({Index,[_,Body]}) -> {Index,Body}
               end,
     lists:map(Transform,Cells).

internal_wait(0) ->
    ok;
internal_wait(N) ->
    internal_wait(N-1).
