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
	 read_excel_file/1,
	 equal_to_digit/3,
	 excel_equal/2,
	 wait/0,
	 wait/1,
         test_state/1,
         make_float/1
	]).

-include("excel_errors.hrl").

%% scratch utility for running from the shell
-export([scratch_Arg/0]).

-define(FILEDIR, "../../../../excel_files/").
-define(EXCEL_IMPORT_FLOAT_PRECISION, 13).
-define(DEFAULT,1000000).

test_state(State)->
  receive
    {msg,Pid,_Suite,{Sheet,Row,Col}} -> Pid ! read_from_excel_data(State,{Sheet,Row,Col});
    {die}                            -> exit(die);
    Other                            -> io:format("message means nothing in test_util:state ~p~n",
                                           [Other])
  end,
  test_state(State).
  
read_from_excel_data(State,{Sheet,Row,Col})->
  Key={{sheet,Sheet},{row_index,Row},{col_index,Col}},
  Return=lists:keysearch(Key, 1, State),
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
%%
%% uses the mochinum library to do so
%%equal_to_digit(F1,F2,DigitIdx) ->
%%  A=mochinum:digits(F1),
%%  B=mochinum:digits(F2),
%%  io:format("in test_util:equal_to_digit A is ~p and B is ~p~n",[A,B]),
%%  A==B.

equal_to_digit(F1,F2,DigitIdx) ->
  %% force any rogue integers to floats  
  F1a=float(F1),
  F2a=float(F2),
  [As0,Bs0]=io_lib:fwrite("~.*f~.*f",[DigitIdx+1,F1a-erlang:trunc(F1a), 
                                       DigitIdx+1,F2a-erlang:trunc(F2a)]),
    As=string:substr(As0,1,DigitIdx+2), 
    Bs=string:substr(Bs0,1,DigitIdx+2),
    As==Bs.

excel_equal("-2146826281","#DIV/0!") -> true;
excel_equal("-2146826246","#N/A")    -> true;
excel_equal("-2146826259","#NAME?")  -> true;
excel_equal("-2146826288","#NULL!")  -> true;
excel_equal("-2146826252","#NUM!")   -> true;
excel_equal("-2146826265","#REF!")   -> true;
excel_equal("-2146826273","#VALUE!") -> true;
%% Checks that two Excel values are equal.
excel_equal(String1,String2) when is_list(String1), is_list(String2) ->
  %% fix-up the fact that we have changed the name of the function Error.Type to ErrorType
    Return=regexp:gsub(String2,"ERROR.TYPE","ERRORTYPE"),
    io:format("Return is ~p~n",[Return]),
    {ok,String2a,_}=Return,
    io:format("String1 is ~p and String2a is ~p~n",[String1,String2a]),
    Result = case String1 of
            String2a -> true;
            _        -> false
      end,
    %% if the strings aren't the same try and make numbers of them and compare then
    case Result of
      true  -> true;
      false ->
        String1f=make_float(String1),
        String2f=make_float(String2),
        case {String1f,String2f} of
          {"not float","not float"} -> false;
          {"not float",_}           -> false;
          {_          ,"not float"} -> false;
          _                          -> equal_to_digit(String1f,String2f,
                                            ?EXCEL_IMPORT_FLOAT_PRECISION)
        end
    end.
        
excel_equal2({number, F1}, {number, F2}) ->
    equal_to_digit(F1, F2, ?EXCEL_IMPORT_FLOAT_PRECISION);
excel_equal2({formula, Fla1}, {formula, Fla2}) ->
  %% fix-up the fact that we have changed the name of the function Error.Type to ErrorType
    Return=regexp:gsub(Fla2,"ERROR.TYPE","ERRORTYPE"),
    io:format("Return is ~p~n",[Return]),
    {ok,Fla2a,_}=Return,
    io:format("Fla1 is  ~p~nFla2a is ~p~n",[Fla1,Fla2a]),
    case Fla1 of
      Fla2a -> true;
      _     -> false
    end;
excel_equal2({boolean,Boolean1},{boolean,Boolean2}) ->
    case Boolean1 of
        Boolean2 -> true;
        _        -> false
    end;
excel_equal2({error,Error1},{number,ErrorVal}) ->
    Error2=make_err_val(ErrorVal),
    case Error1 of
        Error2 -> true;
        _      -> false
    end;
excel_equal2({error,Error1},{error,Error2}) ->
    case Error1 of
        Error2 -> true;
        _      -> false
    end;
excel_equal2({string,String1},{string,String2}) ->
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
            io:format("SUCCESS~nExpected: ~p~nGot:       ~p~n",[Expected,Got]),
            {test, ok};
        _Else ->
            exit({"E:", Expected, "G:", Got})
    end.

expected2(Expected, Got) ->
    Result=excel_equal2(Expected,Got),
    io:format("in test_util:expected2 Result is ~p~n",[Result]),
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
    filefilters:read(excel,?FILEDIR++Filename,fun dump_cells/1).

make_float(List) ->
    Return = try
                list_to_float(List)
              catch
                exit:_Reason   -> "not float";
                error:_Message -> "not float";
                throw:_Term     -> "not float"
            end,
    case Return of
      "not float" -> make_float2(List);
      _           -> Return
    end.

make_float2(List)->
    try
      list_to_integer(List)*1.0
    catch
      exit:_Reason   -> "not float";
      error:_Message -> "not float";
      throw:_Term     -> "not float"
    end.


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
