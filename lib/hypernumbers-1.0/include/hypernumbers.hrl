%% Anything to string
-define(STR(X),lists:flatten(io_lib:format("~p",[X]))).
%% Formatted String
-define(FORMAT(X,Y),lists:flatten(io_lib:format(X,Y))).
%% Generate SASL info message
-define(INFO(X,Y),error_logger:info_msg(X++"~n",Y)).
-define(ERROR(X,Y),error_logger:error_msg(X++"~n",Y)).
