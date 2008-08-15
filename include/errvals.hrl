
%% Literal error values.
-define(ERRVAL_NULL, {errval, '#NULL!'}).
-define(ERRVAL_DIV,  {errval, '#DIV/0!'}).
-define(ERRVAL_VAL,  {errval, '#VALUE!'}).
-define(ERRVAL_REF,  {errval, '#REF!'}).
-define(ERRVAL_NAME, {errval, '#NAME?'}).
-define(ERRVAL_NUM,  {errval, '#NUM!'}).
-define(ERRVAL_NA,   {errval, '#N/A'}).

%% -define(ERRVAL_NULL, {errval, null}).
%% -define(ERRVAL_DIV,  {errval, div0}).
%% -define(ERRVAL_VAL,  {errval, value}).
%% -define(ERRVAL_REF,  {errval, ref}).
%% -define(ERRVAL_NAME, {errval, name}).
%% -define(ERRVAL_NUM,  {errval, num}).
%% -define(ERRVAL_NA,   {errval, na}).

%% Return an error.
-define(ERR_NULL, throw(?ERRVAL_NULL)).
-define(ERR_DIV,  throw(?ERRVAL_DIV)).
-define(ERR_VAL,  throw(?ERRVAL_VAL)).
-define(ERR_REF,  throw(?ERRVAL_REF)).
-define(ERR_NAME, throw(?ERRVAL_NAME)).
-define(ERR_NUM,  throw(?ERRVAL_NUM)).
-define(ERR_NA,   throw(?ERRVAL_NA)).
