%% Additions to this file must be reflected in 2 other places:
%% typechecks.hrl and the functions 'error.type', 'iserr' and 'iserror' in stdfuns_info
%% Literal error values.
-define(ERRVAL_NULL,    {errval, '#NULL!'}).
-define(ERRVAL_DIV,     {errval, '#DIV/0!'}).
-define(ERRVAL_VAL,     {errval, '#VALUE!'}).
-define(ERRVAL_REF,     {errval, '#REF!'}).
-define(ERRVAL_NAME,    {errval, '#NAME?'}).
-define(ERRVAL_NUM,     {errval, '#NUM!'}).
-define(ERRVAL_NA,      {errval, '#N/A'}).
-define(ERRVAL_CIRCREF, {errval, '#CIRCREF!'}).
-define(ERRVAL_AUTH,    {errval, '#AUTH!'}).
-define(ERRVAL_FORM,    {errval, '#ERROR!'}).
-define(ERRVAL_MOCHI,   {errval, '#MOCHIJSON!'}). % THIS IS A TEMPORARY ERROR MSG AND MUST BE MADE TO GO AWAY! ITS A BUG!!

%% Return an error.
-define(ERR_NULL,    throw(?ERRVAL_NULL)).
-define(ERR_DIV,     throw(?ERRVAL_DIV)).
-define(ERR_VAL,     throw(?ERRVAL_VAL)).
-define(ERR_REF,     throw(?ERRVAL_REF)).
-define(ERR_NAME,    throw(?ERRVAL_NAME)).
-define(ERR_NUM,     throw(?ERRVAL_NUM)).
-define(ERR_NA,      throw(?ERRVAL_NA)).
-define(ERR_CIRCREF, throw(?ERRVAL_CIRCREF)).
-define(ERR_AUTH,    throw(?ERRVAL_AUTH)).
-define(ERR_FORM,    throw(?ERRVAL_FORM)).
-define(ERR_MOCHI,   throw(?ERRVAL_MOCHI)).
