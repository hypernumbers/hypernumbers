%%% @copyright (C) 2009-2014, Hypernumbers Ltd.
%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
%% Additions to this file must be reflected in 3 other places:
%% * typechecks.hrl
%% * the functions 'error.type', 'iserr' and 'iserror' in stdfuns_info
%% * xfl_lexer.xrl

%% Literal error values.
-define(ERRVAL_NULL,     {errval, '#NULL!'}).
-define(ERRVAL_DIV,      {errval, '#DIV/0!'}).
-define(ERRVAL_VAL,      {errval, '#VALUE!'}).
-define(ERRVAL_REF,      {errval, '#REF!'}).
-define(ERRVAL_NAME,     {errval, '#NAME?'}).
-define(ERRVAL_NUM,      {errval, '#NUM!'}).
-define(ERRVAL_NA,       {errval, '#N/A'}).
-define(ERRVAL_CIRCREF,  {errval, '#CIRCREF!'}).
-define(ERRVAL_AUTH,     {errval, '#AUTH!'}).
-define(ERRVAL_FORM,     {errval, '#ERROR!'}).
-define(ERRVAL_ERR,      {errval, '#ERROR!'}).
-define(ERRVAL_MOCHI,    {errval, '#MOCHIJSON!'}). % THIS IS A TEMPORARY ERROR MSG AND MUST BE MADE TO GO AWAY! ITS A BUG!!
-define(ERRVAL_CANTINC,  {errval, '#CANTINC!'}).
-define(ERRVAL_PAYONLY,  {errval, '#PAYONLY!'}).
-define(ERRVAL_NOTSETUP, {errval, '#NOTSETUP!'}).

%% Return an error.
-define(ERR_NULL,     throw(?ERRVAL_NULL)).
-define(ERR_DIV,      throw(?ERRVAL_DIV)).
-define(ERR_VAL,      throw(?ERRVAL_VAL)).
-define(ERR_REF,      throw(?ERRVAL_REF)).
-define(ERR_NAME,     throw(?ERRVAL_NAME)).
-define(ERR_NUM,      throw(?ERRVAL_NUM)).
-define(ERR_NA,       throw(?ERRVAL_NA)).
-define(ERR_CIRCREF,  throw(?ERRVAL_CIRCREF)).
-define(ERR_AUTH,     throw(?ERRVAL_AUTH)).
-define(ERR_FORM,     throw(?ERRVAL_FORM)).
-define(ERR_MOCHI,    throw(?ERRVAL_MOCHI)).
-define(ERR_CANTINC,  throw(?ERRVAL_CANTINC)).
-define(ERR_PAYONLY,  throw(?ERRVAL_PAYONLY)).
-define(ERR_NOTSETUP, throw(?ERRVAL_NOTSETUP)).

