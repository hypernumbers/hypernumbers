%%% @author    Gordon Guthrie
%%% @copyright (C) 2011-2014, Hypernumbers Ltd
%%% @doc        Salts for the various cryptographic routines
%%%
%%% @end
%%% Created :  27 May 2011 by gordon@hypernumbers.com

%%% NOTE THIS MODULE IS UNDER THE GPL V3.0
%%% YOU DO NOT HAVE TO DISTRIBUTE YOUR CRYPTOGRAPHIC SALTS

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(salts).

-export([
         ivector/0,
         server_key/0,
         server_token_key/0,
         randomsalt/0
        ]).

%% ivctor, server_key and server_token_key are used in user management

%% How I generated this.
%% X = crypto:rand_uniform(round(math:pow(2,128)),
%%                         round(math:pow(2,129)-1)),
%% <<X:128>>.
ivector() -> <<121,155,254,177,79,133,224,14,193,76,204,153,223,222,231,143>>.

server_key() -> <<"now I can ollie and I'm not so shite">>.

server_token_key() -> <<"her suntan starts just above the collar">>.

%% random salt is used to create page segements when the function
%% =create.button(...) is used by a user with the 'numbered, random'
%% option
%% http://documentation.hypernumbers.com/contents/hypernumbers-functions/templates/create-button.html
randomsalt() -> "we almost had a baby".
