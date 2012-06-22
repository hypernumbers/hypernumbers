%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Hypernumbers Ltd
%%% @doc
%%%
%%% @end
%%% Created :  27 May 2011 by gordon@hypernumbers.com

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
