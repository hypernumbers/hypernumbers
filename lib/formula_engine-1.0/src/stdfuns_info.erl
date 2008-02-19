-module(stdfuns_info).
-export([
         error_type/1
        ]).

error_type({error, null})  -> 1;
error_type({error, div0})  -> 2;
error_type({error, value}) -> 3;
error_type({error, ref})   -> 4;
error_type({error, name})  -> 5;
error_type({error, num})   -> 6;
error_type({error, na})    -> 7;
error_type(_)              -> {error, na}.
