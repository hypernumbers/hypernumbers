%% Mostly for shorthand functions

-define(log(Value),
        error_logger:info_msg("~p~n", [Value])).
