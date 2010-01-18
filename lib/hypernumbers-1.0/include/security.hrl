-record(binding,
        {
          type = invalid,
          from = [],
          to   = []
         }).

-type transaction() :: [#binding{}].
-type security() :: [transaction()].
