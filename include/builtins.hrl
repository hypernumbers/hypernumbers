
-define(STDFUNS,
        [
         {stdfuns_math,
          [
           plus, minus, times, divide, negate, sum, product, average,
           quotient, abs, sqrt, power, sign, exp, fact, mod, mdeterm,
           minverse, mmult, ln, log, log10, rand, randbetween, round,
           rounddown, roundup, ceiling, combin, even, floor, int, mround,
           odd, trunc, pi, sqrtpi, roman, sin, cos, tan, asin, acos, atan,
           atan2, sinh, cosh, tanh, asinh, acosh, atanh, degrees, radian
          ]}, 

         {stdfuns_text,
          [
           exact, len, mid, clean, fixed, lower, proper, upper, char, find,
           left, right, concatenate, rept
          ]},

         {stdfuns_logical,
          [
           eq, neq, lt, gt, lte, gte, 'if', 'and', 'not', 'or'
          ]},

         {stdfuns_stats,
          [
           avedev, average, averagea, binomdist, chidist, exp, expondist
          ]},

         {stdfuns_info,
          [
           error_type, iserr, iserror, iseven, islogical, isna, isnontext,
           isnumber, isodd, istext, n, na, type
          ]}
        ]).
