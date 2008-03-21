
-define(STDFUNS,
        [
         {stdfuns_logical,
         [
          eq, neq, lt, gt, lte, gte,
          'if'
         ]},

         {stdfuns_math,
          [
           %% Basic arithmetics
           plus,
           minus,
           times,
           divide,
           negate,
         
           %% Arithmetic
           sum,
           product,
           average,
           quotient,
           abs,
           sqrt,
           power,
           sign,
           exp,
           fact,
           %%gcd,
           %%lcm,
           mod,

           %% Arrays and matrices
           mdeterm,
           minverse,
           mmult,
           %%multinom,

           %% Logarithms
           ln,
           log,
           log10,

           %% Random numbers
           rand,
           randbetween,

           %% Rounding numbers
           round,
           rounddown,
           roundup,
           ceiling,
           combin,
           even,
           floor,
           int,
           mround,
           odd,
           trunc,

           %% Special numbers
           pi,
           sqrtpi,
           roman,

           %% Summation
           %%seriessum,
           %%subtotal,
           %%sumif,
           %%sumproduct,
           %%sumsq,
           %%sumx2my2,
           %%sumx2py2,
           %%sumxmy2,

           %% Trigonometry
           sin,
           cos,
           tan,
           asin,
           acos,
           atan,
           atan2,
           sinh,
           cosh,
           tanh,
           asinh,
           acosh,
           atanh,
           degrees,
           radian,
          ]},

         {stdfuns_text,
          [
           %% Comparison functions.
           exact,
           len,
           mid,
           
           %% Conversion functions.
           clean,
           fixed,
           lower,
           proper,
           upper,
           
           %% Locate text.
           char,
           find,
           left,
           right,
           
           concatenate,
           rept           
          ]}
        ]).
