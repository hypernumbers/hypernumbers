
-define(STDFUNS,
        [
         {stdfuns_logical,
         [
          eq, neq, lt, gt, lte, gte
         ]},

         {stdfuns_math,
          [
           %% Basic arithmetics 
           plus, minus, times, divide, negate,
           
           %% Arithmetic 
           sum, product, average, quotient, abs, sqrt, power, sign, exp, fact,
           %%gcd, %%lcm,
           mod,
           
           %% Arrays and matrices
           mdeterm, minverse, mmult,
           %%multinomial
           
           %% Logarithms
           ln, log, log10,
           
           %% Random numbers
           rand, randbetween,
           
           %% Rounding numbers
           int
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
