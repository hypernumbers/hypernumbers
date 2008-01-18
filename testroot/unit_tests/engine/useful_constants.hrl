% If these are changed, some test code will need to be changed accordingly (the expected output
% will be different).
% These aren't _really_ constants, but it's easier to refer to the numbers they represent by the
% names, and it also allows to see when edge cases have been covered in relevant unit tests at a 
% glance.

-define(HUGE_INT, 10000000000). % 10^10
-define(HUGE_NEGATIVE_INT, -?HUGE_INT). % -(10^10)
-define(TINY_FLOAT, 0.00001).

% This is just to save some typing. These arrays have a good range of values and include
% positive / negative and int / float values.
-define(ARR1, [0, 0.5, 0.00001, 3.998, 24, 1.7, 21.8761, 491, -90, -21.79, 1.1]).
-define(ARR2, [20, 1, 100.01, 0, 781, 955, 100, -110.78511, -863.009, 1000001, 999.2]).
-define(ARR3, [0, 10000461, 78.91, 86.3, 23, 0, -0.0001, -11.11, 42.42, 27, 40976.99]).
-define(ARR4, [1, 10000461, 78.91, 86.3, 23, 1, 0.0001, 11.11, 42.42, 27, 40976.99]).