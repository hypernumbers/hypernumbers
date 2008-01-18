%% These constants are all defined in the Open Office Microsoft Compound
%% File Description Document

%% It can be found at:
%% http://sc.openoffice.org/compdocfileformat.pdf

%% Each constant is commented by the binary signature that it ought
%% to have when used in bit matching...

%% Defined in Section 4.1
-define(HEADER_SIZE, 512).    % Size in bytes

%% Defined in Section 3.1
-define(FREE_SID, -1).        % :32/little-signed-integer
-define(END_OF_CHAIN_SID,-2). % :32/little-signed-integer
-define(SAT_SID,-3).          % :32/little-signed-integer 
-define(MSAT_SID,-4).         % :32/little-signed-integer

%% Defined in Section 4.1       :64/little-signed-integer
-define(BIFF8_MAGIC_NUMBER, -2226271756974174256). % D0 CF 11 E0 A1 B1 1A E1 

