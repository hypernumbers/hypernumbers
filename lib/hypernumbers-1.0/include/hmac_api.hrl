%%% @copyright (C) 2009-2014, Hypernumbers Ltd.
%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
-author("Hypernumbers Ltd <gordon@hypernumbers.com>").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Default values for defining a generic API                                %%%
%%%                                                                          %%%
%%% Only change these if you alter the canonicalisation                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(schema, "MOCHIAPI").
-define(headerprefix, $x,$-,$m,$o,$c,$h,$i,$a,$p,$i,$-).
-define(dateheader, "x-mochiapi-date").

%-define(publickey,  "0PN5J17HBGZHT7JJ3X82").
%-define(privatekey, "uV3F3YluFJax1cknvbcGwgjvx4QpvB+leU8dUj2o").

-record(hmac_signature,
        {
          method,
          contentmd5,
          contenttype,
          date,
          headers,
          resource
         }).
