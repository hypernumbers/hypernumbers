-author("Hypernumbers Ltd <gordon@hypernumbers.com>").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Default values for defining a generic API                                %%%
%%%                                                                          %%%
%%% Only change these if you alter the canonicalisation                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(schema, "MOCHIAPI").
-define(headerprefix, $x,$-,$m,$o,$c,$h,$i,$w,$a,$p,$i,$-).
-define(dateheader, "x-mochiapi-date").

-define(publickey,  "0PN5J17HBGZHT7JJ3X82").
-define(privatekey, "uV3F3YluFJax1cknvbcGwgjvx4QpvB+leU8dUj2o").

-record(hmac_signature,
        {
          method,
          contentmd5,
          contenttype,
          date,
          headers,
          resource
         }).
