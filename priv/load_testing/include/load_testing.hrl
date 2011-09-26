%%% constants for load testing to make it easier to develop

-define(site, "http://load.hypernumbers.dev:9000").
%-define(site, "http://loadtesting.hypernumbers.com:80").

% basic load parameters
-define(no_of_datapages, 20).
-define(no_of_calcpages, 20).
-define(no_of_zquerypages, 20).
-define(dataprefix, "datapages").
-define(datapage, "400datapoints").
-define(calcsprefix, "calcspages").
-define(calcspage, "400calcs").
-define(zquerypage, "400zqueries").
-define(zqueryprefix, "zqueries").
-define(additionaldata, "additionaldata").
-define(no_of_additional_data, 20).
-define(additionalcalcs, "additionalcalcs").
-define(no_of_additional_calcs, 20).
-define(addzs_prefix, "additionalzs").
-define(no_of_addzpages, 1).
-define(addz_template, "minizs").
-define(bulkpage, "onecell").
-define(pageload, 2). % creates N^4 pages

% some params for z_tests
-define(z_testpage, "400datapoints").
-define(z_test_loadpage, "400zqueries").
-define(z_testprefix, "z_tests").
-define(z_test_loadprefix, "z_load").
-define(no_of_z_testpages, 100).

% now the number times the tests have to run
-define(no_of_deletes, 25).
-define(no_of_recalcs, 25).
-define(no_of_forcedzs, 25).

% define the number of times indivual profiles have to run
-define(no_of_zquery_profiles, 500).
-define(zquery_profile_page, "zquery_profile").
