%%% constants for load testing to make it easier to develop

%-define(site, "http://load.hypernumbers.dev:9000").
-define(site, "http://loadtesting.hypernumbers.com:80").

% basic load parameters
-define(no_of_datapages, 100).
-define(no_of_calcpages, 100).
-define(no_of_zquerypages, 100).
-define(dataprefix, "datapages").
-define(datapage, "400datapoints").
-define(calcsprefix, "calcspages").
-define(calcspage, "400calcs").
-define(zqueryprefix, "zquerypages").
-define(zquerypage, "400zqueries").
-define(bulkpage, "onecell").
-define(pageload, 10). % creates N^4 pages

% now the number times the tests have to run
-define(no_of_deletes, 25).
-define(no_of_recalcs, 25).
-define(no_of_forcedzs, 25).

% define the number of times indivual profiles have to run
-define(no_of_zquery_profiles, 500).
-define(zquery_profile_page, "zquery_profile").
