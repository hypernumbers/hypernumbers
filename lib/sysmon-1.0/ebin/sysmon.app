{application, sysmon,
 [{description,  "A systems monitor"},
  {vsn,          "1.0"},
  {modules,      [
                  sysmon_app,
                  sysmon_srv,
                  sysmon_sup,
                  syslib
                 ]},
  {registered,   [ ]},
  {included_applications, []},
  {applications, [kernel, stdlib, crypto, inets, mnesia, ssl]},
  {mod,		{sysmon_app, []}}]}.
