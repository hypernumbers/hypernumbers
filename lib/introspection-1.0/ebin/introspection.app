{application, introspection,
 [{description,  "An app to help introspect the system at runtime."},
  {vsn,          "1.0"},
  {modules,      [introspection_app, introspection_sup, introspection_srv]},
  {registered,   [introspection_srv, introspection_sup]},
  {applications, [kernel, stdlib]},
  {mod,          {introspection_app, []}}
 ]}.
