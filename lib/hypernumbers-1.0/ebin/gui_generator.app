{application, gui_generator,
 [{description,  "An app to generate the guis automatically."},
  {vsn,          "1.0"},
  {modules,      [gui_generator_app, gui_generator_sup, gui_generator_srv]},
  {registered,   [gui_generator_srv, gui_generator_sup]},
  {applications, [kernel, stdlib]},
  {mod,          {gui_generator_app, []}}
 ]}.
