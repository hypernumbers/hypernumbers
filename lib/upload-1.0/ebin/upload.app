{application, hypernumbers,
 [{description,  "Uploader modules"},
  {vsn,          "1.0"},     
  {modules,      [
                  sust_adv_upload
                 ]},
  {registered,   [ ]},   
  {included_applications, []},   
  {applications, [kernel, stdlib, crypto, inets, mnesia, ssl]},
  {mod,		{hypernumbers_app, [ ]}}]}.
