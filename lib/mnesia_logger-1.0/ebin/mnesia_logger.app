{application, mnesia_logger,
	[{description,  "An Mnesia Logger"},
	 {vsn,          "1.0"},     
	 {modules,      []},
	 {registered,   [ ]},   
	 {included_applications, []},   
	 {applications, [kernel, stdlib, crypto, inets, mnesia]},
	 {mod,		{mnesia_logger_app, [ ]}}]}.
