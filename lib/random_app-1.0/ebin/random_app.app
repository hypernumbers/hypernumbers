{application, random_app,
	[{description,  "A shared random number generator"},
	 {vsn,          "1.0"},
	 {modules,      [random_sup,
			 random_srv]},
	 {registered,   [ ]},
	 {included_applications, []},
	 {applications, [kernel, stdlib]},
	 {mod,		{random_app, [ ]}}]}.
