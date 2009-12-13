{application, hypernumbers,
	[{description,  "The engine application"},
	 {vsn,          "1.0"},     
	 {modules,      []},
	 {registered,   [ ]},   
	 {included_applications, []},   
	 {applications, [kernel, stdlib, crypto, inets, mnesia, ssl]},
	 {mod,		{hypernumbers_app, [ ]}}]}.
