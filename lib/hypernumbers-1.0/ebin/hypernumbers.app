{application, hypernumbers,
	[{description,  "The engine application"},
	 {vsn,          "1.0"},     
	 {modules,      [hn_yaws]},
	 {registered,   [ ]},   
	 {included_applications, []},   
	 {applications, [kernel, stdlib, crypto, inets, mnesia]},
	 {mod,		{hypernumbers_app, [ ]}}]}.
