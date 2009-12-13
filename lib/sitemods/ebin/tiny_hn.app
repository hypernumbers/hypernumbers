{application, tiny_hn,
	[{description,  "creates tiny sites"},
	 {vsn,          "1.0"},     
	 {modules,      []},
	 {registered,   [ ]},   
	 {included_applications, []},   
	 {applications, [kernel, stdlib, crypto, inets, mnesia, hypernumbers]},
	 {mod,		{tiny_hn_app, [ ]}}]}.
