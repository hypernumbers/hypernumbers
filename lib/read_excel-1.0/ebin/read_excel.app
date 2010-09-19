{application, read_excel,
	[{description,  "reads excel files"},
	 {vsn,          "1.0"},     
	 {modules,      [
                     excel,
                     excel_tokens,
                     read_excel.app,
                     excel_post_process,
                     excel_util,
                     read_excel,
                     excel_records,
                     filefilters,
                     read_excel_sup
                    ]},
	 {registered,   [ ]},   
	 {included_applications, []},   
	 {applications, [kernel, stdlib, crypto, inets, mnesia, ssl]},
	 {mod,		{hypernumbers_app, [ ]}}]}.
