Generic = {};
Generic.reload = function () {
	$("ul.hn_sld_menu").parent().css({"overflow": "visible", 'padding' : 0} );
	$("hr.hn_sld_hr, div.hn_sld_vertline").hide().fadeIn(450);
	$("hr.hn_sld_hr, div.hn_sld_alert, div.hn_sld_plainbox, div.hn_sld_ruledbox, div.hn_sld_box").parent().css("padding",  0);
	$("h3.hn_sld_headline").parent().css("text-align","left");
	$("div.hn_sld_plainbox").parent().css("text-align","left");
	
	// div.hn_sld_plainbox, div.hn_sld_ruledbox, div.hn_sld_box")
	
	if ($("ul.hn_sld_menu").parent().parent().hasClass("hn_inner")) {
		$("ul.hn_sld_menu").parent().parent().parent().css({"overflow": "visible", 'padding' : 0} );
	}
	$("ul.hn_sld_menu").parent().css({"overflow": "visible", 'padding' : 0} );
	$("a img").parent().css("border-bottom", "none");

	$("#hn_sld_tabs").tabs();
	$("#hn_sld_tabs").parent().css({'padding' : 0} );
//if (/MSIE (\d+\.\d+);/.test(navigator.userAgent)){ //test for MSIE x.x;
 //var ieversion=new Number(RegExp.$1) // capture x.x portion and store as a number
// if (ieversion==7)
 // document.write("Internet 7 user! Do some extra styling")
//}

};

