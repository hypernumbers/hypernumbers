HN.NewWebComponents = {};

// Tabs only work if there are any - if there aint it fails silently
HN.NewWebComponents.reload_tabs = function () {

    // Tabs
    var tabs = $(".hn_sld_tabs"),
    i, id;
    for (i = 0; i < tabs.length; i = i + 1) {
        id = $(tabs[i]).attr("id");
	      $("#" + id).tabs();
	      $("#" + id).parent().css({'padding' : 0} );    
    }
    
}

HN.NewWebComponents.reload = function () {

    // Menus
	  $("ul.hn_sld_menu").parent().css({"overflow": "visible", 'padding' : 0});
	  if ($("ul.hn_sld_menu").parent().parent().hasClass("hn_inner")) {
		    $("ul.hn_sld_menu").parent().parent().parent().css({"overflow": "visible", 'padding' : 0} );
	  }

    // Vertical lines
	  $("hr.hn_sld_hr, div.hn_sld_vertline").hide().fadeIn(450);
	  $("hr.hn_sld_hr").parent().css("padding",  0);

    // Boxes
	  $("div.hn_sld_alert, div.hn_sld_plainbox, div.hn_sld_ruledbox, div.hn_sld_box").parent().css("padding",  0);
    
    // take alignment under advisance
	  //$("div.hn_sld_plainbox").parent().css("text-align","left");

    // Headlines
	  $("h3.hn_sld_headline").parent().css("text-align","left");
	  
	  $("a img").parent().css("border-bottom", "none");
    
};

