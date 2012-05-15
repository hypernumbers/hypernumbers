HN.HTMLPanel = {};

HN.HTMLPanel.reload = function() {

  var i, panels = $(".hn_html_panel");

    for (i = 0; i < panels.length; i++) {

        var height, newheight, padding;

        // get the size of the panel, its parent
        // and work out a vertical padding and stuff
        // to make it look snut

        height = $(panels[i]).height();
        newheight = $(panels[i]).parent().height() - 4;
        padding = Math.floor((newheight - height)/2);
        $(panels[i]).css("height", newheight - padding + "px");
        $(panels[i]).css("padding", padding + "px 0 0 0");

        // we overflow the parent so we can show the shadows (if appropriate)
        $(panels[i]).parent().css("overflow", "visible");
        
    }
};