HN.UI = {};

HN.UI.close_dialog = function (layout, id, callback) {

    layout.resumeSelection();

    if (callback) { 
        $("#"+id).fadeOut("fast", callback);        
    } else { 
        $("#"+id).fadeOut("fast");
    }
    $("#cover").fadeOut("fast");
};

var Intro = function() {

    this.current_page = 1;
    this.pages = $("#introwrapper").children().length;

    var t = this;

    $("#intronext").click( function() {
        var id = ( t.current_page == t.pages )
            ? 1 : t.current_page+1;
        t.show_intro_page(id);
    });

    $("#introback").click( function() {
        var id = ( t.current_page == 1 )
            ? t.pages : t.current_page-1;
        t.show_intro_page(id);
    });

    this.show_intro_page = function(x) {
        this.current_page = x;
        HN.Util.id("intropager").innerHTML = "Page "+this.current_page
            +" of "+this.pages+" Pages";
        $("#introwrapper").children().hide();
        $("#introwrapper").children().eq(x-1).show();
    };

    this.show_intro_page(1);
};


$(".mclose").click( function() {
                        HN.UI.close_dialog(layout, $(this).parent().attr("id"));
                    });

Intro();