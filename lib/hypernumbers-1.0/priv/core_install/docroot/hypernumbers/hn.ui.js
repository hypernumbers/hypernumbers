HN.UI = {};

HN.UI.init = function()
{
    // Dialogs
    HN.UI.init_dialogs(layout);
    
    // Links that hope dialog boxes
    $(".dialoglink").each( function() {
        HN.UI.initHotLink(this);
    });

    //HN.UI.createColorList("#bgcolors", HN.BGCOLORS);
    //HN.UI.createColorList("#fontcolors", HN.FONTCOLORS);
    //HN.UI.createComboColorList("#combocolors", HN.COMBOCOLORS);
    HN.UI.createComboList("#combocolorsbg", HN.BGCOMBO);
    HN.UI.createComboList("#combocolorstxt", HN.TEXTCOMBO);
};

HN.UI.createColorList = function(sel, list) {
    for( var i=0; i<list.length; i++ ) {
        var tmp = $("<a class='color' data-color='"+list[i]+"' "
                    +" style='background-color:#"+list[i]+"'></a>");
        $(sel).append(tmp);
        
        if( ((i+1) % 6 == 0) ) {
            $(sel).append("<br />");
        }
    }
};

HN.UI.createComboList = function(sel, list) {
    for (var i=0; i<list.length; i++) {
         var tmp = $(
           "<a class='primocombocolor' data-bgcolor='"+list[i][2][4][0]
                 +"' data-fontcolor='"+list[i][2][4][1]
                 +"' style='background-color:#"+list[i][2][4][0]
                 +";color:#"+list[i][2][4][1]+"'"
                 +" title='#"+list[i][2][4][0]+", #"+list[i][2][4][1]+"'"
                 +">"+list[i][1]+"</a>"
                 +"<a class='combocolor' data-bgcolor='"+list[i][2][3][0]
                 +"' data-fontcolor='"+list[i][2][3][1]
                 +"' style='background-color:#"+list[i][2][3][0]
                 +";color:#"+list[i][2][3][1]+"'"
                 +" title='#"+list[i][2][4][0]+", #"+list[i][2][4][1]+"'"
                 +">ah</a>"
                 +"<a class='combocolor' data-bgcolor='"+list[i][2][2][0]
                 +"' data-fontcolor='"+list[i][2][2][1]
                 +"' style='background-color:#"+list[i][2][2][0]
                 +";color:#"+list[i][2][2][1]+"'"
                 +" title='#"+list[i][2][4][0]+", #"+list[i][2][4][1]+"'"
                 +">eh</a>"
                 +"<a class='combocolor' data-bgcolor='"+list[i][2][1][0]
                 +"' data-fontcolor='"+list[i][2][1][1]
                 +"' style='background-color:#"+list[i][2][1][0]
                 +";color:#"+list[i][2][1][1]+"'"
                 +" title='#"+list[i][2][4][0]+", #"+list[i][2][4][1]+"'"
                 +">oh</a>"
                 + "<a class='combocolor' data-bgcolor='"+list[i][2][0][0]
                 +"' data-fontcolor='"+list[i][2][0][1]
                 +"' style='background-color:#"+list[i][2][0][0]
                 +";color:#"+list[i][2][0][1]+"'"
                 +">uh</a><hr/>"

        );
        $(sel).append(tmp);
    };
};

HN.UI.createComboColorList = function(sel, list) {
    var tmp0 = "";
    var tmp1 = "";
    var tmp2 = "";
    var tmp3 = "";
    var tmp4 = "";
    var tmp5 = "";
    var tmp6 = "";
    var tmp7 = "";

    for( var i=0; i<list.length; i++ ) {
        tmp0 += "<a class='combocolor' data-bgcolor='"+list[i][0][0]
            +"' data-fontcolor='"+list[i][0][1]
            +"' style='background-color:#"+list[i][0][0]
            +";color:#"+list[i][0][1]+"'>a</a>";
        tmp1 += "<a class='combocolor' data-bgcolor='"+list[i][1][0]
            +"' data-fontcolor='"+list[i][1][1]
            +"' style='background-color:#"+list[i][1][0]
            +";color:#"+list[i][1][1]+"'>b</a>";
        tmp2 += "<a class='combocolor' data-bgcolor='"+list[i][2][0]
            +"' data-fontcolor='"+list[i][2][1]
            +"' style='background-color:#"+list[i][2][0]
            +";color:#"+list[i][2][1]+"'>c</a>";
        tmp3 += "<a class='combocolor' data-bgcolor='"+list[i][3][0]
            +"' data-fontcolor='"+list[i][3][1]
            +"' style='background-color:#"+list[i][3][0]
            +";color:#"+list[i][3][1]+"'>d</a>";
        tmp4 += "<a class='combocolor' data-bgcolor='"+list[i][4][0]
            +"' data-fontcolor='"+list[i][4][1]
            +"' style='background-color:#"+list[i][4][0]
            +";color:#"+list[i][4][1]+"'>1</a>";
        tmp5 += "<a class='combocolor' data-bgcolor='"+list[i][5][0]
            +"' data-fontcolor='"+list[i][5][1]
            +"' style='background-color:#"+list[i][5][0]
            +";color:#"+list[i][5][1]+"'>2</a>";
        tmp6 += "<a class='combocolor' data-bgcolor='"+list[i][6][0]
            +"' data-fontcolor='"+list[i][6][1]
            +"' style='background-color:#"+list[i][6][0]
            +";color:#"+list[i][6][1]+"'>3</a>";
        tmp7 += "<a class='combocolor' data-bgcolor='"+list[i][7][0]
            +"' data-fontcolor='"+list[i][7][1]
            +"' style='background-color:#"+list[i][7][0]
            +";color:#"+list[i][7][1]+"'>4</a>";
        if( ((i+1) % 24 == 0) ) {
            $(sel).append($(tmp0));
            $(sel).append("<br />");
            $(sel).append($(tmp1));
            $(sel).append("<br />");
            $(sel).append($(tmp2));
            $(sel).append("<br />");
            $(sel).append($(tmp3));
            $(sel).append("<br />");
            $(sel).append($(tmp4));
            $(sel).append("<br />");
            $(sel).append($(tmp5));
            $(sel).append("<br />");
            $(sel).append($(tmp6));
            $(sel).append("<br />");
            $(sel).append($(tmp7));
            $(sel).append("<br />");
            tmp0 = tmp1 = tmp2 = tmp3 = tmp4 = tmp5 = tmp6 = tmp7 = "";
        }
    }
};


HN.UI.initHotLink = function(link) {
    $(link).click( function() {
        HN.UI.open_dialog(layout, $(this).attr("data-id"));
    });
};

HN.UI.init_dialogs = function(layout)
{
    var el    = document.getElementsByClassName("dialog");
    var len   = el.length;
    var cur   = null;
    var dpos  = {"x":0, "y":0};
    var start = {"x":0, "y":0};
    
    $(".mclose").click( function() {
        HN.UI.close_dialog(layout, $(this).parent().attr("id"));
    });
    
    var up = function( e ) {
        HN.Util.removeEvent(document, "mouseup", up);
        HN.Util.removeEvent(document, "mousemove", move);
    };
    
    var move = function( e ) {
        var y = (dpos.y + (e.clientY - start.y));
        var x = (dpos.x + (e.clientX - start.x));
        cur.style.top  = (( y > 0 ) ? y : 0) + "px";
        cur.style.left = (( x > 0 ) ? x : 0) + "px";
    };
    
    var down = function( e ) {
        if( e.target.className == "close") {
            this.style.display = "none";
            e.preventDefault();
        } else if( e.target.nodeName == "H2" ||
                   e.target.parentNode.nodeName == "H2" ) {
            e.preventDefault();
            cur   = this;
            dpos  = {x:parseInt(this.style.left), y:parseInt(this.style.top)};
            start = {x:e.clientX, y:e.clientY};
            
            HN.Util.addEvent(document, "mousemove", move);
            HN.Util.addEvent(document, "mouseup", up);
        }
    };


    
    for( var i = 0; i < len; i++ ) {
        el[i].style.top = "50px";
        el[i].style.left = ($("body").width() - 400) + "px";
        el[i].innerHTML = "<div class='close'>&nbsp;</div>" + el[i].innerHTML;
        HN.Util.addEvent(el[i], "mousedown", down);
    }
};

HN.UI.open_dialog = function(layout, id) {

    var dialog = $("#"+id);
    var css = {"margin-left": -(dialog.width() /2),
               "margin-top" : -(dialog.height()/2)};

    layout.grabFocus();
    
    $("#cover").fadeIn("fast");
    dialog.css(css).fadeIn("fast").find("input").eq(0).focus();
};

HN.UI.close_dialog = function(layout, id) {

    layout.resumeSelection();

    $("#"+id).fadeOut("fast");
    $("#cover").fadeOut("fast");
};