/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, layout: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false , toolbar: false */
HN.UI = {};

HN.UI.init = function ()
{
    // Dialogs
    HN.UI.init_dialogs(layout);
    
    // Links that hope dialog boxes
    $(".dialoglink").each(function () {
        HN.UI.initHotLink(this);
    });

    HN.UI.createComboList("#combocolorsbg", HN.THEMES);
};

HN.UI.createComboList = function (sel, list) {

    var themes = list.slice(0),
        recent = HN.Util.localStorage() &&
          localStorage.recentColours &&
          JSON.parse(localStorage.recentColours) ||
          HN.DEFAULT_COLOUR,
        i, html, len, attributes;
    
    themes.push(recent);
    
    attributes = function (obj) {
        var bg   = obj["background-color"],
            fore = obj.color;
        return "data-bgcolor='" + bg + "' data-textcolor='" + fore + "' " +
            "style='background-color:#" + bg + ";color:#" + fore + "' " + 
            "title='#" + bg + ", #" + fore + "'";
    };
    
    for (i = 0, html = "<table>", len = themes.length; i < len; i = i + 1) {

        html += "<tr><td>" + "<a class='title' " + 
            attributes(themes[i].combos[0]) + ">" + themes[i].name + "</a></td>" +
            "<td><a " + attributes(themes[i].combos[1]) + ">Ab</a></td>" +
            "<td><a " + attributes(themes[i].combos[2]) + ">Cd</a></td>" +
            "<td><a " + attributes(themes[i].combos[3]) + ">Ef</a></td>" +
            "<td><a " + attributes(themes[i].combos[4]) + ">Gh</a></td>";
    }

    $(sel).empty().append(html);
};

HN.UI.initHotLink = function (link) {
    $(link).click(function () {
        HN.UI.open_dialog(layout, $(this).attr("data-id"));
    });
};

HN.UI.init_dialogs = function (layout)
{
    var el    = $(".dialog"),
        len   = el.length,
        cur   = null,
        dpos  = {"x": 0, "y": 0},
        start = {"x": 0, "y": 0},
        i, up, move, down;
    
    $(".mclose").click(function () {
        HN.UI.close_dialog(layout, $(this).parent().attr("id"));
    });
    
    up = function (e) {
        HN.Util.removeEvent(document, "mouseup", up);
        HN.Util.removeEvent(document, "mousemove", move);
    };
    
    move = function (e) {
        var y = (dpos.y + (e.clientY - start.y)),
            x = (dpos.x + (e.clientX - start.x));
        cur.style.top  = ((y > 0) ? y : 0) + "px";
        cur.style.left = ((x > 0) ? x : 0) + "px";
    };
    
    down = function (e) {
        
        if (e.target.className === "close") {
            this.style.display = "none";
            e.preventDefault();
        } else if (e.target.nodeName === "H2" ||
                   e.target.parentNode.nodeName === "H2") {
            e.preventDefault();
            cur   = this;
            dpos  = {x: parseInt(this.style.left, 10), y: parseInt(this.style.top, 10)};
            start = {x: e.clientX, y: e.clientY};
            
            HN.Util.addEvent(document, "mousemove", move);
            HN.Util.addEvent(document, "mouseup", up);
        }
    };
    
    for (i = 0; i < len; i =  i + 1) {
        el[i].style.top = "50px";
        el[i].style.left = ($("body").width() - 400) + "px";
        HN.Util.addEvent(el[i], "mousedown", down);
    }
};

HN.UI.open_dialog = function (layout, id) {

    var dialog = $("#" + id),
        css = {"margin-left": -(dialog.width() / 2),
               "margin-top" : -(dialog.height() / 2)};

    layout.grabFocus();
    
    $("#cover").fadeIn("fast");
    dialog.css(css).fadeIn("fast").find("input").eq(0).focus();
};

HN.UI.close_dialog = function (layout, id, callback) {

    layout.resumeSelection();

    if (callback) { 
        $("#" + id).fadeOut("fast", callback);        
    } else { 
        $("#" + id).fadeOut("fast");
    }
    $("#cover").fadeOut("fast");
};