
/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.TableRender = function () {
    
    function render(sheet) { 

        var y, x, ix, iy, cell, position, value, css, tag,
            width = 0,
            first = true,
            html  = "<table id='tablecol'>",
            config = "",
            data  = sheet.pageData();

        var opt = {listTargetID: 'targetall', 
                   onClass: 'advon', 
                   offClass: 'advoff',  
                   hide: function(c){ 
                       $(c).fadeOut(); 
                   },  
                   show: function(c){ 
                       $(c).fadeIn(); 
                   }}; 

        var which = function(col) {
            var list;
            list = col.split("_");
            return list[1];
        };
        
        for (iy = 1; iy < sheet.max.y; iy++) {                
            for (ix = 1; ix < sheet.max.x; ix++) {
                cell = sheet.cell(iy, ix);
                if (cell && cell.value && (ix + 1) > width) {
                    width = ix + 1;
                }
            }
        }

        for (iy = 1; iy < sheet.max.y + 1; iy++) {

            if (typeof data.data.cell[iy] != "undefined") {

                if (first) {
                    html += "<thead>";
                } 

                html += "<tr>";
                
                for (ix = 1; ix < width; ix++) {

                    cell = sheet.cell(iy, ix);
                    position = sheet.cell_offset(y, x);
                    value    = cell.value || "";
                    if (first) {
                        html += "<th>" + value + "</th>";
                        config += "<div class=\"configrow\"><label>" + value + " " 
                            + "<input class='colhider' "
                            + "type='checkbox' checked='yes' name='col_" 
                            + ix +"' /></label></div>";
                    } else {
                        html += "<td>" + value + "</td>";
                    }
                }
                html += "</tr>";
                
                if (first) {
                    html += "</thead>";
                    first = false;
                }

            }
        }

        var table = $(html + "</table>");
        $("#table").empty().append(table);
        $.tablesorter.defaults.widgets=['zebra'];
        $('#tablecol').columnManager({listTargetID:'targetcol', onClass: 'simpleon', offClass: 'simpleoff'});
        table.addClass("tablesorter").tablesorter();
        $("#config").html(config);
        $('.colhider').change(function(event){ $('#tablecol').toggleColumns(which(event.target.name), opt); });
    };

    return {
        "render" : render
    };
};

HN.RenderPage = (function () {

    var data,
        sheets = [],
        tableRender = new HN.TableRender(),
        path       = document.location.pathname,
        options = {
            "stopUpdate"   : true,
            "dataLoaded"   : function () {
                sheets[path] = new HN.Sheet(path, data);
                tableRender.render(sheets[path]);
                HN.Util.initLogin();
                setTimeout(function () {
                    $("#editspreadsheet").fadeIn("slow");
                }, 1000);
            },
            "dataReloaded" : function (data) {  
                sheets[path].reload_data(data);
                tableRender.render(sheets[path]);
            },
            "update"       : function () { 
                sheets[path].processLoaded();            
                tableRender.render(sheets[path]);
            }
        };
    
    data = new HN.Data(options);
    data.addPage(path);

}());

var open_dialog = function(id) {
    var dialog = $("#"+id);
    var css = {"margin-left": -(dialog.width() /2),
               "margin-top" : -(dialog.height()/2)};
    $("#lightcover").css('filter', 'alpha(opacity=10)'); /* IE8 fix */
    $("#lightcover").fadeIn("fast");
    dialog.css(css).fadeIn("fast");
};

$("#path").html(HN.Util.getCrumbTrail());
$("#showhidecols").click(function(){open_dialog("columnmanagerdialog");});
$("#dialogclose").bind("mousedown", function() {
                           $("#lightcover, #columnmanagerdialog").hide();});
$("#lightcover, #overlay, #columnmanager").fadeOut("fast");
