/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.TableRender = function () {
    
    function render(sheet) { 

        var y, x, ix, iy, cell, position, value, css, tag,
            width = 0,
            first = true,
            html  = "<table>", 
            data  = sheet.pageData();

        for (iy = 1; iy < sheet.max.y; iy++) {                
            for (ix = 1; ix < sheet.max.x; ix++) {
                cell = sheet.cell(iy, ix);
                if (cell && cell.value && (ix + 1) > width) {
                    width = ix + 1;
                }
            }
        }

        for (iy = 1; iy < sheet.max.y; iy++) {

            if (typeof data.data.cell[iy] != "undefined") {

                if (first) {
                    html += "<thead>";
                    tag   = "th";
                } else {
                    tag = "td";
                }

                html += "<tr>";
                
                for (ix = 1; ix < width; ix++) {

                    cell = sheet.cell(iy, ix);
                    position = sheet.cell_offset(y, x);
                    value    = cell.value || "";
                    
                    html += "<"+tag+">" + value + "</"+tag+">";
                }
                html += "</tr>";
                
                if (first) {
                    html += "</thead>";
                    first = false;
                }

            }
        }

        var table = $(html + "</table>");
        $("#table").append(table);
        table.addClass("tablesorter").tablesorter();
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

$("#path").html(document.location.pathname);

setTimeout(function () {
    $("#editspreadsheet").fadeIn("slow");
}, 1000);
