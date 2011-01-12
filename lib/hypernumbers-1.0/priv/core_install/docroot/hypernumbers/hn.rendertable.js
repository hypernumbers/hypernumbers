/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */


var hn = {},
    lang,
    sitedata;

HN.TableRender = function () {
    
    function render(sheet) { 

        var y, x, ix, iy, cell, position, value, css, tag,
            width = 0,
            first = true,
            html  = "<table id='tablecol'>",
            config = "",
            data  = sheet.pageData(),
            table,
            opt,
            which;

        opt = {
            listTargetID: 'targetall', 
            onClass:  'advon', 
            offClass: 'advoff',  
            hide: function (c) { 
                $(c).fadeOut(); 
            },  
            show: function (c) { 
                $(c).fadeIn(); 
            }
        }; 

        which = function (col) {
            var list;
            list = col.split("_");
            return list[1];
        };
        
        for (iy = 1; iy < sheet.max.y; iy = iy + 1) {                
            for (ix = 1; ix < sheet.max.x; ix = ix + 1) {
                cell = sheet.cell(iy, ix);
                if (cell && cell.value && (ix + 1) > width) {
                    width = ix + 1;
                }
            }
        }

        for (iy = 1; iy < sheet.max.y + 1; iy = iy + 1) {

            if (typeof data.data.cell[iy] !== "undefined") {

                if (first) {
                    html += "<thead>";
                } 

                html += "<tr>";
                for (ix = 1; ix < width; ix = ix + 1) {

                    cell = sheet.cell(iy, ix);
                    position = sheet.cell_offset(y, x);
                    value    = cell.value || "";
                    if (first) {
                        html += "<th jFilterSelect='1'>" + value + "</th>";
                        config += "<div class=\"configrow\"><label>" + value + " " +
                            "<input class='colhider' " +
                            "type='checkbox' checked='yes' name='col_" + 
                            ix + "' /></label></div>";
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

        table = $(html + "</table>");
        $("#table").empty().append(table);
        $.tablesorter.defaults.widgets = ['jfilterselect','zebra'];
        $('#tablecol').columnManager({listTargetID: 'targetcol', onClass: 'simpleon', offClass: 'simpleoff'});
        table.addClass("tablesorter").tablesorter();
        $("#columnsconfig").html(config);
        $('.colhider').change(function (event) {
            $('#tablecol').toggleColumns(which(event.target.name), opt);
        });
    }

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
                sheets[path] = new HN.Sheet(path, data, false);
                tableRender.render(sheets[path]);
                HN.Util.initLogin("tableview");
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
        },
        successFun = function (data) {
            hn.functions = data.functions;
            hn.pages = data.pages;
            hn.groups = data.groups;
            hn.is_admin = data.is_admin;
            lang = data.lang;
        };
    
    sitedata = new HN.SiteData();
    sitedata.loadSiteData(successFun);
    data = new HN.Data(options);
    data.addPage(path);
    // need to expose data via hn (fugly bodge)
    hn.data = data;

}());

var open_dialog = function (id) {
    var dialog = $("#" + id),
        css = {"left": ($(document).width() - (dialog.width())) / 2,
               "top" : ($(document).height() - (dialog.height())) / 2};
    $("#lightcover").css('filter', 'alpha(opacity=10)'); /* IE8 fix */
    $("#lightcover").fadeIn("fast");
    dialog.css(css).fadeIn("fast");
};

$("#path").html(HN.Util.getCrumbTrail());
$("#showhidecols").click(function () {
    open_dialog("columnmanagerdialog");
});
$("#dialogclose").bind("mousedown", function () {
    $("#lightcover, #columnmanagerdialog").hide();
});
$("#lightcover, #overlay, #columnmanager").fadeOut("fast");
