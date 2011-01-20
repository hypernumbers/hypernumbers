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

	var line_view = function(row) {
	    // sheet.canWrite()
            var lines  = "<table id='lineview_table'>";
            for (var ix = 1; ix < width; ix++) {
		var head  = sheet.cell(1, ix).value  || ("Column " + ix.toString());
		var value = sheet.cell(row, ix).value || "";
		var field = sheet.canWrite() ?
		    ("<input value='" + value + "' />") :
		    ((value == "") ? "&nbsp;" : value);
		lines += "<tr><th>" + head + "</th></tr><tr><td>" + field + "</td></tr>";
	    }
            lines += "</table>";

	    $("#lineview_table").empty().append(lines);
	    open_dialog("lineview");      
	};

        for (var iy = 1; iy < sheet.max.y + 1; iy++) {

            if (typeof data.data.cell[iy] !== "undefined") {

                if (first) {
                    html += "<thead>";
                } 

                html += "<tr>";
                for (var ix = 1; ix < width; ix++) {

                    cell = sheet.cell(iy, ix);
                    position = sheet.cell_offset(y, x);
                    value    = cell.value || "";
                    if (first) {
                        html += "<th jFilterSelect='1'>" + value + "</th>";
                        config += "<div class=\"configrow\"><strong>" + value + "</strong> " +
                            "<label for='coli_" + ix + "'>Show? " +
                            "<input class='colhider' " +
                            "type='checkbox' checked='checked' name='col_" + 
                            ix + "' id='coli_" + ix + "' /></label>" +
                            " <label for='fili_" + ix + "'>Filter? " +
                            "<input class='filhider' " +
                            "type='checkbox' name='fil_" + 
                            ix + "' id='fili_" + ix + "' /></label>" +
                            "</div>";
                    } else {
                        html += "<td>" + value + "</td>";
                    }
                }
                // add button for line view
		if (first) {
		    html += "<th jFilterSelect='0'></th>"; // disable filters for this column
		} else {
		    html += "<td><a id='lineviewbtn" + iy + "' class='lineviewbutton'><img src='/hypernumbers/right.png' alt='line view' /></a></td>";		    
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
	var hdrs = { headers: { } };
            hdrs['headers'][width-1] = { sorter: false}; // disable sorting 
        table.addClass("tablesorter").tablesorter(hdrs);

        for (var iy = 2; iy < sheet.max.y + 1; iy++) {
	    // Wrapper function to work around JavaScript closure quirks.
	    var f = function(y) {
	      return function() { line_view(y); };	
	    }(iy);
	    $("#lineviewbtn"+iy).click(f);
	}

        $("#columnsconfig").html(config);
        $('.colhider').change(function (event) {
            $('#tablecol').toggleColumns(which(event.target.name), opt);
        });
        $('.filhider').change(function (event) {
            var col = which(event.target.name);
/*
            $("th:nth-child("+col+")").attr("jFilterSelect", event.target.checked?1:0);
            $("#table").trigger("applyWidgets");
*/
            id="#filter-column-"+table.attr('id')+"-"+(col-1);
            if (event.target.checked) {
		$(id).show();		    
	    } else {
		$(id).hide();					     
	    }
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
        win_width  = $(window).width(),
        win_height = $(window).height(),
        max_width  = win_width  - 150,
        max_height = win_height - 150,
        css        = {
	    "overflow"   : "auto",
            "max-width"  : max_width  + "px",            
            "max-height" : max_height + "px",
	}

    if (dialog.width() > max_width) {
	dialog.width(max_width);
    }

    css["left"] = Math.max(0, ((win_width  - (dialog.width()))  / 2) + $("body").scrollLeft());

    if (dialog.height() > max_height) {
	dialog.height(max_height);
    }

    css["height"] = dialog.height() + "px"; // IE fix

    css["top"]  = Math.max(0, ((win_height - (dialog.height())) / 2) + $("body").scrollTop());

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
$("#lightcover, #overlay, #columnmanager, #lineview").fadeOut("fast");

$("#lineviewclose").bind("mousedown", function () {
    $("#lightcover, #lineview").hide();
});
