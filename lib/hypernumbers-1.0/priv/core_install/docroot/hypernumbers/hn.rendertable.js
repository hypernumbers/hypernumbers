/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */


var hn = {},
    lang,
    sitedata;

HN.TableRender = function () {
    
    function render(sheet) { 

        var y, x, ix, iy, cell, position, value, css, tag,
            width = 0,
	    height = 0,
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

	// Determine with and height of sheet
        
        for (iy = 1; iy < sheet.max.y; iy = iy + 1) {                
            for (ix = 1; ix < sheet.max.x; ix = ix + 1) {
                cell = sheet.cell(iy, ix);
                if (cell && cell.value){
		    if ((ix + 1) > width) {
			width = ix + 1;
		    }
		    if ((iy + 1) > height) {
			height = iy + 1;
		    }
                }
            }
        }

	// Function for updating cell

	var update_cell = function(y, x, str) {	    
	    var path = sheet.path(),
	        cell = { "y" : y, "x" : x },
	        cmd  = { "set" : { "formula" : str } };
	    if (sheet.canWrite()) {
		HN.Util.postCell(path, cell, cmd);
		// TODO update filter view?		
	    }
	};

	// Function for showing line view (aka row popup)

	// TODO? the row corresponds to the unsorted row, not the row
	// in the sorted view.

	var line_view = function(row) {
	    /*
	    dialog = $("#lineview");
 	    dialog.css({
                  "position"   : "static",            
                  "max-width"  : "none",
                  "max-height" : "none",
                  "height"     : "auto",
                  "width"      : "auto"
	    });
	     */
	    if ((row < 2) || (row >= height)) return;
            var lines  = "<table id='lineview_table'>";
            for (var ix = 1; ix < width; ix++) {
		var head  = sheet.cell(1, ix).value  || ("Column " + ix.toString());
		var value = sheet.cell(row, ix).value || "";
		var field = sheet.canWrite() ?
		    ("<input value='" + value + "' id='lineview_in" + ix + "' />") :
		    ((value == "") ? "&nbsp;" : value);
		lines += "<tr><th><label>" + head + "</label></th></tr><tr><td>" + field + "</td></tr>";
	    }
            lines += "</table>";

	    $("#lineview_table").empty().append(lines);

	    if (sheet.canWrite()) {
		for (var ix = 1; ix < width; ix++) {
		    var f = function(y, x) {
			return function() { 
			    var value = $("#lineview_in"+x).val();
			    update_cell(y,x,value);
			};	
		    }(row,ix);
		    var field = $("#lineview_in"+ix);
		    field.change(f);
		    field.focus(function() { this.select(); })
		}

		// TODO rewrite the #lv_new handler to use
		// last_row_append method so that it avoids race
		// conditions.

		$("#lv_new").css({ "display": "inline" });
		$("#lv_new").click( function() {
                  line_view(height++);
                });
		
	    }
	    else {
		$("#lv_new").css({ "display": "none" });
		$("#lv_new").unbind("click");
	    }
	    $("#lv_this").html(row);   // actual row number
	    $("#lv_pick").val(row-1);  // displayed row number

	    if (row==2) {
		$("#lv_prev").css({ "cursor": "auto" });
		$("#lv_first").css({ "cursor": "auto" });
	    } else {
		$("#lv_prev").css({ "cursor": "pointer" });		
		$("#lv_first").css({ "cursor": "pointer" });		
	    }

	    if (row==(height-1)) {
		$("#lv_next").css({ "cursor": "auto" });
		$("#lv_last").css({ "cursor": "auto" });
	    } else {
		$("#lv_next").css({ "cursor": "pointer" });		
		$("#lv_last").css({ "cursor": "pointer" });		
	    }

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

	// Set up line_view (aka "row popup") navigation widgets

        for (var iy = 2; iy < height; iy++) {
	    // Wrapper function to work around JavaScript closure quirks.
	    var f = function(y) {
	      return function() { line_view(y); };	
	    }(iy);
	    $("#lineviewbtn"+iy).click(f);
	}

	$("#lv_first").click( function() { line_view(2); });
	$("#lv_last").click( function() { line_view(height-1); });
	$("#lv_max").html(height-2);
	$("#lv_prev").click( function() {
            var row = $("#lv_this").html();
            if (row > 2) {
		line_view(--row);
	    }
        });
	$("#lv_next").click( function() {
            var row = $("#lv_this").html();
            if (row < (height-1)) {
		line_view(++row);
	    }
        });
	$("#lv_pick").change( function() {
            var row = $("#lv_pick").val();
            if (isNaN(row) || (row < 1) || (row > (height-2))) {
		alert("Invalid Row Number");
		row = $("#lv_this").html();
		$("#lv_pick").val(row-1);		
	    } else {
		line_view(++row);
	    }
        });

	//

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

var bounded = function(x, min, max) {
   if (x<min) {
       return min;
   } else if (x>max) {
       return max;
   } else {
       return x;
   }
}

// TODO bottom margin when there is a maximum height should be
// calculated automatically.

var open_dialog = function (id) {
    var dialog = $("#" + id),
        win_width  = $(window).width(),
        win_height = $(window).height(),
        max_width  = Math.max(150, win_width  - 150),
        max_height = Math.max(150, win_height - 150),
        css        = {
            "position"   : "absolute",            
            "max-width"  : max_width  + "px",            
            "max-height" : max_height + "px",
	}

    var content = dialog.find(".scrollable");

    dialog.width(Math.min(max_width, dialog.width()));

    css["left"] = Math.max(0, ((win_width  - (dialog.width()))  / 2) + $("body").scrollLeft());

    dialog.height(Math.min(max_height, dialog.height()));

    css["height"] = dialog.height() + "px"; // IE fix

    css["top"]  = Math.max(0, ((win_height - (dialog.height())) / 2) + $("body").scrollTop());

    // Enable mouse dragging events

    // TODO movement is jittery

    var header = dialog.find('h2');

    var x = null, y = null;

    var mouse_move = function(e) {
      if (e.target != this) return true;
      e.stopPropagation();
      var px = bounded(parseInt(dialog.css('left')) + e.pageX - x, 0, win_width - dialog.width());
	  py = bounded(parseInt(dialog.css('top')) + e.pageY - y, 0, win_height - header.height());
      dialog.css({"cursor": "move", "left": px + "px", "top": py + "px"});
      x = e.pageX;
      y = e.pageY;
      e.preventDefault();
    }

    var mouse_down = function(e) {
      e.stopPropagation();
      x = e.pageX;
      y = e.pageY;		
      header.unbind("mousedown", mouse_down);
      header.bind("mousemove", mouse_move);
      e.preventDefault();
    };

    header.bind("mousedown", mouse_down);
    header.bind("mouseup", function(e) {
      e.stopPropagation();
      header.unbind("mousemove", mouse_move);
      header.bind("mousedown", mouse_down);
      dialog.css({cursor:"auto"});
      e.preventDefault();
    });

    // Bind the close_dialog function to keypresses when the dialog is
    // opened. If Esc is pressed or it's a mousedown event (triggered
    // by clicking on a close-dialog link), close the dialog and
    // unbind the function from keypress.

    var close_dialog = function(e) {
	  e.preventDefault();
	  $("#lightcover").hide();
	  dialog.hide();
	  $(document).unbind("keypress", close_dialog);
	  header.unbind(); // undo all mouse movement events
	  dialog.css({
                  "position"   : "static",            
                  "max-width"  : "none",
                  "max-height" : "none",
                  "height"     : "auto",
                  "width"      : "auto"
	  });
	  content.css({
                   "max-height" : "none",
                   "height"     : "auto"
          });	
    };

    $(document).keypress(function(e) {
      if (e.keyCode == 27) { close_dialog(e); }
    });

    dialog.find('a[id$="close"]').bind("mousedown", close_dialog);

    $("#lightcover").css('filter', 'alpha(opacity=10)'); /* IE8 fix */
    $("#lightcover").fadeIn("fast");
    dialog.css(css).fadeIn("fast");

    // In case the scrollable content is longer than the dialog box,
    // we calculate the height that it should be based on the size of
    // the header and footer, the content top margin, and the content
    // style margins.

    // Note that we don't bother adjusting for width.

    var footer = dialog.find(".footer");

    content.css({
      "max-height" : Math.min(dialog.height() - (content.position().top + footer.height() + header.height() - 4), content.height())
    });	

};

$("#path").html(HN.Util.getCrumbTrail());
$("#showhidecols").click(function () {
    open_dialog("columnmanagerdialog");
});
$("#lightcover, #overlay, #columnmanager, #lineview").fadeOut("fast");
