/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */


var hn = {},
    lang,
    sitedata;

HN.TableRender = function () {

    // indexer(table) builds an index between spreadsheet rows and
    // displayed table rows. It should be called whenever the table is
    // changed, e.g. adding rows, sorting, or filtering.

    function indexer(table) {
        $("tr", table).removeAttr('displayIndex');
        var cnt = -1;
        $("tr:visible", table).each(function (i) {
           $(this).attr('displayIndex',++cnt);
        });
        table.trigger("indexEnd");
    }
    
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

	// Determine width and height of sheet
        
        for (iy = 1; iy < sheet.max.y; iy = iy + 1) {                
            for (ix = 1; ix < sheet.max.x; ix = ix + 1) {
                cell = sheet.cell(iy, ix);
                if (cell){
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
                // Note: indexer is already triggered
	    }
	};

	// Utility functions to translate between row and display indices.

	var row2display = function(row) {
	  return parseInt($('tr[rowindex="' + row + '"]:first').attr("displayindex")); 
	};

	var display2row = function(row) {
	  return parseInt($('tr[displayindex="' + row + '"]:first').attr("rowindex"));
	};

        var displayCount = function() {
	  return $('tr[displayindex]').length - 1;
	};

	// Function for showing line view (aka Row Editor)

	var tr = null; // static variable with current row element

	var line_view = function(row) {
	    dialog = $("#lineview");
	    if ((row < 2) || (row > height)) return;

	    var restore_row = function() {
	      if (tr) {
		tr.attr("class", tr.attr("x-class")).removeAttr("x-class");		
	      }
	    };

            tr = $('tr[rowindex="' + row + '"]:first');
	    if (tr) {
		tr.attr("x-class", tr.attr("class")).attr("class", "highlight");

	       var row_top    = tr.offset().top,
                   row_height = tr.height(),
		   body_top   = $("body").scrollTop(),
		   win_height = $(window).height();

		if (row_top >= (body_top + win_height)) {
		    
		    body_top += ((row_top + row_height + 1) - (body_top + win_height));

		    $('body').animate({scrollTop : body_top }, 'slow');
	            var this_top = ((win_height - dialog.height()) / 2) + body_top;
	            dialog.animate({ "top" : this_top }, 'slow');

		} else if (row_top < body_top) {

		    body_top -= (body_top - row_top) + 1;

		    $('body').animate({scrollTop : body_top }, 'slow');
	            var this_top = ((win_height - dialog.height()) / 2) + body_top;
	            dialog.animate({ "top" : this_top }, 'slow');
		    
		}
	    }

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
		    field.focus(function(e) { this.select(); });
		}

		// TODO rewrite the #lv_new handler to use
		// last_row_append method so that it avoids race
		// conditions.

		// BUG Newly added row is not highlighted.

		$("#lv_new").css({ "display": "inline" });
		$("#lv_new").click( function() {
		  return function(e) {
                    e.stopPropagation();
		    restore_row();
		    var table = $("#table"),
                        row = height++;
		    $("#lv_new").unbind("click");   // unbind event (line_view will re-add it)

		  // Note that the indexer is run automatically when
		  // the table is re-rendered, by calling the sortEnd
		  // event. However, if we call line_view() after
		  // updating a cell, it is possible that the indexer
		  // won't finish before line_view() is called. To
		  // work around this, we bind a function to the
		  // indexEnd event (called by the indexer when it is
		  // done) that calls line_view instead.

                    table.bind("indexEnd", function(table,row) {
		      return function(e) {
                        line_view(row);
                        table.unbind("indexEnd");
                      }; 
                    }(table,row));

                    update_cell(row,1,"");          // visibly add new row
		  };
                }());
		
	    }
	    else {
		$("#lv_new").css({ "display": "none" });
	    }
	    $("#lv_this").html(row);   // actual row number
	    $("#lv_pick").val(row2display(row));  // displayed row number

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

	    $("#lv_max").html(displayCount());

	    if (!dialog.is(':visible')) {

		$("#lv_first").click( function() {
		    return function(e) {
		      e.stopPropagation();
		      restore_row();
 		      line_view(display2row(1)); 
		    };
                }());

		$("#lv_last").click( function() {
		    return function(e) {
		      e.stopPropagation();
		      restore_row();
		      line_view(display2row(displayCount())); 
		    };
                }());

		$("#lv_prev").click( function() {
                  return function(e) {
		    e.stopPropagation();
		    var idx = row2display($("#lv_this").html());
		    if (idx > 1) {
		      restore_row();
		      line_view(display2row(idx-1));
		    }
		  };			 
		}());

		$("#lv_next").click( function() {
                  return function(e) {
		    e.stopPropagation();
		    var idx = row2display($("#lv_this").html());
		    if (idx < displayCount()) {
		      restore_row();
		      line_view(display2row(idx+1));
		    }
                  };
		}());

		$("#lv_pick").change( function() {
                  return function(e) {
		    var idx = $("#lv_pick").val();
		    if (isNaN(idx) || (parseInt(idx) < 1) || (parseInt(idx) > displayCount())) {
		      alert("Invalid Row Number");
		      idx = row2dislay($("#lv_this").html());
		      $("#lv_pick").val(idx);		
		    } else {
		      restore_row();
		      line_view(display2row(idx));
		    }
		  };
		}());

		$("#lv_pick").focus( function() {
		  return function(e) {
		     this.select();
		  };
		}());

		open_dialog("lineview"); // TODO only when not yet visible!

		var cleanup = function(dialog) {
                  return function(e) {
		    dialog.unbind("CloseDialog", cleanup);		
		    $("#lv_new").unbind();	   
		    $("#lv_pick").unbind();	   
		    $("#lv_first").unbind();	   
		    $("#lv_prev").unbind();	   
		    $("#lv_next").unbind();	   
		    $("#lv_last").unbind();
		    restore_row();
	          };
		}(dialog);
		dialog.bind("CloseDialog", cleanup);		
	    }

	};

        for (var iy = 1; iy < sheet.max.y + 1; iy++) {

            if (typeof data.data.cell[iy] !== "undefined") {

                if (first) {
                    html += "<thead>";
                } 

                html += "<tr rowIndex='" + iy + "'>";
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

	// TODO investigate why sortEnd is triggered multiple times
	// when a cell is changed through the row editor.

        var reindex = function() { indexer(table); };

	table.unbind("sortEnd", reindex); // only one call should be bound
	table.bind("sortEnd", reindex);
        table.trigger("sortEnd");

	// Set up Row Editor (aka "Line View") navigation widgets

        for (var iy = 2; iy < height; iy++) {
	    // Wrapper function to work around JavaScript closure quirks.
	    var f = function(y) {
	      return function(e) { 
                line_view(y); 
              };
	    }(iy);
	    $("#lineviewbtn"+iy).click(f);
	}


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


var open_dialog = function (id) {
    var dialog = $("#" + id),
        win_width  = $(window).width(),
        win_height = $(window).height(),
        max_width  = Math.max(50, win_width  - 50),
        max_height = Math.max(50, win_height - 50),
        css        = {
            "position"   : "absolute",            
            "max-width"  : max_width  + "px",            
            "max-height" : max_height + "px"
	};

    var content = dialog.find(".scrollable");

    // TODO trap window resize events and adjust max size accordingly,
    // and fix bug where large sized dialog is not resized when opened
    // after window is shrunk.

    css["left"] = Math.max(0, ((win_width  - (dialog.width()))  / 2) + $("body").scrollLeft());

    dialog.height(Math.min(max_height, dialog.height(), 300));
    css["height"] = dialog.height() + "px"; // IE fix

    css["top"]  = Math.max(0, ((win_height - (dialog.height())) / 2) + $("body").scrollTop());

    // Dialogue Moving

    var disable_text_selection = function(e) {
      e.stopPropagation();
      e.preventDefault();
      return false;
    };

    var header = dialog.find('h2');

    var x = null, y = null;

    var move_dialog = function(dialog,header) { 
      return function(e) {
        e.stopPropagation();
        var px = bounded(parseInt(dialog.css('left')) + e.pageX - x, 0, win_width - dialog.width()),
	    py = bounded(parseInt(dialog.css('top')) + e.pageY - y, $("body").scrollTop(), $("body").scrollTop() + win_height - header.height());
        dialog.css({"left": px + "px", "top": py + "px"});
        x = e.pageX;
        y = e.pageY;
        e.preventDefault();
      };
    }(dialog,header);

    var enable_move_dialog = function(header) { 
      return function(e) {
        e.stopPropagation();
        x = e.pageX;
        y = e.pageY;	
        header.unbind("mousedown", enable_move_dialog);
	$(document).bind("mouseup",  disable_move_dialog);
        $(document).bind("mousemove", move_dialog);
        header.css("cursor", "move");
        e.preventDefault();
      };
    }(header);

    var disable_move_dialog = function(header) { 
      return function(e) {
        e.stopPropagation();
        $(document).unbind("mousemove", move_dialog);
        header.bind("mousedown", enable_move_dialog);
	$(document).unbind("mouseup",  disable_move_dialog);
        header.css("cursor", "auto");
        e.preventDefault();
      };
    }(header);

    header.bind("mousedown", enable_move_dialog);

    // Dialogue Resizing

    var footer  = dialog.find(".footer"),
        resizer = footer.find(".resize");

    // In case the scrollable content is longer than the dialog box,
    // we calculate the height that it should be based on the size of
    // the header and footer, the content top margin, and the content
    // style margins.

    // Note that we don't bother adjusting for width.

    var adjust_content = function(dialog, content, header, footer) {
      content.height(dialog.height() - (content.position().top + footer.height() + header.height() - 4));
      content.css({ "height" : content.height() + "px" }); // IE fix
    };

    var resize_dialog = function(dialog, content, header, footer) {	
      return function(e) {
        e.stopPropagation();
        var px = dialog.width() + e.pageX - x,
	    py = dialog.height() + e.pageY - y;      

        dialog.width(px);
        dialog.height(py);
        dialog.css({"width": px + "px", "height": py + "px"}); // IE fixes

	adjust_content(dialog, content, header, footer);

        x = e.pageX;
        y = e.pageY;	 
        e.preventDefault();
      };
    }(dialog, content, header, footer);

    var enable_resize_dialog = function(resizer) {
      return function(e) {
        e.stopPropagation();
        x = e.pageX;
        y = e.pageY;		
        resizer.unbind("mousedown");
        $(document).bind("mousemove", resize_dialog);
        $(document).bind("mouseup",  disable_resize_dialog);
        e.preventDefault();	
      };
    }(resizer);

    var disable_resize_dialog = function(resizer) {
      return function(e) {
        e.stopPropagation();
        $(document).unbind("mousemove", resize_dialog);
        $(document).unbind("mouseup",  disable_resize_dialog);
        resizer.bind("mousedown", enable_resize_dialog);
        e.preventDefault();
      };
    }(resizer);

    // disable text selection so that it doesn't interfere with resize
    resizer.bind("selectstart", disable_text_selection);
    resizer.bind("mousedown", enable_resize_dialog);

    // Bind the close_dialog function to keypresses when the dialog is
    // opened. If Esc is pressed or it's a mousedown event (triggered
    // by clicking on a close-dialog link), close the dialog and
    // unbind the function from keypress.

    dialog.bind("CloseDialog", function(e) {
	  $("#lightcover").hide();
	  dialog.hide();
	  $(document).unbind("keypress", close_dialog);
	  header.unbind(); // undo all mouse movement events
          resizer.unbind();
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
        $(document).unbind("selectstart", disable_text_selection);	
    });

    var close_dialog = function(e) {
	e.preventDefault();
        dialog.trigger("CloseDialog");
        dialog.unbind("CloseDialog");
    };

    $(document).keypress(function(e) {
      if (e.keyCode == 27) { close_dialog(e); }
    });

    dialog.find('a[id$="close"]').bind("mousedown", close_dialog);

    $(document).bind("selectstart", disable_text_selection);

    $("#lightcover").css('filter', 'alpha(opacity=10)'); /* IE8 fix */
    $("#lightcover").fadeIn("fast");
    dialog.css(css).fadeIn("fast");

    dialog.css("min-height", dialog.height() + "px"); // set min height for resize

    adjust_content(dialog, content, header, footer);
/*
    content.css({
        "max-height" : Math.min(dialog.height() - (content.position().top + footer.height() + header.height() - 4), content.height())
     });		
*/

};

$("#path").html(HN.Util.getCrumbTrail());
$("#showhidecols").click(function () {
    open_dialog("columnmanagerdialog");
});
$("#lightcover, #overlay, #columnmanager, #lineview").fadeOut("fast");
