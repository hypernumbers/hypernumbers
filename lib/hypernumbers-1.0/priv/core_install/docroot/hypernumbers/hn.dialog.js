
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

    // Actually, resizing needs a lot of improvement, especially for
    // use with the function wizard.

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
                  "width"      : "auto"  // TODO we don't want dialogue resizing with very large data values
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
