/**
 * jQuery TableResizer
 * Version 0.1 - 23/04/2008
 * @author Dale Harvey <harveyd@gmail.com>
 *
 * A lightweight plugin that creates resizable tables
 *
 **/

(function($) {

$.fn.tableresizer = function(options) 
{        
    $.fn.tableresizer.defaults = 
    {
        set_width  : null,
        set_height : null
    };
    
    // default options used on initialisation
    // and arguments used on later calls
    var opts = $.extend({}, $.fn.tableresizer.defaults, options);
    var args = arguments;
    
    /**
     * Make table columns resizable
     */  
    var resize_columns = function(root)
    {                   
        var tbl = root.children("table");
        var tr  = tbl.find("tr:first");
        var header,newwidth;
        var resize = false;
        
        root.width(tbl.width());
            
        var left_pos = root.offset().left;
    
        endresize = function()
        {
            if(resize == true && header != null)
            {
                if(typeof opts.set_height == "function")
                {
                    opts.set_width(header[0].cellIndex + 1,newwidth);
                }
                
                document.onselectstart=new Function ("return true");
                resize = false;
                root.children("table").css("cursor","");
            }   
        };
        
        tbl.mousemove(function(e)
        {
            var left = (e.clientX - left_pos);
    
            if(resize)
            {
                // when jquery includes dimensions into core, use that
                // to get implicit with instead of subtracting padding
                var width = left - (header.offset().left - left_pos)
                    - parseInt(header.css("padding-left"))
                    - parseInt(header.css("padding-right"));
    
                if(width > 1)
                {
                    var current_width = header.width();
                    // If expanding, resize container first, else resize
                    // column then container. otherwise the adjacent 
                    // cells resize
                    if(width > current_width)
                    {
                        var total = root.width() + ((width - header.width()));
                        root.width(total);
                        root.find("table").width(total);
                        header.width(width);
                    }
                    else
                    {
                        // nasty, need to resize table before wrapper
                        var total = root.width() + ((width - header.width()));
                        root.find("table").width(total);
                        
                        header.width(width);
                       
                        // check the header resize (might have
                        // a min width
                        if(header.width() == width)
                        {
                            var total = root.width() + ((width - current_width));
                            root.width(total);
                        }
                    }
                    newwidth = width;
                }
            }
            else
            {
                if(e.target.nodeName == "TH")
                {
                    // nasty calculation to check the mouse is on / around
                    // the border to a header
                    var tgt = $(e.target);
                    var dosize = (left-(tgt.offset().left-left_pos) 
                        > tgt.width()-2);
                    $(this).css("cursor",dosize?"col-resize":"");
                }
            }                   
        });
        
        tbl.mouseup(function(e) 
        {
            endresize();   
        });
                
        tbl.bind("mouseleave",function(e)
        {
            endresize();
            return false; 
        });
        
        tr.mousedown(function(e) 
        {
            if(e.target.nodeName == "TH" 
                && $(this).css("cursor") ==  "col-resize")
            {
                header = $(e.target);                    
                resize = true;
                // Stop ie selecting text
                document.onselectstart=new Function ("return false");
            }    
            return false;
        });
        
        tr.bind('mouseleave',function(e)
        {
            if(!resize)
                root.children("table").css("cursor","");
        });
    };
    
    /**
     * Make table rows resizable
     */  
    var resize_rows = function(root)
    {            
        var tbl = root.find("table");
        var row,newheight;
        var rows = root.find("tr").children("td:nth-child(1)");
        var resize = false;
        
        var top = root.offset().top;

        rows.mousemove(function(e)
        {
            var x = (e.clientY - top) + document.documentElement.scrollTop
                + document.body.scrollTop;
    
            if(resize)
            {
                var height = x - (row.offset().top - top);
                row.height(height);
                newheight = height;
            }
    
            else
            {
                var cursor = (x - ($(this).offset().top - top) 
                    > $(this).height() - 6) ? "row-resize" : "";
                tbl.css("cursor",cursor);
            }
        });
        
        rows.mousedown(function(e) 
        {
            if(tbl.css("cursor") ==  "row-resize")
            {
                row = $(e.target);    
                resize = true;
                // Stop ie selecting text
                document.onselectstart=new Function ("return false");
            }
            return false;
        });
        
        tbl.mouseup(function(e) 
        {
            if(typeof opts.set_height == "function" && row != null)
            {
                opts.set_height(row.parents("tr")[0].rowIndex,newheight);
            }
        
            document.onselectstart=new Function ("return true");
            row = null;
            resize = false;
            tbl.css("cursor","");
        });
    };
    
    /**
     * Entry point
     */   
    return this.each(function() 
    {
        var root = $(this).wrap("<div class='roottbl' />").parent();
        resize_columns(root);
        resize_rows(root);  
    });
};

})(jQuery);
