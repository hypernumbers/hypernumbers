/**
 * jQuery.SpreadSheet
 * Copyright (c) 2008 HyperNumbers.com
 * All rights reserved.
 * @projectDescription A Spreadsheet GUI 
 * @author Dale Harvey
 * @version 0.1
 */

(function($) 
{
    $.fn.spreadsheet = function(options) 
    {    
        // Builder for the toolbar, an <a> is built for each element
        // the callback is called on click and is given info
        // on the current selection                    
        $.fn.toolbar = [                          
            {css:"left",    click:function(e,s){ s.range.css("text-align","left"); }},                        
            {css:"center",  click:function(e,s){ s.range.css("text-align","center"); }},
            {css:"right",   click:function(e,s){ s.range.css("text-align","right"); }},
            {css:"justify", click:function(e,s){ s.range.css("text-align","justify"); }},
            {css:"separator"},         
            {css:"bold",    click:function(e,s){ s.range.each(function(i) { toggle_css(this,"font-weight","normal","bold"); }); }},
            {css:"italic",  click:function(e,s){ s.range.each(function(i) { toggle_css(this,"font-style","normal","italic"); }); }},
            {css:"underl",  click:function(e,s){ s.range.each(function(i) { toggle_css(this,"text-decoration","none","underline"); }); }},
            {css:"strike",  click:function(e,s){ s.range.each(function(i) { toggle_css(this,"text-decoration","none","line-through"); }); }},
            {css:"separator"}
        ]; 
        
        $.fn.spreadsheet.defaults = 
        {
            range: "a1:z50",    // Default Range that is loaded
            fmargin: 0,
            cellSelect:null,
            change:null
        };

        $.fn.modes =
        { 
            CELL_EDIT:0,        
            FORMULA_EDIT:1,
            NOT_EDITING:2       
        };
    
        $.fn.states =           // Keep track of states during selection
        { 
            INIT:0,             // Page Just Loaded
            VIEW:1,             // Editing cell
            SELECT:2,           // Dragging over a range
            DRAG:3              // Formula Drag
        };
      
        // Consts for key presses
        $.fn.keys = 
        { 
            RETURN: 13,
            UP:     38, 
            DOWN:   40, 
            LEFT:   37, 
            RIGHT:  39 
        };

        // Consts for range types
        $.fn.addr = 
        {
            ROW:0,
            COLUMN:1,
            CELL:2,
            RANGE:3            
        };

        var o = $.extend({}, $.fn.spreadsheet.defaults, options);

        var edit_mode = $.fn.modes.NOT_EDITING;

        /**
        * Replaces a cell with an input element to enter cell values, 
        * called on cell focus, removes input on blur
        */    
        var select_cell = function($this,$sel)
        {
            var cell    = $(getcell($this,$sel.x,$sel.y)).find("div");
            var val     = $(cell).text();
            var formula = $this.find("#formula input");
    
            $(cell).empty();

            if(typeof o.cellSelect != "function")
                formula.val(val);
            else
                o.cellSelect($sel.x,$sel.y+1,formula);
    
            $(document.createElement("textarea")).val(val).keyup(function (e) 
            {              
                var coords = offset(e.keyCode,$sel);
    
                if(coords != false)
                    key_pressed($this,$sel,coords,e);
                else
                    formula.val($(this).val());
    
            }).keydown(function(e)
            {
                if(e.keyCode == $.fn.keys.RETURN)
                {
                    return false;
                }
            }).blur(function()
            {
                if(edit_mode  != $.fn.modes.FORMULA_EDIT)
                {
                    var newval = $(this).val();
                    $(this).parent().empty().text(newval);
            
                    if(val != newval && typeof o.change == "function") 
                    {
                        o.change($sel.x,$sel.y+1,newval); 
                    }
                    edit_mode  = $.fn.modes.NOT_EDITING;
                }
            }).appendTo(cell).width(cell.width()).height(cell.height()).focus();

            edit_mode = $.fn.modes.CELL_EDIT;              
        };

        /**
        * Replaces a cell with an input element to enter cell values, 
        * called on cell focus, removes input on blur
        */     
        var offset = function(key,$sel) 
        {
            switch(key)
            {
                case $.fn.keys.UP     : return [$sel.x,$sel.y-1]; break;
                case $.fn.keys.RIGHT  : return [$sel.x+1,$sel.y]; break;
                case $.fn.keys.LEFT   : return [$sel.x-1,$sel.y]; break;
                case $.fn.keys.DOWN   : 
                case $.fn.keys.RETURN : return [$sel.x,$sel.y+1];  break;
                default : return false; break;
            }
        };
                
        /**
        * Called when user presses arrow / navigation keys
        */   
        var key_pressed = function($this, $sel, i, e)
        {
            var cell = $(getcell($this,i[0],i[1])).find("div");
    
            if($(cell).size() > 0)
            {
                $sel.blurcell();
    
                if(e.shiftKey)
                {
                    $sel.state = $.fn.states.SELECT;
                    $sel.hover(cell[0]);
                }
                else
                {    
                    $sel.hide_selection();
                    $sel.hide_drag();             
                    $sel.start($.fn.addr.CELL,cell[0]);
                }
    
                select_cell($this,$sel);
                $sel.state = $.fn.states.VIEW;
                set_name($this,$sel);
            }
        }

        /**
        * Return a cell table given an x/y index
        */  
        var getcell = function($this,x,y)
        {
            return $this.find("tr:nth-child("+(y+1)+") > td:nth-child("+(x+1)+")");    
        };
    
        /**
        * Create the formula bar + name field and buttons
        */  
        var write_formula_bar = function(root,$sel)
        {
            root.prepend("<div class=\"clearfix\">"
                +"<input type=\"text\" id=\"name\" />"
                +"<button id=\"functions\">f(x)</button>"
                +"<div id=\"formula\"><input type=\"text\" /></div></div>");

            var change = function(x,y,val)
            {
                edit_mode = $.fn.modes.NOT_EDITING;

                if(typeof o.change == "function")
                    o.change(x,y,val);            
            }
    
            $("#formula input").click(function(e)
            {
                $(this).focus();
            }
            ).mousedown(function(e)
            {
                 edit_mode = $.fn.modes.FORMULA_EDIT;
            }
            ).keyup(function (e)
            {
                if($sel.state != $.fn.states.INIT)
                {
                    var coords = offset(e.keyCode,$sel);
        
                    if(coords != false)
                    {
                        change($sel.x,$sel.y+1,$(this).val());
                        key_pressed(root,$sel,coords,e);
                    }
                    else
                    {
                        var cell = $(getcell(root,$sel.x,$sel.y));
            
                        if(cell.size() > 0)
                            cell.find("div").text($(this).val());
                    }
                }
            }
            ).blur(function (e)
            {
                change($sel.x,$sel.y+1,$(this).val());
            });  
        };
    
        /**
        * Stub for the formatting tools etc
        */  
        var write_button_menu = function(root,$sel,toolbar)
        {
            var wrap = $("<div id=\"toolbar\" class=\"clearfix\">");
    
            $(toolbar).each(function(i)
            {
                var $this = this;
                var item = $("<a />").addClass($this.css);
    
                if(typeof this.click != "undefined")
                    item.click(function(e) { $this.click(e,$sel); });
    
                wrap.append(item);
            });
    
            root.prepend(wrap);
        };
    
        /**
        * Toggles a css attribute on an element between 2 values
        */    
        var toggle_css = function(el,attr,off,on)
        {
            $(el).css(attr,($(el).css(attr) == on ? off : on));    
        };
    
        /**
        * Sets default width / height values and display attributes
        * may be overridden with user defined values later
        */  
        var set_display_values = function(root,cols,fmargin)
        {
            root.find("tr th:not(:nth-child(1)),tr td:not(:nth-child(1))").width(100);
            root.find(".ssinner,#datainner").width(((cols - 1) * 100) + 25);
    
            $(".datacontainer").scroll(function (e) 
            {
                $("#headers").css("left","-"+e.target.scrollLeft+"px");            
            });
    
            $(window).resize(function()
            {
                var height = ($(window).height() - 
                    root.find(".datacontainer").offset().top) - fmargin;
    
                var width = $(root).width();
    
                root.find(".datacontainer").height(height).width(width);
                $("#marker").height(height-15);
            });
    
            $(window).resize();
        };
    
        /**
        * Sets the the value of the name field to the currently selected
        * range / cell
        */  
        var set_name = function(root,$s)
        {
            var range = $.fn.to_b26($s.startx) + "" + ($s.starty+1);
    
            if($s.startx != $s.endx && $s.starty != $s.endy)
                range += ":"+$.fn.to_b26($s.endx)+""+($s.endy+1);
    
            root.find("#name").val(range);
        };
    
        /**
        * Make table columns resizable
        */  
        var resize_columns = function(root,$sel)
        {
            var marker = $("<div id=\"marker\" />").appendTo(root);
            marker.css("top",root.find(".datacontainer").offset().top+"px");
            var headers = $("#headers th:not(:first)");
            var header,newwidth;
            var resize = false;
    
            endresize = function()
            {
                if(resize == true && header != null)
                {
                    root.find("#datainner").width($(".ssinner").width());
                    var child = $.fn.from_b26($(header).text()) + 1;
                    var td = $(".tabledata tr td:nth-child("+child+")");
                    td.width(newwidth);
                    td.find("textarea").width(newwidth-4);
                    marker.css("display","none");
                    resize = false;
                    $sel.hide_selection();
                    $sel.show_selection(); 
                }   
            };
    
            headers.mousemove(function(e)
            {
                var x = (e.clientX - root.offset().left);
    
                if(resize)
                {
                    var width = x - (header.offset().left - root.offset().left);
    
                    if(width > 1)
                    {
                        var cont = root.find("div.container > div.ssinner");
                        cont.width(cont.width() + ((width - header.width())));
                        header.width(width);
                        marker.css("left",x+"px");
                        newwidth = width;
                    }
                }
                else
                {
                    headers.css("cursor",
                        (x-($(this).offset().left-root.offset().left) > $(this).width()-4)
                        ? "col-resize" : "");
                }                  
            });
    
            $("#headers").bind("mouseleave",function(e)
            {
                endresize();
                return false; 
            });
    
            headers.each(function(i)
            {
                $(this).mousedown(function(e) 
                {
                    if(headers.css("cursor") ==  "col-resize")
                    {
                        marker.css("left",e.clientX+"px").css("display","block");
                        header = $(this);                    
                        resize = true;
                    }    
                    return false;
                });
        
                $(this).mouseup(function(e) 
                {
                    endresize();
                });
            });
        };
    
        /**
        * Make table rows resizable
        */  
        var resize_rows = function(root,$sel)
        {
            var row;
            var rows = $("td.rowindex");
            var resize = false;
            var top = root.offset().top;
    
            rows.mousemove(function(e)
            {
                var x = (e.clientY - top);
    
                if(resize)
                {
                    var height = x - ($(row).offset().top - top);
                    $(row).height(height);
                    $(row).parent().find("div,textarea").height(height);
                }
    
                else
                    rows.css("cursor",
                        (x - ($(this).offset().top - top) > $(this).height() - 4) 
                        ? "row-resize" : "");
            });
    
            rows.each(function()
            {
                $(this).mousedown(function(e) 
                {
                    if(rows.css("cursor") ==  "row-resize")
                    {
                        row = this;                   
                        resize = true;
                    }
                    return false;
    
                }).mouseup(function(e) 
                {
                    row = null;
                    resize = false;
                    
                    $sel.hide_selection();
                    $sel.show_selection();
                });
            });
        };
    
        /**
        * Creates the basic table layout
        */  
        var create_table = function(root,rows,cols)
        {
            var cont = $("<div class=\"ssinner\" />").appendTo(
                $("<div class=\"container\" />").appendTo(root));
    
            var tr = $("<tr />").appendTo($("<table id=\"headers\" />").appendTo(cont));
    
            for(var n = 0; n < cols; n++) 
            {
                tr.append((n == 0)
                    ? "<th class='rowindex'>&nbsp;</th>"
                    : "<th>"+ $.fn.to_b26(n)+"</th>" );
            }
    
            var table = $("<table cellspacing=\"0\" class=\"tabledata\" />").appendTo(
                $("<div id=\"datainner\" />").appendTo(
                $("<div class=\"datacontainer\" />").appendTo(cont)));                       
    
            for(var z = 1; z < rows+1; z++)
            {
                var row =  $("<tr/>").appendTo(table);
                
                for(var n = 0; n < cols; n++) 
                {
                    var col = $.fn.to_b26(n);
                    
                    row.append((n == 0)
                        ? "<td class='rowindex'>"+z+"</td>"
                        : "<td><div class='"+(col+z)+" cell'></div></td>");
                }
            }
        }
    
        /**
        * Returns the x/y coordinated of a cell from 
        * within a table
        */  
        var cell_index = function(cell)
        {
            return [cell.parentNode.cellIndex,
                cell.parentNode.parentNode.rowIndex];
        }

        /**
        * returns true if the mouse button clicked was a left button
        * only tested in ie
        */  
        var leftclick = function(button) 
        {
            return ($.browser.msie && button == 1) || button == 0;
        };        
    
        /**
        * This contains most of the functionality behind the range selection
        * and formula drag 
        */               
        var Selection = function(root) 
        {
            /**
            * Fake constructor
            */   
            this.construct = function(root)
            {
                var $this = this;
    
                this.state = $.fn.states.INIT;
                this.root  = root;
                this.tbl   = root.find("#datainner table");
                this.drag  = new Object();         
    
                // Store the current selection / borders etc
                // Quicker deletion
                this.drag.bottom = this.drag.top = 
                    this.drag.right  = this.drag.left =
                    this.cols_highlight = this.rows_highlight = 
                    this.range = $();
    
                // Little blue formula drag handler
                this.drag.div = $("<div id=\"drag\" />").appendTo(this.tbl)
                this.drag.div.bind("mousedown",function() 
                {
                    $this.state = $.fn.states.DRAG;
                    $this.blurcell();
                    return false;
                });
    
                this.add_events(this);
            }; 
    
            /**
            * Mouse Events that track user selections
            */               
            this.add_events = function($this)
            {
                this.tbl.bind('mousedown',function(e) 
                {
                    if( leftclick(e.button) && !e.shiftKey && $(e.target).is("div"))
                    {
                        $this.start($.fn.addr.CELL,e.target);
                        return false;
                    }
        
                }).bind('mouseover',function(e) 
                { 
                    if($(e.target).is("div"))
                    {
                        $this.hover(e.target);
                        return false;
                    }
        
                }).bind('mouseup',function(e) 
                { 
                    if( leftclick(e.button) && $(e.target).is("div"))
                    {
                        if(e.shiftKey)
                        {
                            $this.state = $.fn.states.SELECT;
                            $this.hover(e.target);
                            $this.end(e.target);
                            set_name(root,$this);
                        }
                        else if($this.state == $.fn.states.SELECT 
                            || $this.state == $.fn.states.DRAG)
                        {
                            $this.end(e.target);
                            set_name(root,$this);
                        }
                    }
                    return true;
                });
                    
                this.root.find("div.datacontainer").scroll(function (e) 
                {
                    $this.hide_selection();
                    $this.show_selection();
                });
            };
    
            /**
            * Called on mousedown, clear + hide all the previous selections
            */               
            this.start = function(addr,cell)
            {
                if(this.state != $.fn.states.INIT)
                    this.blurcell();
    
                this.hide_selection();
                this.hide_drag();
    
                this.state = $.fn.states.SELECT;
                var i = cell_index(cell);
                this.x = this.initx = this.startx = this.endx = i[0];
                this.y = this.inity = this.starty = this.endy = i[1];
                this.hover(cell);
            };
    
            /**
            * Set actively selected cell
            */        
            this.set = function(cell)
            {
                var index = cell_index(cell);
                this.x = index[0];
                this.y = index[1];            
    
            };
    
            /**
            * Blur currently selected editable cell
            */        
            this.blurcell = function(cell)
            {
                $(getcell(root,this.x,this.y)).find("textarea").blur();
            };
    
            /**
            * Moused up
            */
            this.end = function(cell)
            {
                if(this.state != $.fn.states.DRAG)
                {
                    this.set(cell);
                    select_cell(root,this);
                }
        
                this.state = $.fn.states.VIEW;
            };
    
            /**
            * Call when a new cell is being added, either by dragging over
            * with mouse or keyboard shortcuts
            */
            this.hover = function(cell)
            {
                // Typical range selection, drag or shift select
                if(this.state == $.fn.states.SELECT)
                {
                    this.newrefs = function(x,y,x1,y1,x2,y2,x3,y3)
                    {
                        if(x >= x3 && y >= y3) return [x3,y3,x,y];
                        if(x >= x3 && y <= y3) return [x3,y,x,y3];
                        if(x <= x3 && y >= y3) return [x,y3,x3,y];
                        if(x <= x3 && y <= y3) return [x,y,x3,y3];
                    };
    
                    var i = cell_index(cell);
                    var refs = this.newrefs( i[0], i[1],
                        this.startx,this.starty,
                        this.endx,this.endy,
                        this.initx,this.inity);
    
                    if(refs != null)
                    {
                        this.hide_selection();  
    
                        this.x = i[0];
                        this.y = i[1];
                        this.startx = refs[0];
                        this.starty = refs[1];
                        this.endx = refs[2];
                        this.endy = refs[3];                 
    
                        this.range = this.tbl.find("tr:lt("+(this.endy+1)+"):gt("+(this.starty-1)
                            + ")").find("td:lt("+(this.endx+1)+"):gt("+(this.startx-1)+") div");
                        this.rows_highlight = this.tbl.find("tr td.rowindex:lt("
                            + (this.endy+1)+"):gt("+(this.starty-1)+")")
                        this.cols_highlight = root.find("tr th:lt("+(this.endx+1)
                            + "):gt("+(this.startx-1)+")");
    
                        this.show_selection();
                    }
                }
    
                // Formula drag
                else if(this.state == $.fn.states.DRAG)
                {
                    var i = cell_index(cell);
                    
                    // Make sure the new selection is outside current
                    // range
                    if(    i[0] >= this.startx && i[0] <= this.endx 
                        && i[1] >= this.starty && i[1] <= this.endy)
                    {
                        return false;
                    }
    
                    this.newrefs = function(x,y,x1,y1,x2,y2,x3,y3)
                    {
                        if(x >= x1 && x <= x2 && y > y3) return [x1,y2+1,x2,y];
                        if(x >= x1 && x <= x2 && y < y3) return [x1,y,x2,y1-1];
                        if(y >= y1 && y <= y2 && x < x3) return [x,y1,x1-1,y2];
                        if(y >= y1 && y <= y2 && x > x3) return [x2+1,y1,x,y2];
                        return null;
                    };
    
                    var refs = this.newrefs(i[0],i[1],
                        this.startx,this.starty,
                        this.endx,this.endy,
                        this.initx,this.inity);
    
                    if(refs != null)
                    {
                        this.hide_drag();
    
                        this.drag.startx = refs[0];
                        this.drag.starty = refs[1]; 
                        this.drag.endx = refs[2];
                        this.drag.endy = refs[3]
    
                        this.drag.right = this.tbl.find("tr:lt("+(this.drag.endy+1)+"):gt(" 
                            + (this.drag.starty-1)+")").find("td:eq("+(this.drag.endx)+") div");   
                        this.drag.left = this.tbl.find("tr:lt("+(this.drag.endy+1)+"):gt("
                            + (this.drag.starty-1)+")").find("td:eq("+(this.drag.startx)+") div");          
                        this.drag.top = this.tbl.find("tr:eq("+(this.drag.starty)+")"
                            ).find("td:lt("+(this.drag.endx+1)+"):gt("+(this.drag.startx-1)+") div");
                        this.drag.bottom = this.tbl.find("tr:eq("+(this.drag.endy)+")"
                            ).find("td:lt("+(this.drag.endx+1)+"):gt("+(this.drag.startx-1)+") div");
    
                        this.show_drag();
                    }
                }
            };
    
            /**
            * Adds the background to selected items, positions the drag handle
            * and highlights the row / column index highlights
            */
            this.show_selection = function()
            {
                this.rows_highlight.addClass("highlight");
                this.cols_highlight.addClass("highlight");
                this.range.addClass("selected");
    
                if(this.state != $.fn.states.INIT)
                {
                    var c = $(getcell(root,this.endx,this.endy));
                    var off = c.offset();
    
                    $(getcell(root,this.x,this.y)).addClass("current");
    
                    // Hide of displayed above the table
                    if(off.top+c.height() < this.root.find(".datacontainer").offset().top)
                        this.drag.div.css("display","none");
                    
                    else
                    {
                        this.drag.div.css("left",(off.left+c.width()-3)+"px");
                        this.drag.div.css("top", (off.top+c.height())+"px");
                        this.drag.div.css("display","block");
                    }
                }
            };
    
            /**
            * Hides above
            */      
            this.hide_selection = function()
            {
                this.rows_highlight.removeClass("highlight");
                this.cols_highlight.removeClass("highlight");
                this.range.removeClass("selected");           
                this.drag.div.css("display","none");
    
                if(this.state != $.fn.states.INIT)
                {
                    $(getcell(root,this.x,this.y)).removeClass("current");
                }            
            };
    
            /**
            * Adds the dashed borders around a drag 
            * selection
            */
            this.show_drag = function()
            {
                this.drag.right.addClass("dragright");
                this.drag.left.addClass("dragleft");
                this.drag.top.addClass("dragtop");
                this.drag.bottom.addClass("dragbottom");
            };
    
            /**
            * Hides above
            */
            this.hide_drag = function()
            {
                this.drag.right.removeClass("dragright");
                this.drag.left.removeClass("dragleft");
                this.drag.top.removeClass("dragtop");
                this.drag.bottom.removeClass("dragbottom");
            };
    
            // I do this to make sure all the functions
            // are defined before they can be called
            // when the object is created
            this.construct(root);
        };

        /**
        * Entry point
        */   
        return this.each(function() 
        {               
            var $this = $(this);
            var range = $.fn.parse_cell(o.range.split(":")[1]);
            
            create_table($this,range[1],range[0]);

            var $sel = new Selection($this);

            write_formula_bar($this,$sel);
            write_button_menu($this,$sel,$.fn.toolbar);

            resize_columns($this,$sel);
            resize_rows($this,$sel);
            set_display_values($this,range[0],o.fmargin);
        });
    };

    /**
     * Parses a cell reference (a1) into its x/y coordinates
     */
    $.fn.parse_cell = function(cell) 
    {
        var x = $.fn.from_b26((cell.match(/[a-z]+/i)[0]).toLowerCase());
        var y = parseInt(cell.match(/[0-9]+/)[0]);
            
        return [x,y];
    };

    /**
     * turns an integer into a base 26 string
     * 1,2,3,4,5,,,,26,27,28
     * a,b,c,d,e,,,,z,aa,ab
     * TODO : fix
     */  
    $.fn.to_b26 = function(cell) 
    {
        return String.fromCharCode(cell+96);
    };

    /**
     * turns a base 26 string into an interger
     * TODO : fix
     */  
    $.fn.from_b26 = function(cell) 
    {
        return cell.charCodeAt(0)-96;
    };

})(jQuery);