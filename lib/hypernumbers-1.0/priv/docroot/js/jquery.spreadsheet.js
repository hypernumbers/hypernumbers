/**
 * jQuery SpreadSheet
 * Version 0.1 - 18/03/2008
 * @author Dale Harvey <harveyd@gmail.com>
 *
 * http://code.google.com/p/jqueryspreadsheet/
 *
 **/

(function($) {

    // Consts for key presses
    $.key = {
        ENTER: 13,
        UP:    38,
        DOWN:  40,
        LEFT:  37,
        RIGHT: 39
    };

    $.fn.spreadsheet = function(options)
    {
        // Builder for the toolbar, an <a> is built for each element
        // the callback is called on click and is given info
        // on the current selection
        $.fn.toolbar = [
            {css:"left",    click:function(e,s){ s.range.each(
                function() { set_css(this,"text-align","left"); }); }},
            {css:"center",  click:function(e,s){ s.range.each(
                function() { set_css(this,"text-align","center"); }); }},
            {css:"right",   click:function(e,s){ s.range.each(
                function() { set_css(this,"text-align","right"); }); }},
            {css:"justify", click:function(e,s){ s.range.each(
                function() { set_css(this,"text-align","justify"); }); }},
        {css:"separator"},
            {css:"bold",    click:function(e,s){ s.range.each(
                function() { toggle_css(this,"font-weight","normal","bold"); }); }},
            {css:"italic",  click:function(e,s){ s.range.each(
                function() { toggle_css(this,"font-style","normal","italic"); }); }},
            {css:"underl",  click:function(e,s){ s.range.each(
                function() { toggle_css(this,"text-decoration","none","underline"); }); }},
            {css:"strike",  click:function(e,s){ s.range.each(
                function() { toggle_css(this,"text-decoration","none","line-through"); }); }},
            {css:"separator"},
            {css:"format",  click:function(e,s){
                var f = $("#formats");
                f.toggle();
                select_frame(f,(f.find(".section_list li:first a").text()).toLowerCase());
             }},
             {css:"separator"},
             {css:"form",  click:function(e,s){
                $("#builder").toggle();
             }},
             {css:"formview",  click:function(e,s){
                document.location = document.location.pathname;
             }}
        ];

        $.fn.spreadsheet.defaults = {
            range: "a1:z200",    // Default Range that is loaded
            fmargin: 0,         // Footer Margin
            cellSelect:null,    // Callback on a cell being selected
            cellChange:null,    // Callback for a cell Changing
            formatChange:null,
            rowResize:null,
            colResize:null,
            cssChange:null,
            addName:null,
            nameSet:null,
            fullscreen:false
        };

        $.fn.modes = {
            CELL_EDIT:0,        // Currently Editing a cell
            FORMULA_EDIT:1,     // Editing in the formula bar
            NOT_EDITING:2       // Not Editing
        };

        $.fn.states =           // Keep track of states during selection
        {
            INIT:0,             // Page Just Loaded
            VIEW:1,             // Editing cell
            SELECT:2,           // Dragging over a range
            DRAG:3              // Formula Drag
        };

        // Consts for range types
        $.fn.addr = {
            ROW:0,
            COLUMN:1,
            CELL:2,
            RANGE:3
        };

        // default options used on initialisation
        // and arguments used on later calls
        var opts = $.extend({}, $.fn.spreadsheet.defaults, options);
        var args = arguments;

        /**
     * Replaces a cell with an input element to enter cell values,
     * called on cell focus, removes input on blur
     */
        var on_end_selection = function($this,$sel,fullselect)
        {
            //console.log(edit_mode);
            // get rid of click bound event in case of
            // double click
            $(document).unbind("keydown.ss");
            $(document).unbind("mousedown.ss");

            var dom_formula = $this.find("#formula");
            var dom_cell    = cell_div($sel.tbl,$sel.x,$sel.y);

            var cell_val    = $(dom_cell).text();

            !is_fun(opts.cellSelect)
                ? dom_formula.val(cell_val)
                : opts.cellSelect($sel.x,$sel.y+1,dom_formula);

            var formula_val = dom_formula.val();

            var move = function(e,sel)
            {
                var coords = offset(e.keyCode,sel.x,sel.y);
                if(coords != false)
                {
                    key_pressed($this,sel,coords,e);
                    return true;
                }
            };

            var keyup = function(e,test)
            {
                dom_formula.val($(this).val());

                if(!fullselect)
                {
                    move(e,$sel);
                    return false;
                }
            };

            var keydown = function(e)
            {
                return (e.keyCode != $.key.ENTER);
            };

            var blur = function()
            {
                var newval = $(this).val();
                dom_cell.removeClass("current");
                $(this).parent().empty().text(newval);

                if(cell_val != newval && is_fun(opts.cellChange)
                   && edit_mode != $.fn.modes.FORMULA_EDIT)
                    opts.cellChange($sel.x,$sel.y+1,newval);
            };

            var init = function(e)
            {
                if(e != null)
                {
                    var inv_chars = new Array(13,17,37,38,39,40);

                    if($.inArray(e.keyCode,inv_chars) != -1)
                    {
                        move(e,$sel)
                        return true;
                    }
                    $(document).unbind("keydown.ss");
                }

                dom_cell.empty();
                dom_cell.addClass("current");

                var v = fullselect ? formula_val : "";

                $("<textarea />").val(v).keyup(keyup).keydown(keydown).blur(blur)
                .appendTo(dom_cell).width(dom_cell.width()).height(dom_cell.height()).focus();
            };

            if(fullselect)
                init(null);
            else
                $(document).bind("keydown.ss",init);

            var removeinit = function(e)
            {
                $(document).unbind("keydown.ss");
            };
            $(document).one("mousedown.ss",removeinit);

            edit_mode = $.fn.modes.CELL_EDIT;
        };

    /**
     * Called when user presses arrow / navigation keys
     */
    var key_pressed = function($this, $sel, i, e)
    {
        var cell = cell_div($sel.tbl,i[0],i[1]);

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

            on_end_selection($this,$sel,false);
            $sel.state = $.fn.states.VIEW;
            set_name($this,$sel);
        }
    }

    /**
     * Create the formula bar + name field and buttons
     */
    var write_formula_bar = function(root,$sel)
    {
        var initval = "";

        var change = function(x,y,val)
        {
            edit_mode = $.fn.modes.NOT_EDITING;

            if(typeof opts.cellChange == "function" && initval != val)
                 opts.cellChange(x,y,val);
        }

        $("#formula").mousedown(function(e)
        {
            edit_mode = $.fn.modes.FORMULA_EDIT;
            $(this).focus().addClass("focus");
            initval = $(this).val();
        }
        ).keyup(function (e)
                {
                    if($sel.state != $.fn.states.INIT)
                    {
                        var coords = offset(e.keyCode,$sel.x,$sel.y);

                        if(coords != false)
                        {
                            change($sel.x,$sel.y+1,$(this).val());
                            key_pressed(root,$sel,coords,e);
                        }
                        else
                        {
                            var c = cell($sel.tbl,$sel.x,$sel.y);

                            if(c.size() > 0)
                            {
                                c.children("div").text($(this).val());
                            }
                        }
                    }
                }
        ).blur(function (e)
               {
                   $(this).removeClass("focus");
                   change($sel.x,$sel.y+1,$(this).val());
                   initval = "";
               });
    };

    /**
     * Stub for the formatting tools etc
     */
    var write_button_menu = function(root,$sel,toolbar)
    {
        var wrap = root.find("div.toolbar");

        $(toolbar).each(function(i)
        {
            var $this = this;
            var item = $("<a />").addClass($this.css);

            if(typeof this.click != "undefined")
                item.click(function(e) { $this.click(e,$sel); });

            wrap.append(item);
        });
    };

    /**
     * Toggles a css attribute on an element between 2 values
     * Used for toolbar
     */
    var toggle_css = function(el,attr,off,on)
    {
        set_css(el,attr,($(el).css(attr) == on ? off : on));
    };

    /**
     *
     */
    var set_css = function(el,attr,val)
    {
        if(typeof opts.cssChange == "function")
            opts.cssChange(el,attr,val);

        $(el).css(attr,val);
    };

    /**
     * Sets default width / height values and display attributes
     * may be overridden with user defined values later
     */
    var set_display_values = function(root,cols,fmargin)
    {
        var width = 100;

        root.find("div.columns table, div.data table").width(cols * width);
        root.find("div.columns table th, div.data table td").width(width);
        root.find("div.data table td div").width(width-6);

        root.find(".data").scroll(function (e)
        {
            root.find("div.columns table").css("left","-"+e.target.scrollLeft+"px");
            root.find("div.rows table").css("top","-"+e.target.scrollTop+"px");
        });

        $(window).resize(function()
        {
            root.children("div.datatable").width($(root).width());

            var h = ((opts.fullscreen)
                     ? $(window).height()
                     : $(root).height()) - root.offset().top - fmargin;

            root.find(".data,.rows").height(h);

            $("#formula").width(root.width()-200);
        });

        $(window).resize();


        var tblopts =
        {
            col_border : false,
            row_border : "1px solid #aaa",
            set_height: function(row,height)
            {
                set_height(root,row+1,height);
                if(typeof opts.rowResize == "function")
                    opts.rowResize(row+1,height);
            },
            set_width: function(col,width)
            {
                set_width(root,$.fn.to_b26(col),width);
                if(typeof opts.colResize == "function")
                    opts.colResize(col,width);

            }
        };

        $("div.rows table, div.columns table").tableresizer(tblopts);
    };

    /**
     * Sets the the value of the name field to the currently selected
     * range / cell
     */
    var set_name = function(root,$s)
    {
        var range = $.fn.to_b26($s.startx+1) + "" + ($s.starty+1);

        if($s.startx != $s.endx || $s.starty != $s.endy)
            range += ":"+$.fn.to_b26($s.endx+1)+""+($s.endy+1);

        root.find("#name").val(range);
    };

    /**
     * Creates the basic table layout
     */
    var create_table = function(root,rows,cols)
    {
         var html = '\
            <div class="clearfix toolbar" />\
            <div class="clearfix formulabar">\
                <input type="text" id="name" />\
                <input type="button" id="functionbutton" value="f(x)" />\
                <input type="text" id="formula"/>\
            </div>\
            <div class="datatable">\
                <div class="corner" />\
                <div class="columns">\
                    <table cellpadding="0" cellspacing="0"><tr></tr></table>\
                </div>\
                <div class="rowsanddata">\
                    <div class="rows">\
                        <table cellpadding="0" cellspacing="0"></table>\
                    </div>\
                    <div class="data">\
                        <table cellpadding="0" cellspacing="0"></table>\
                    </div>\
                </div>\
            </div>\
            <div class="dialog" id="functions">\
                <h2>Functions</h2>\
                <div class="clearfix">\
                    <ul class="section_list" />\
                    <div class="content_list" />\
                </div>\
                <div class="functioninput">\
                    <input type="text" id="curfunction">\
                    <input type="button" id="insertfunction" value="insert formula" />\
                </div>\
            </div>\
            <div class="dialog" id="builder">\
              <h2>Form Builder</h2>\
              <div class="clearfix" id="form" />\
            </div>\
        ';

        root.append(html);

        var colstable = root.find("div.columns table tr");
        for(var n = 1; n < cols+1; n++)
        {
            colstable.append("<th>"+ $.fn.to_b26(n)+"</th>");
        }

        var rowstable = root.find("div.rows table");
        for(var z = 1; z < rows+1; z++)
            rowstable.append($("<tr><td>"+z+"</td></tr>"));

        var datatable = root.find("div.data table");
        for(var z = 1; z < rows+1; z++)
        {
            var row = $("<tr/>").appendTo(datatable);

            for(var n = 0; n < cols; n++)
                row.append("<td><div class='"+($.fn.to_b26(n)+z)
                    + " cell'></div></td>");
        }

        root.find(".cover").click(function(e)
        {
            return false;
        });
    };

    /**
     * Creates the basic table layout
     */
    var setup_rowcol_selection = function(root,sel,maxcol,maxrow)
    {
        var is_th = function(e)
        {
            return e.target.nodeName == "TH";
        };

        var columns = root.find("div.columns table tr");
        columns.bind('mousedown',function(e)
        {
            if(is_th(e) && $(this).css("cursor") !=  "col-resize")
            {
                if(sel.state != $.fn.states.INIT)
                    sel.blurcell();

                sel.hide_selection();
                var index = $.fn.from_b26($(e.target).text()) - 1;
                sel.setbounds(index,0,index,0,index,maxcol);
                sel.set_selection();
                sel.show_selection();

                columns.mouseover(function(e)
                {
                    if(is_th(e))
                    {
                        sel.hide_selection();
                        var newindex = $.fn.from_b26($(e.target).text())-1;
                        var start = index < newindex ? index : newindex;
                        var end =   index > newindex ? index : newindex;
                        sel.setbounds(index,0,start,0,end,maxcol);
                        sel.set_selection();
                        sel.show_selection();
                    }
                });

                $("body").one('mouseup',function(e)
                {
                    columns.unbind('mouseover');
                });
            }
        });

        var rows = root.find("div.rows table tr");
        rows.bind('mousedown',function(e)
        {
            if($(this).css("cursor") !=  "row-resize")
            {
                if(sel.state != $.fn.states.INIT)
                    sel.blurcell();

                sel.hide_selection();
                var index = parseInt($(e.target).text())-1;

                sel.setbounds(0,index,0,index,maxrow,index);
                sel.set_selection();
                sel.show_selection();

                rows.mouseover(function(e)
                {
                    sel.hide_selection();
                    var newindex = parseInt($(e.target).text())-1;
                    var start = index < newindex ? index : newindex;
                    var end =   index > newindex ? index : newindex;
                    sel.setbounds(0,index,0,start,maxrow,end);
                    sel.set_selection();
                    sel.show_selection();
                });

                $("body").one('mouseup',function(e)
                {
                    rows.unbind('mouseover');
                });
            }

            return false;
        });
    };

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
            this.tbl   = root.find(".data table");
            this.drag  = new Object();

            // Store the current selection / borders etc
            // Quicker deletion
            this.drag.bottom = this.drag.top =
                this.drag.right  = this.drag.left =
                this.cols_highlight = this.rows_highlight =
                this.range = $();

            // Little blue formula drag handler
            this.drag.div = $("<div id=\"drag\" />").insertAfter(this.tbl);
            this.drag.div.bind("mouseover",function(e)
            {
                e.preventDefault();
                return false;
            });

            this.drag.div.bind("mousedown",function()
            {
                document.onselectstart=new Function ("return false");
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
            var cell = "div:not(#drag)";

            var mousedown = function(e)
            {
                var formula = $("#formula");
                if(formula.hasClass("focus"))
                    formula.blur();

                if( leftclick(e.button) && !e.shiftKey && $(e.target).is(cell))
                {
                    $this.start($.fn.addr.CELL,e.target);
                    document.onselectstart=new Function ("return false");
                    e.preventDefault();
                    return false;
                }
                return true;
            };

            var mouseover = function(e)
            {
                if($(e.target).is(cell))
                {
                    $this.hover(e.target);
                    e.preventDefault();
                    return false;
                }
                return true;
            };

            var mouseup = function(e)
            {
                if( leftclick(e.button) && $(e.target).is(cell))
                {
                    document.onselectstart=new Function ("return true");

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
                        $this.end(e.target,false);
                        set_name(root,$this);
                    }
                }
                return true;
            }

            var dblclick = function(e)
            {
                $this.end(e.target,true);
                set_name(root,$this);
            };

            this.tbl.bind('mousedown',mousedown).bind('mouseover',mouseover)
            .bind('mouseup',mouseup).bind('dblclick',dblclick);

            this.root.find("div.data").scroll(function (e)
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
            var i = $.fn.cell_index(cell);
            this.x = this.initx = this.startx = this.endx = i[0];
            this.y = this.inity = this.starty = this.endy = i[1];

            this.hover(cell);
        };

        /**
         * Set actively selected cell
         */
        this.setbounds = function(x,y,startx,starty,endx,endy)
        {
            this.x = x;
            this.y = y;
            this.startx = startx;
            this.starty = starty;
            this.endx = endx;
            this.endy = endy;
        };

        /**
         * Set actively selected cell
         */
        this.set = function(cell)
        {
            var index = $.fn.cell_index(cell);
            this.x = index[0];
            this.y = index[1];
        };

        /**
        * Blur currently selected editable cell
        */
        this.blurcell = function(cell)
        {
            cell_div(this.tbl,this.x,this.y).children("textarea").blur();
        };

        /**
        * Moused up
        */
        this.end = function(cell,fullselect)
        {
            if(this.state != $.fn.states.DRAG)
            {
                this.set(cell);
                on_end_selection(root,this,fullselect);
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
                    return false;
                };

                var i = $.fn.cell_index(cell);

                var refs = this.newrefs( i[0], i[1],
                    this.startx,this.starty,
                    this.endx,this.endy,
                    this.initx,this.inity);

                if(refs != null)
                {
                    this.hide_selection();
                    this.setbounds(i[0],i[1],refs[0],refs[1],refs[2],refs[3]);
                    this.set_selection();
                    this.show_selection();
                }
            }

            // Formula drag
            else if(this.state == $.fn.states.DRAG)
            {
                var i = $.fn.cell_index(cell);

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

                    this.drag.right = this.tbl.find("tr:lt("+(this.drag.endy+1)+"):gt("+(this.drag.starty-1)+")").find("td:eq("+(this.drag.endx)+")").children("div");
                    this.drag.left = this.tbl.find("tr:lt("+(this.drag.endy+1)+"):gt("+(this.drag.starty-1)+")").find("td:eq("+(this.drag.startx)+")").children("div");
                    this.drag.top = this.tbl.find("tr:eq("+(this.drag.starty)+")").find("td:lt("+(this.drag.endx+1)+"):gt("+(this.drag.startx-1)+")").children("div");
                    this.drag.bottom = this.tbl.find("tr:eq("+(this.drag.endy)+")").find("td:lt("+(this.drag.endx+1)+"):gt("+(this.drag.startx-1)+")").children("div");

                    this.show_drag();
                }
            }
            return false;
        };

        this.set_selection = function()
        {
            var c =  root.children("div.data");
            this.range = this.tbl.find("tr:lt("+(this.endy+1)+"):gt("+(this.starty-1) + ")").find("td:lt("+(this.endx+1)+"):gt("+(this.startx-1)+")").children("div");
            this.rows_highlight = root.find("div.rows:first table").find("tr td:lt("+(this.endy+1)+"):gt("+(this.starty-1)+")");
            this.cols_highlight = root.find("div.columns:first table").find("tr th:lt("+(this.endx+1)+"):gt("+(this.startx-1)+")");
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
                var c = cell(this.tbl,this.endx,this.endy);
                var off = c.offset();
//                cell(this.tbl,this.x,this.y).addClass("current");
                // Hide of displayed above the table
                if(off.top+c.height() < this.tbl.parent().parent().offset().top)
                {
                    this.drag.div.css("display","none");
                }
                else
                {
                    var f = document.documentElement.scrollTop
                        + document.body.scrollTop;
                    this.drag.div.css("left",(off.left+c.width()-3)+"px");
                    this.drag.div.css("top", (off.top+c.height()-f)+"px");
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
                cell(this.tbl,this.x,this.y).removeClass("current");
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

        this.current_range = function()
        {
            if(typeof this.startx != "undefined")
            {
                return $.fn.to_b26(this.startx+1) + "" + (this.starty+1)
                    + ":"+$.fn.to_b26(this.endx+1)+""+(this.endy+1);
            }
            return false;
        };

        // I do this to make sure all the functions
        // are defined before they can be called
        // when the object is created
        this.construct(root);
    };

    var addName = function(tb,names,range,item)
    {
        if(typeof names[item] == "undefined")
        {
            names[item] = range;
            tb.textbox("add",item);
        }
    };

   /**
    * Set the height of a row
    */
    var set_height = function(root,row,height)
    {
        var ind  = root.find(".rows table tr:nth-child("+row+") td");
        var td = root.find(".data tr:nth-child("+row+") td");

        td.height(height+1);
        ind.height(height);

        td.find("div,textarea").height(height+1);
    };

    var select_frame = function(dialog,category)
    {
        dialog.find(".content_list > *").hide();
        dialog.find(".section_list li").removeClass("selected_cat");
        dialog.find(".section_list li."+category).addClass("selected_cat");
        dialog.find(".content_list ."+category).show();
    };

   /**
    */
   var init_dialogs = function(root,sel)
   {
        root.find("#functionbutton").click(function()
        {
            var f   = $("#functions");
            var cat = (f.find(".section_list li:first a").text()).toLowerCase();
            f.toggle();
            select_frame(f,cat);
        });

        var format_win = $("#formats");

        format_win.find(".submit").click(function()
        {
            var val = format_win.find("input.format").val();
            var range = sel.current_range();

            if(range && typeof opts.formatChange == "function" && val != "")
                 opts.formatChange(range,val);
        });

        format_win.find("select").change(function(item)
        {
            format_win.find("input.format").val($(this).val());
        });

        root.find("#insertfunction").click(function()
        {
            var txt = root.find("#curfunction").val();

            if(txt != "")
            {
                var formula = root.find("#formula");
                var newval = (formula.val() == "")
                    ? txt : formula.val() + txt.substring(1);
                formula.val(newval);
            }
        });

        $("#formatcell").click(function()
        {
            format_win.toggle();
            select_frame(format_win,(format_win.find(".section_list li:first a").text()).toLowerCase());
        });

        var dialogs = $(".dialog");

        dialogs.css("top",(root.offset().top+50)+"px");
        dialogs.css("left",(root.offset().left+50)+"px");

        dialogs.each(function()
        {
            var d = $(this);

            $("<div class='close' />").appendTo(d).click(function()
            {
                d.toggle();
            });

            d.find(".section_list a").click(function()
            {
                select_frame(d,$(this).text().toLowerCase());
            });

            var cat = (d.find(".section_list li:first a").text()).toLowerCase();
            select_frame(d,cat);

            d.draggable(
            {
                handle:"h2",
                containment:'document',
                cursor : "move",
                start  : function(event, ui) {
                     $(this).css('cursor','move');
                },
                stop  : function(event, ui) {
                     $(this).css('cursor','default');
                }
            });
        });
   };

    /**
     */
    var add_function_to_menu = function(root,category,func,funtext)
    {
        var dialog = root.find("#functions");

        var c = category.toLowerCase();
        var cat = dialog.find(".section_list li."+c);
        var list = null;

        if(cat.size() == 0)
        {
            cat = $("<li class='"+c+"'><a>"
                +category+"</a></li>").appendTo(dialog.find(".section_list"));

            cat.find("a").click(function()
            {
                select_frame(dialog,c);
            });

            list = $("<ul class='"+c+"' />").appendTo(
                dialog.find(".content_list"));
        }
        else
        {
            list = dialog.find(".content_list:first > ul."+c);
        }

        var link = $("<li><a>"+func+"</a></li>").appendTo(list);

        link.click(function()
        {
            dialog.find("#curfunction").val(funtext);
        });
    };

    /**
     * Set the width of a column
     */
    var set_width = function(root,col,width)
    {
        var column = $.fn.from_b26(col.toLowerCase());
        var td = root.find(".data").find("tr td:nth-child("+column+")");

        root.find(".columns tr th:nth-child("+column+")").width(width);

        var total = root.find("div.data table").width() + (width - td.width());
        root.find(".data table").width(total);
        root.find(".columns .roottbl , .columns table").width(total);

        td.width(width);
        td.find("div").width(width-6);
    };

    var cell = function(root,x,y)
    {
      return $(root.children("tbody")[0].childNodes[y].childNodes[x]);
    }

    var cell_div = function(root,x,y)
    {
        return cell(root,x,y).children("div");
    }

    /**
     * Entry point
     */
    return this.each(function()
    {
        var data = $.data(this,"spreadsheet");

        if(typeof data == "undefined")
        {
            $.data(this,"spreadsheet",{});

            if(opts.fullscreen)
            {
                $("body").css("overflow","hidden");
            }

            var $this = $(this);
            var range = $.fn.parse_cell(opts.range.split(":")[1]);
            var names = [];

            $this.addClass("spreadsheet");

            create_table($this,range[1],range[0]);

            var $sel = new Selection($this);
          $this.range = range;
            setup_rowcol_selection($this,$sel,range[1]-1,range[0]-1);

            write_formula_bar($this,$sel);
            write_button_menu($this,$sel,$.fn.toolbar);
            set_display_values($this,range[0],opts.fmargin);
            init_dialogs($this,$sel);

            $("#name").textbox(
            {
                onChange : function(item)
                {
                    if($sel.state != $.fn.states.INIT)
                    {
                        var range = $.fn.to_b26($sel.startx+1) + "" + ($sel.starty+1)
                            + ":"+$.fn.to_b26($sel.endx+1)+""+($sel.endy+1);

                        addName($("#name"),names,range,item);

                        if(typeof opts.setName == "function")
                            opts.setName(range,item);
                    }
                },
                onSelect : function(item)
                {
                    var start = $.fn.parse_cell((names[item]).split(":")[0]);
                    var end =   $.fn.parse_cell((names[item]).split(":")[1]);

                    var s = cell_div($sel.tbl,(start[0]-1),(start[1]-1))[0];
                    $sel.start($.fn.addr.CELL,s);

                    var e = cell_div($sel.tbl,(end[0]-1),(end[1]-1))[0];
                    $sel.hover(e);
                    $sel.end(e);
                }
            });

            $(window).resize();

            $.data(this,"spreadsheet",{names:names});
        }
        // The plugin has already been created on this object
        // must be an external call to modify
        else if(args[0] == "addName")
            addName($("#name"),data.names,args[1],args[2]);

        else if(args[0] == "setWidth")
            set_width($(this),args[1],args[2]);

        else if(args[0] == "setHeight")
            set_height($(this),args[1],args[2]);

        else if(args[0] == "setValue")
        {
            var ref = $.fn.parse_cell(args[1]);

            if(ref[1] > 50) // nasty quick hack
              return;


            var tbl = $(this).find(".data table");
            if(typeof args[2] == "string")
            {
                cell_div(tbl,ref[0]-1,ref[1]-1).text(args[2]);
            }
            else
            {
                switch(args[2][0])
                {
                    case "matrix" :
                        break;
                    case "integer" :
                    case "float" :
                    case "string" :
                    case "boolean" :
                        cell_div(tbl,ref[0]-1,ref[1]-1).text(args[2][1][0]);
                        break;
                }
            }
        }
        else if(args[0] == "setStyle")
        {
            var ref = $.fn.parse_cell(args[1]);

            if(ref[1] > 50) // nasty quick hack
              return;

            var tbl = $(this).find(".data table");

            var cell = cell_div(tbl,ref[0]-1,ref[1]-1);
			cell.css(args[2],args[3]);
        }

        else if(args[0] == "addFunction")
        {
            add_function_to_menu($(this),args[1],args[2],args[3]);
        }
    });
};

/**
 * Returns the x/y coordinated of a cell from
 * within a table
 */
$.fn.cell_index = function(cell)
{
    var off = $.browser.msie ? 1 : 0;
    return [cell.parentNode.cellIndex,
        cell.parentNode.parentNode.rowIndex];
}

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

    var is_fun = function(x)
    {
        return (typeof x == "function");
    };


    /**
     * Replaces a cell with an input element to enter cell values,
     * called on cell focus, removes input on blur
     */
    var offset = function(key,x,y)
    {
        switch(key)
        {
        case $.key.UP     : return [x,y-1]; break;
        case $.key.RIGHT  : return [x+1,y]; break;
        case $.key.LEFT   : return [x-1,y]; break;
        case $.key.DOWN   :
        case $.key.ENTER : return [x,y+1];  break;
        default : return false; break;
        }
    };

})(jQuery);
