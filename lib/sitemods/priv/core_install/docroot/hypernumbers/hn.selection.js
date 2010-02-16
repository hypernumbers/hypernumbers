HN.Layout.Cell = function(selection)
{
  this.selection = selection;
  this.layout    = selection.layout;
  this.cell      = selection.cell;
  this.input     = HN.Util.id("input");
  this.hidden    = HN.Util.id("hidden_input");
};

HN.Layout.Cell.prototype.calculateWidth = function()
{
    var input  = this.input;
    var hidden = this.hidden;
    var sheet  = this.layout.s;
    var index  = 0;
    var val    = null;
    var tmp    = 0;
    var fn     = null;

    hidden.innerHTML = input.value;

    if( this.input.style.whiteSpace == "normal" ) {
        var xy = Y;
        var t      = (sheet.col_width(this.selection.cell.x)-2) +"px";
        val                    = hidden.clientHeight + 5;
        fn                     = "row_height";
        hidden.style.width     = t;
        this.input.style.width = t;
     } else {
         var xy = X;
         val    = hidden.clientWidth+5;
         fn     = "col_width";
         this.input.style.height =
             (sheet.row_height(this.selection.cell.y)-2)+"px";
     }
    
     do {
         tmp += sheet[fn](this.selection.cell[xy.str] + index);
         this.input.style[xy.dimension] = (tmp-2) + "px";
         index++;
     } while( val > tmp );
};

HN.Layout.Cell.prototype.start_edit = function(val)
{
    var cell      = this.selection.cell;
    var dom       = this.dom;
    var layout    = this.layout;
    var sheet     = layout.s;
    var cell_attr = sheet.cell(cell.y, cell.x);
    var cell_pos  = sheet.cell_offset(cell.y, cell.x);
    
    var style   = "background:#FFF;"+sheet.get_style_by_cell(cell)
         +"text-align:left; display:block;outline:none;resize: none;";
    
    this.selection.cell_dom.innerHTML = "";
    this.hidden.setAttribute("style", style);
    
    var inp   = this.input;
    var input = inp.cloneNode(true);
    var s     = input.style;
    
    this.input = input;
    
    input.value = val;
    input.setAttribute("style", style);
    s.width  = "0px";
    s.height = (sheet.row_height(cell.y)-2) + "px";
    s.top    = (cell_pos.top-1)  + "px";
    s.left   = (cell_pos.left+0) + "px";
    
    this.calculateWidth();
    inp.parentNode.replaceChild(input, inp);
    input.focus();
};

HN.Layout.Cell.prototype.end_edit = function(val)
{
    if( this.selection.in_formula_bar ) {
        this.selection.in_formula_bar = false;
    }
    if( this.selection.formula ) {
        this.selection.formula.blur();
    }
    
    this.input.blur();
    this.input.style.display = "none";
    
    HN.Util.id("dragfill").style.display = "none";
    this.selection.start_drag = false;
    
    var c = this.layout.s.cell(this.selection.cell.y, this.selection.cell.x);
    
    if( this.selection.is_light_edit() ) {
        this.selection.cell_dom.innerHTML = this.input.value;
        if( c ) {
            c.value = this.input.value;
        }
        this.layout.s.data.poke_cell_value(this.selection.cell, 
                                            this.input.value);
        HN.Callbacks.set_cell(this.layout.s.data.data.path, 
                              this.selection.cell, this.input.value);
    } else if( this.selection.is_full_edit() ) {
        var formula = (c && c.formula) || "";
        if( this.input.value != formula ) {
            this.selection.cell_dom.innerHTML = this.input.value;
            if( c ) {
                c.value = this.input.value;
            }
            this.layout.s.data.poke_cell_value(this.selection.cell, 
                                               this.input.value);
            HN.Callbacks.set_cell(this.layout.s.data.data.path,
                                  this.selection.cell, this.input.value);
        } else {
            this.selection.cell_dom.innerHTML = (c && c.value) || "";
        }
    }
};

HN.Layout.Cell.prototype.is_formula = function(val)
{
    return this.input.value[0] == "=";
};

HN.Layout.Cell.prototype.insert_range = function(range)
{
    var f = this.input.value;
    var index = Math.max( f.lastIndexOf("="), f.lastIndexOf(","),
                          f.lastIndexOf("("), f.lastIndexOf("+"),
                          f.lastIndexOf("-"),
                          f.lastIndexOf("*"), f.lastIndexOf("/") );
    var formula = f.substring(0, index+1) + HN.Util.range_to_str2(range);
    this.input.value = formula;
    if( this.selection.formula ) {
        this.selection.formula.value = formula;
    }
};

HN.Layout.Cell.prototype.read_formula_bar = function(range)
{
  var that = this;
  setTimeout(function() {
    that.input.value = that.selection.formula.value;
    that.calculateWidth();
  },0);
};

HN.Layout.Selection = function(layout)
{
    this.state      = HN.States.SELECTED_CELL;
    this.layout     = layout;
    this.formula    = HN.Util.id("formula");
    this.wrapper    = HN.Util.id("selection");
    this.dragger    = HN.Util.id("dragger");
    this.dragged    = HN.Util.id("dragged");
    this.start_drag = false;
    this.tmpbounds  = {};

    this.dom = {
        rowselection : HN.Util.id("rowselection"),
        colselection : HN.Util.id("colselection"),
        name         : HN.Util.id("name"),
        current      : HN.Util.id("current")
    };

    this.has_scrolled   = false;
    this.last_click     = null;
    this.cell_dom       = null;
    this.cell           = null;
    this.in_formula_bar = false;
    this.copied_str     = "";
    this.copied         = {x1:1, y1: 1, x2: 1, y2: 1};
    this.bounds         = {x1:1, y1: 1, x2: 1, y2: 1};
    this.dragpos        = {x1:0, y1: 0, x2: 0, y2: 0};
    this.offset  = {
        y : HN.Util.y_pos(this.layout.inner),
        x : HN.Util.x_pos(this.layout.inner)
    };

    this.c = new HN.Layout.Cell(this);

    var that    = this;
    var columns = $("#columns")[0];
    var rows    = $("#rows")[0];


    // Should probably make this some fancy dispatcher
    HN.Util.addEvent(document, "mousedown", function(e) {
        if( e.target.getAttribute("id") == "dragger" ) {
            that.dragdown(e);
        } else {
            if( HN.Util.is_inside(e.target, columns)
                || $(e.target).attr("id") == "colselection") {
                that.mouseDownAxis(e, X);
            } else if( HN.Util.is_inside(e.target, rows)
                       || $(e.target).attr("id") == "rowselection") {
                that.mouseDownAxis(e, Y);
            } else if( $(e.target).attr("id") == "formula") {
                that.in_formula_bar = true;
            } else if( $(e.target).attr("id") == "corner") {
                that.selectFullSheet();
            } else if( e.target.getAttribute("id") == "use-range" ){
                return;
            } else {
                that.mousedown(e);
            }
        }
    });

    HN.Util.addEvent(layout.inner, "dblclick", function(e) {
        that.dblclick(e);
    });

    HN.Util.addEvent(layout.inner, "contextmenu", function(e) {
        that.gridContextMenu(e);
    });

    var add_axis_events = function(id, axis) {
        HN.Util.addEvent(id, "contextmenu", function(e) {
            that.axisContextMenu(e, axis);
        });
    };

    add_axis_events(HN.Util.id("columnsout"), X);
    add_axis_events(HN.Util.id("rowsout"), Y);

    HN.Util.addEvent(document, "keydown", function(e) {
        that.keydown(e);
    });

    this.set_cell(1, 1);
    this.show_selection();
};

HN.Layout.Selection.prototype.selectFullSheet = function() {
    this.bounds = {"x1":1, "y1":1,
                   "x2":this.layout.s.max.x,
                   "y2":this.layout.s.max.y};
    this.show_selection();
};

HN.Layout.Selection.prototype.clearMenu = function(e, type)
{
  HN.Callbacks.clear(this.bounds, type);
};

HN.Layout.Selection.prototype.gridContextMenu = function(e)
{
    var that = this;

    var el = HN.Util.id("gridcontextmenu");

    el.style.display = "block";
    el.style.top     = e.clientY+"px";
    el.style.left    = e.clientX+"px";

    var hide = function(e) {
        var target = ( e.target.nodeName != "A" )
            ? $(e.target).parents("a")[0] : e.target;

        if( that.menuItemPicked(target) ) {
            HN.Util.removeEvent(document, "mousedown", hide);
            el.style.display = "none";
        }
    };

    HN.Util.addEvent(document, "mousedown", hide);

    e.preventDefault();
    return false;
};

HN.Layout.Selection.prototype.axisContextMenu = function(e, axis)
{
    var that = this;

    var el = (axis == Y)
        ? HN.Util.id("axiscontextmenurow")
        : HN.Util.id("axiscontextmenucol");
    el.style.display = "block";
    el.style.top     = e.clientY+"px";
    el.style.left    = e.clientX+"px";

    var hide = function(e) {
        if( that.menuItemPicked(e.target) ) {
            HN.Util.removeEvent(document, "mousedown", hide);
            el.style.display = "none";
        }
    };

    HN.Util.addEvent(document, "mousedown", hide);

    e.preventDefault();
    return false;
};

HN.Layout.Selection.prototype.mouseDownAxis = function(e, xy)
{
    e.preventDefault();

    if( e.target.className == "handle" && e.button != 2 ) {

        var that    = this;
        var attr    = (xy == Y) ? "clientY" : "clientX"
        var start   = e[attr];
        var handle  = e.target;
        var new_val = 0;
        var cell    = handle.parentNode;
        var orig    = parseInt(cell.style[xy.dimension], 10);
        var index   = (xy == Y)
            ? parseInt(cell.textContent, 10)
            : HN.Util.from_b26(cell.textContent);
        var axis    = (xy == Y) ? HN.Util.id("rsel") : HN.Util.id("csel");
        
        axis.style.display           = "none";
        handle.style.backgroundColor = "#FF9966";
        document.body.style.cursor   = (xy == Y) ? "ns-resize" : "ew-resize";
        HN.Util.id("overlay").style.display = "block";


        var mouseup = function( e ) {

            HN.Util.id("overlay").style.display = "none";
            handle.style.backgroundColor        = "";
            document.body.style.cursor          = "";
            axis.style.display                  = "block";
            HN.Util.removeEvent(document, "mouseup", mouseup);
            HN.Util.removeEvent(document, "mousemove", mousemove);

            if( xy == Y ) {
                HN.Callbacks.setHeight(that.layout.s.data.data.path, 
                                       index, new_val+4);
            } else {
                HN.Callbacks.setWidth(that.layout.s.data.data.path,
                                      index, new_val+4);
            }
        };

        var mousemove = function( e ) {
            new_val = orig + (e[attr] - start);
            if( new_val > 0 ) {
                that.shiftAxis(cell, xy, index, (e[attr] - start));
                cell.style[xy.dimension] =  new_val + "px";
            }
        };

        HN.Util.addEvent(document, "mouseup", mouseup);
        HN.Util.addEvent(document, "mousemove", mousemove);

    } else {

        var that = this;
        var attr     = (xy == Y) ? "clientY" : "clientX";
        var tmp      = parseInt(this.layout.inner.style[xy.coord], 10);
        var offset   = (xy == Y) 
            ? HN.Util.y_pos($("#scroller")[0])
            : HN.Util.x_pos($("#scroller")[0]);
        var max      = (xy == Y) ? this.layout.s.max.x : this.layout.s.max.y;
        var b        = this.order_bounds(this.bounds);
        var start    = that.getNewOffset(e[attr] - offset, xy);

        if( e.button == 2 &&
            ((xy == Y && start.index >= b.y1
              && start.index <= b.y2 && b.x1 == 1) ||
             (xy == X && start.index >= b.x1
              && start.index <= b.x2 && b.y1 == 1))) {
            return;
        }

        document.body.style.cursor = (xy == Y) ? "e-resize" : "s-resize";
        HN.Util.id("overlay").style.display = "block";

        if( xy == Y ) {
            that.bounds = {"y1":start.index, "x1":1, 
                           "y2":start.index, "x2":max};
            that.set_cell(start.index, 1);
        } else {
            that.bounds = {"x1":start.index, "y1":1, 
                           "x2":start.index, "y2":max};
            that.set_cell(1, start.index);
        }

        that.show_selection();

        var mouseup = function( e ) {
            HN.Util.id("overlay").style.display = "none";
            document.body.style.cursor = "";

            HN.Util.removeEvent(document, "mouseup", mouseup);
            HN.Util.removeEvent(document, "mousemove", mousemove);
        };

        var mousemove = function( e ) {
            var end = that.getNewOffset(e[attr] - offset, xy);

            if( xy == Y ) {
                that.bounds = {"y1":Math.min(start.index, end.index), "x1":1,
                               "y2":Math.max(start.index, end.index), "x2":max};

            } else {
                that.bounds = {"x1":Math.min(start.index, end.index), "y1":1,
                               "x2":Math.max(start.index, end.index), "y2":max};
            }
            that.show_selection();
            e.preventDefault();
            return false;
        };

        HN.Util.addEvent(document, "mouseup", mouseup);
        HN.Util.addEvent(document, "mousemove", mousemove);
    }
};

HN.Layout.Selection.prototype.shiftAxis = function(element, axis, index, diff)
{
    while( element.nextSibling && ++index) {
        element = element.nextSibling;
        var tmp = (axis == Y)
            ? this.layout.s.cell_offset(index, 1)
            : this.layout.s.cell_offset(1, index);
        element.style[axis.coord] = (tmp[axis.coord] + diff) + "px";
    }
};

HN.Layout.Selection.prototype.is_viewable = function(y, x)
{
    var ret   = {"x":true, "y":true};
    var coord = this.layout.s.cell_offset(y, x);
    var top   = Math.abs(parseInt(this.layout.inner.style.top, 10));
    var left  = Math.abs(parseInt(this.layout.inner.style.left, 10));
   
    if( coord.top < top || (coord.top+this.layout.s.row_height(y)) 
        > top + this.layout.size.y ) {
        ret.y = false;
    }

    if( coord.left < left || (coord.left+this.layout.s.col_width(x)) 
        > left + this.layout.size.x ) {
        ret.x = false;
    }

    return ret;
};

HN.Layout.Selection.prototype.scrollIntoView = function(y, x, dir)
{
    var tmp = this.is_viewable(y, x);

    if( tmp.x && tmp.y ) {
        this.has_scrolled = false;
        return;
    }

    if( !tmp.x && dir === true ) {
        this.layout.panes.p[0][0].col = x;
        this.layout.panes.redraw();
    } else if ( !tmp.x ) {
        this.layout.scrollBy1(dir == HN.RIGHT, X);
        this.scrollIntoView(y, x, dir);
    }

    if( !tmp.y && dir === true ) {
        this.layout.panes.p[0][0].row = y;
        this.layout.panes.redraw();
    } else if ( !tmp.y ) {
        this.layout.scrollBy1(dir == HN.DOWN, Y);
        this.scrollIntoView(y, x, dir);
    }
    this.has_scrolled = false;
};

HN.Layout.Selection.prototype.set_cell = function(y, x, dir)
{
    var sheet = this.layout.s,
    current   = this.dom.current.style;

    this.cell = {"y":y, "x":x};

    if( dir ) {
        var arg = (!this.has_scrolled) ? dir : this.has_scrolled;
        this.scrollIntoView(y, x, arg);
    }

    this.cell_dom = this.layout.panes.get_cell(y, x);

    var cell_attr = sheet.cell(y, x),
    cell_pos      = sheet.cell_offset(y, x),
    formula       = (cell_attr.formula) || "";

    if( this.formula ) {
        this.formula.value = formula;
    }

    current.top    = (cell_pos.top-1)  + "px";
    current.left   = (cell_pos.left-1) + "px";
    current.width  = (sheet.col_width(x)-1) + "px";
    current.height = (sheet.row_height(y)-1) + "px";
};

HN.Layout.Selection.prototype.canEdit = function()
{
    return true;
};

HN.Layout.Selection.prototype.keydown = function(e)
{
    if( this.state == HN.States.NOT_EDITING
        || this.state == HN.States.COPY_URL ) {
        return;
    }

    if( e.keyCode == HN.Keys.BACKSPACE && this.is_selected()
        && !this.in_formula_bar ) {
        e.preventDefault();
    }

    this.do_keydown(e);

    if(!(e.keyCode == HN.Keys.DOWN || e.keyCode == HN.Keys.UP || 
         e.keyCode == HN.Keys.TAB  || e.keyCode == HN.Keys.RETURN || 
         e.keyCode == HN.Keys.LEFT || e.keyCode == HN.Keys.RIGHT) ) {
        return;
    }

    var that    = this;
    var timeout = null;
    var timer   = null;

    function cleanup() {
        window.clearTimeout(timeout);
        HN.Util.removeEvent(document, "keyup", cleanup);
        HN.Util.removeEvent(window, "blur", cleanup);
        clearInterval(timer);
    };

    var set_interval = function() {
        timer = setInterval(function() {
            that.do_keydown(e);
        }, 30);
    };

    HN.Util.addEvent(document, "keyup", cleanup);
    HN.Util.addEvent(window, "blur", cleanup);

    timeout = window.setTimeout(set_interval, 300);
};

HN.Layout.Selection.prototype.do_keydown = function(e)
{
    var that = this;

    if( this.in_formula_bar ) {

        if(e.keyCode == HN.Keys.ENTER || e.keyCode == HN.Keys.TAB) {
            this.select(e.keyCode, e);
        } else {
            if( this.is_selected() ) {
                this.startEditing();
                this.formula.focus();
            }
            this.c.read_formula_bar();
        }
    }
    else if( (e.keyCode == HN.Keys.ENTER || e.keyCode == HN.Keys.TAB)
               && (this.is_editing() || this.is_selected()) )
    {
        if( this.is_cell() ) {
            this.select(e.keyCode, e);
        } else {
            this.select_next(e.keyCode, e);
        }
    }
    else if( (this.is_light_edit() || this.is_selected())
             && (e.keyCode == HN.Keys.UP || e.keyCode == HN.Keys.DOWN
                 || e.keyCode == HN.Keys.LEFT || e.keyCode == HN.Keys.RIGHT) )
    {
        e.preventDefault();
        this.select(e.keyCode, e);
    }
    else if(this.canEdit() && this.is_selected()
            && (e.keyCode == HN.Keys.DELETE || e.keyCode == HN.Keys.BACKSPACE))
    {
        this.bounds = this.order_bounds(this.bounds);
        HN.Callbacks.clear(this.layout.s.data.data.path, 
                           this.bounds, "contents");
    }
    else if( this.is_selected() ){

        if( (e.ctrlKey || e.metaKey) && e.keyCode == 86  && this.canEdit()) {

            var inp = HN.Util.id("paste");
            inp.value = "";
            inp.focus();

            setTimeout( function() {
                that.pasteText(inp.value);
                inp.blur();
            },0);

            // Copy
        } else if( (e.ctrlKey || e.metaKey) && e.keyCode == 67 ) {
            var str = this.copy();
            var inp = HN.Util.id("paste");
            inp.value = str;
            inp.select();
            inp.focus();
            setTimeout( function() {
                inp.blur();
            },0);
        }

        else if ( HN.Util.is_char(e.keyCode) && 
                  this.canEdit() && 
                  !(e.ctrlKey || e.metaKey)) {
            this.scrollIntoView(this.cell.y, this.cell.x, true);
            that.startEditing("");
        }
    }
    else if( this.is_editing() ) {
        setTimeout(function() {
            if( that.formula ) {
                that.formula.value = that.c.input.value;
            }
            that.c.calculateWidth();
        },0);
    }
};

HN.Layout.Selection.prototype.select = function(key, event)
{
    if( event.shiftKey || key == HN.Keys.TAB ) {
        event.preventDefault();
    }

    var b    = this.tmpbounds;
    var that = this;
    var cell = HN.Util.clone(this.cell);
    var dir  = null;

    var select_cell = function() {
        that.tmpbounds = HN.Util.clone(b);
        that.bounds    = HN.Util.clone(b);
        that.set_cell(b.y1, b.x1, dir);
        that.state = HN.States.SELECTED_CELL;
        that.show_selection();
    };

    var select_range = function() {
        that.tmpbounds    = HN.Util.clone(b);
        that.bounds       = HN.Util.clone(b);
        that.has_scrolled = true;
        that.state        = HN.States.SELECTED_RANGE;
        that.show_selection();
    };

    if( this.is_editing() ) {
        if( this.c.is_formula() &&
            (key != HN.Keys.ENTER && key != HN.Keys.TAB) ) {
            
            select_cell = function() {

                var arg = (!that.has_scrolled) ? dir : that.has_scrolled;
                that.scrollIntoView(b.y1, b.x1, arg);

                var tmp = that.tmpbounds = b;
                that.c.insert_range(tmp);
                that.show_bounds(HN.Util.id("dragfill"), tmp);
            };

            select_range = function() {
                var tmp = that.tmpbounds = b;
                that.c.insert_range(tmp);
                that.show_bounds(HN.Util.id("dragfill"), tmp);
            };

            if( !this.start_drag ) {
                this.start_drag = true;
                b = { "x1":b.x1, "y1":b.y1, "x2":b.x1, "y2":b.y2 };
            }

            cell = { "x":b.x1, "y":b.y1 };

        } else {
            this.c.end_edit();
        }
    }

    if( !event.shiftKey ) {

        if( key == HN.Keys.ENTER || key == HN.Keys.DOWN ) {
            b.y1 = b.y2 = cell.y+1;
            b.x1 = b.x2 = cell.x;
            dir = HN.DOWN;
        } else if( key == HN.Keys.UP && cell.y > 1 ) {
            b.y1 = b.y2 = cell.y-1;
            b.x1 = b.x2 = cell.x;
            dir = HN.UP;
        } else if( key == HN.Keys.LEFT && cell.x > 1 ) {
            b.y1 = b.y2 = cell.y;
            b.x1 = b.x2 = cell.x-1;
            dir = HN.UP;
        } else if( key == HN.Keys.TAB || key == HN.Keys.RIGHT ) {
            b.y1 = b.y2 = cell.y;
            b.x1 = b.x2 = cell.x+1;
            dir = HN.RIGHT;
        }

        if( dir ) {
            select_cell();
        }

    } else {

        if( key == HN.Keys.DOWN && cell.y == b.y2 && b.y1 != b.y2 ) {
            b.y1++;
            this.scrollIntoView(b.y1, b.x1, HN.DOWN);
        } else if( key == HN.Keys.DOWN ) {
            b.y2++;
            this.scrollIntoView(b.y2, b.x2, HN.DOWN);
        } else if( key == HN.Keys.UP && cell.y == b.y1 && b.y2 > 1 ) {
            b.y2--;
            this.scrollIntoView(b.y2, b.x2, HN.UP);
        } else if( key == HN.Keys.RIGHT && cell.x == b.x2 && b.x1 != b.x2 ) {
            b.x1++;
            this.scrollIntoView(b.y1, b.x1, HN.RIGHT);
        } else if( key == HN.Keys.RIGHT ) {
            b.x2++;
            this.scrollIntoView(b.y2, b.x2, HN.RIGHT);
        } else if( key == HN.Keys.LEFT && this.cell.x == b.x1 && b.x2 > 1 ) {
            b.x2--;
            this.scrollIntoView(b.y2, b.x2, HN.LEFT);
        }

        select_range();
    }
};

HN.Layout.Selection.prototype.select_next = function(key, event)
{
    this.c.end_edit();

    var cell = this.cell;
    var b    = this.order_bounds(this.bounds);

    if( key == HN.Keys.ENTER ) {
        if( cell.y < b.y2 ) {
            this.set_cell(cell.y+1, cell.x, HN.DOWN);
        } else if( cell.y == b.y2 && cell.x == b.x2 ) {
            this.has_scrolled = true;
            this.set_cell(b.y1, b.x1);
        } else {
            this.has_scrolled = true;
            this.set_cell(b.y1, cell.x+1, HN.RIGHT);
        }
    }
    else if( key == HN.Keys.TAB )
    {
        event.preventDefault();

        if( cell.x < b.x2 ) {
            this.set_cell(cell.y, cell.x+1, HN.RIGHT);
        } else if( cell.y == b.y2 && cell.x == b.x2 ) {
            this.has_scrolled = true;
            this.set_cell(b.y1, b.x1);
        } else {
            this.has_scrolled = true;
            this.set_cell(cell.y+1, b.x1, HN.DOWN);
        }
    }
    this.state = HN.States.SELECTED_RANGE;
};

HN.Layout.Selection.prototype.editPasteBounds = function(b, r2)
{
    if(b.x1 == b.x2 && b.y1 == b.y2) {
        b.x2 = b.x1 + (r2.x2 - r2.x1);
        b.y2 = b.y1 + (r2.y2 - r2.y1);
        this.show_selection();
    } else if(
        ((b.x2 - b.x1 + 1) % (r2.x2 - r2.x1 + 1)) != 0 ||
            ((b.y2 - b.y1 + 1) % (r2.y2 - r2.y1 + 1)) != 0) {
        b.x2 = b.x1 + (r2.x2 - r2.x1);
        b.y2 = b.y1 + (r2.y2 - r2.y1);
        this.show_selection();
    }
};

HN.Layout.Selection.prototype.paste = function(copied)
{
    this.state = HN.States.SELECTED_RANGE;
    HN.Callbacks.pasteRange(this.layout.s.data.data.path, this.bounds, 
                            copied.host+copied.path, 
                            copied.bounds);
    this.editPasteBounds(this.bounds, copied.bounds);
};

HN.Layout.Selection.prototype.copy = function()
{
    $(".paste").removeClass("disabled");

    var str = this.build_clipboard_str();

    localStorage.copied = escape(JSON.stringify({
        "bounds" : this.bounds,
        "host"   : this.layout.s.data.data.host,
        "path"   : hn.currentPath(),
        "values" : str
    }));

    this.copied = HN.Util.clone(this.bounds);
    this.show_bounds(HN.Util.id("copied"), this.copied);

    return str;
};

HN.Layout.Selection.prototype.pasteText = function(val)
{
    var copied = JSON.parse(unescape(localStorage.copied));

    // if the text pasted (ctrl+v) is the same as what was copied
    // use "proper" paste
    if( copied && val == copied.values ) {
        this.paste(copied);
        return;
    }

    if(val.match("\t") === null && val.match("\n") === null) {
        this.startEditing(val);
    } else {
        var lines = val.split("\n");
        for( var i = 0; i < lines.length; i++) {
            lines[i] = lines[i].split("\t");
            if(lines[i][lines[i].length] == "") {
                lines[i].length--;
            }
        }
        if( lines[lines.length-1] == "") {
            lines.length--;
        }

        this.state = HN.States.SELECTED_RANGE;
        HN.Callbacks.pasteValues(this.layout.s.data.data.path, this.bounds,lines);
        var tmp = {"x1":1, "y1":1, "x2":lines[0].length, "y2":lines.length};
        this.editPasteBounds(this.bounds, tmp);
    }
};

HN.Layout.Selection.prototype.getNewOffset = function(position, xy)
{
    var i     = 1;
    var total = 0;
    var s     = this.layout.s;
    var f     = ( xy == Y ) ? "row_height" : "col_width";
    
    while( true ) {
        total += s[f](i);
        
        if( total > position ) {
            return {"index":i, "position":total-s[f](i)};
        }

        ++i;
    };
};

HN.Layout.Selection.prototype.startEditing = function(val)
{
    this.c.start_edit(val);

    HN.Util.id("copied").style.display = "none"; 

    this.state = (this.state == HN.States.SELECTED_RANGE)
        ? HN.States.EDIT_LIGHT_RANGE
        : HN.States.EDIT_LIGHT_CELL;
};

HN.Layout.Selection.prototype.is_in_grid = function(auto, e)
{
    var axis = auto.args.axis,
    dir      = auto.args.forward,
    x        = e.clientX,
    y        = e.clientY;

    return (axis == Y && dir  && !(y > this.offset.y + this.layout.size.y))
        || (axis == X && dir  && !(x > this.offset.x + this.layout.size.x))
        || (axis == Y && !dir && !(y < this.offset.y))
        || (axis == X && !dir && !(x < this.offset.x));
};

HN.Layout.Selection.prototype.auto_scroll_coords = function(e)
{
    if( e.clientY > this.offset.y + this.layout.size.y) {
        return {"forward":true, "axis":Y};
    } else if( e.clientY < this.offset.y ) {
        return {"forward":false, "axis":Y};
    } else if( e.clientX > this.offset.x + this.layout.size.x) {
        return {"forward":true, "axis":X};
    } else if( e.clientX < this.offset.x ) {
        return {"forward":false, "axis":X};
    }
    return false;
};

HN.Layout.Selection.prototype.dragdown = function(e)
{
    e.preventDefault();

    var that  = this;
    var sheet = this.layout.s;
    var style = this.layout.inner.style;
    var auto  = {"timer":null, "axis":null};
    var offy  = this.offset.y + parseInt(style.top, 10);
    var offx  = this.offset.x + parseInt(style.left, 10);

    this.state        = HN.States.DRAGGING;
    this.has_scrolled = true;

    var auto_scroll = function() {
        that.layout.scrollBy1(auto.args.forward, auto.args.axis);
        offy  = that.offset.y + parseInt(style.top, 10),
        offx  = that.offset.x + parseInt(style.left, 10);
        var y = that.getNewOffset(auto.mouse.y - offy, Y);
        var x = that.getNewOffset(auto.mouse.x - offx, X);
        that.show_drag_selection({x:x.index, y:y.index});
    };

    var stop_auto_scroll = function() {
        window.clearInterval(auto.timer);
        auto = {timer:null, axis:null, forward:null, mouse:null};
    };

    var mousemove = function( e ) {
        if( !auto.timer ) {
            var args = that.auto_scroll_coords(e);
            if( args ) {
                auto = { args:  args,
                         mouse: {y:e.clientY, x:e.clientX},
                         timer: setInterval(auto_scroll, 30) };
            } else {
                var y = that.getNewOffset(e.clientY - offy, Y);
                var x = that.getNewOffset(e.clientX - offx, X);
                that.show_drag_selection({x:x.index, y:y.index});
            }
        }
        else if( that.is_in_grid(auto, e) ) {
            stop_auto_scroll();
        }
    };

    var mouseup = function(e) {

        if( auto.timer ) {
            window.clearInterval(auto.timer);
        }

        HN.Util.removeEvent(document, "mouseup", mouseup);
        HN.Util.removeEvent(document, "mousemove", mousemove);
        
        HN.Callbacks.drag(that.layout.s.data.data.path, 
                          that.order_bounds(that.bounds), that.dragpos);

        that.bounds = {
            y1: Math.min(that.bounds.y1, that.dragpos.y1),
            x1: Math.min(that.bounds.x1, that.dragpos.x1),
            y2: Math.max(that.bounds.y2, that.dragpos.y2),
            x2: Math.max(that.bounds.x2, that.dragpos.x2)
        };

        that.resumeSelection();

        that.show_selection();
        that.dragged.style.display = "none";
    };

    HN.Util.addEvent(document, "mouseup", mouseup);
    HN.Util.addEvent(document, "mousemove", mousemove);
};

HN.Layout.Selection.prototype.resumeSelection = function()
{
    this.state = (this.bounds.y1 == this.bounds.y2
                  && this.bounds.x1 == this.bounds.x2)
        ? HN.States.SELECTED_CELL
        : HN.States.SELECTED_RANGE;
};

HN.Layout.Selection.prototype.resume_editing = function()
{
    this.state = (this.bounds.y1 == this.bounds.y2
                  && this.bounds.x1 == this.bounds.x2)
        ? HN.States.EDIT_LIGHT_CELL
        : HN.States.EDIT_LIGHT_RANGE;
};


HN.Layout.Selection.prototype.dblclick = function(e)
{
    if( this.state == HN.States.NOT_EDITING || !this.canEdit() ) {
        return;
    }

    if( this.state != HN.States.EDIT_FULL_CELL ) {
        var val = this.layout.s.cell(this.cell.y, this.cell.x).formula || "";
        this.c.start_edit(val);
        this.state = HN.States.EDIT_FULL_CELL;
    }
};

HN.Layout.Selection.prototype.mousedown = function(e)
{
    if( $(e.target).parents("#grid").length == 0
        && $(e.target).attr("id") != "formula" ) {
        this.state = HN.States.NOT_EDITING;
        return;
    } else if ( this.state == HN.States.NOT_EDITING ) {
        this.resumeSelection();
    }

    // Dont interfere with right clicks on selections
    if( e.button == 2 && e.target == this.wrapper || 
        e.button == 2 && e.target == this.dom.current || 
        e.target.nodeName == "TEXTAREA") {
        return true;
    }

    // Let use click on links in cells
    if( e.target.nodeName == "A" && (e.ctrlKey || e.metaKey) ) {
        document.location.href = e.target.getAttribute("href");
    }

    e.preventDefault();

    var that   = this;
    var sheet  = this.layout.s;
    var style  = this.layout.inner.style;
    var auto   = {};
    var bounds = {};
    var offy   = this.offset.y + parseInt(this.layout.inner.style.top, 10);
    var offx   = this.offset.x + parseInt(this.layout.inner.style.left, 10);

    this.has_scrolled = true;

    // Stop double clicks being screwed with
    if( this.last_click && new Date().getTime() - 250 < this.last_click ) {
        return true;
    } else {
        this.last_click = new Date().getTime();
    }

    if( this.in_formula_bar ) {
        this.in_formula_bar = false;
        if( e.target == this.c.input ) {
            return true;
        }
    }

    var select = function() {
        that.bounds = bounds;
        that.show_selection();
    };

    var select_start = function() {
        that.bounds = bounds;
        that.set_cell(bounds.y1, bounds.x1);
        that.tmpbounds = bounds;
        that.show_selection();
    };

    var select_end = function() {
        that.resumeSelection();
    };

    if( this.is_editing() ) {
        if( this.c.is_formula() ) {
            select = function() {
                var tmp = that.order_bounds(bounds);
                that.c.insert_range(tmp);
                that.show_bounds(HN.Util.id("dragfill"), tmp);
            };
            select_start = function() {
                var tmp = that.order_bounds(bounds);
                that.tmpbounds = bounds;
                that.c.insert_range(tmp);
                that.show_bounds(HN.Util.id("dragfill"), tmp);
            };
            select_end = function() {
            };

        } else {
            this.c.end_edit();
        }
    }

    if( e.shiftKey ) {
        var y = that.getNewOffset(e.clientY - offy, Y);
        var x = that.getNewOffset(e.clientX - offx, X);
        bounds = { x1:this.tmpbounds.x1, x2:x.index,
                   y1:this.tmpbounds.y1, y2:y.index };
        select();
        setTimeout( function() { select_end(); }, 1000);

        return true;
    }

    var auto_scroll = function() {
        that.layout.scrollBy1(auto.args.forward, auto.args.axis);
        offy      = that.offset.y + parseInt(style.top, 10),
        offx      = that.offset.x + parseInt(style.left, 10);
        var y     = that.getNewOffset(auto.mouse.y - offy, Y);
        var x     = that.getNewOffset(auto.mouse.x - offx, X);
        bounds.y2 = y.index;
        bounds.x2 = x.index;
        select();
    };

    var stop_auto_scroll = function() {
        window.clearInterval(auto.timer);
        auto = {timer:null, axis:null, forward:null, mouse:null};
    };

    var y = this.getNewOffset(e.clientY - offy, Y);
    var x = this.getNewOffset(e.clientX - offx, X);

    bounds = {y1:y.index, x1:x.index, y2:y.index, x2:x.index};

    select_start();

    var mousemove = function( e ) {
        e.preventDefault();
        if( !auto.timer ) {
            var args = that.auto_scroll_coords(e);
            if( args ) {
                auto = { args:  args,
                         mouse: {y:e.clientY, x:e.clientX},
                         timer: setInterval(auto_scroll, 30) };
            } else {

                var y2 = that.getNewOffset(e.clientY - offy, Y);
                var x2 = that.getNewOffset(e.clientX - offx, X);

                bounds.y2 = y2.index;
                bounds.x2 = x2.index;

                select();
            }
        }
        else if( that.is_in_grid(auto, e) ) {
            stop_auto_scroll();
        }
    };

    var mouseup = function( e ) {

        if( auto.timer ) {
            window.clearInterval(auto.timer);
        }

        document.body.style.cursor = "";
        HN.Util.id("overlay").style.display = "none";
        that.mouse   = {x1:0, y1: 0, x2: 0, y2: 0};
        HN.Util.removeEvent(document, "mouseup", mouseup);
        HN.Util.removeEvent(document, "mousemove", mousemove);
        select_end();
    };
    document.body.style.cursor = "cell";
    HN.Util.id("overlay").style.display = "block";
    HN.Util.addEvent(document, "mouseup", mouseup);
    HN.Util.addEvent(document, "mousemove", mousemove);
};

HN.Layout.Selection.prototype.show_drag_selection = function(drag)
{
  var pos = function(d,b) {
    if( d.x >= b.x1 && d.x <= b.x2 && d.y > b.y2 )
      return [b.y2+1, b.x1, d.y, b.x2];
    if( d.x >= b.x1 && d.x <= b.x2 && d.y < b.y1 )
      return [d.y, b.x1, b.y1-1, b.x2];
    if( d.y >= b.y1 && d.y <= b.y2 && d.x > b.x2 )
      return [b.y1, b.x2+1, b.y2, d.x];
    if( d.y >= b.y1 && d.y <= b.y2 && d.x < b.x1 )
      return [b.y1, d.x, b.y2, b.x1-1];
    return null;
  };

  var npos = pos(drag, this.order_bounds(this.bounds));

  if( npos ) {
    this.dragpos.y1 = npos[0];
    this.dragpos.x1 = npos[1];
    this.dragpos.y2 = npos[2];
    this.dragpos.x2 = npos[3];
    this.draw_drag(npos[0], npos[1], npos[2], npos[3]);
  }
};

HN.Layout.Selection.prototype.draw_drag = function(top, left, bottom, right)
{
  var cell = this.layout.s.cell_offset(top, left);

  for(var x = left, width = 0; x < right+1; x++) {
    width += this.layout.s.col_width(x);
  }
  for(var y = top, height = 0; y < bottom+1; y++) {
    height += this.layout.s.row_height(y);
  }

  this.dragged.style.top     = (cell.top-1) + "px";
  this.dragged.style.left    = (cell.left-1) + "px";
  this.dragged.style.width   = (width-2) + "px";
  this.dragged.style.height  = (height-2) + "px";
  this.dragged.style.display = "block";
};

HN.Layout.Selection.prototype.order_bounds = function(b)
{
    return {
        x1: Math.min(b.x1, b.x2),
        x2: Math.max(b.x1, b.x2),
        y1: Math.min(b.y1, b.y2),
        y2: Math.max(b.y1, b.y2)
    };
};

HN.Layout.Selection.prototype.show_cell = function()
{
    var y     = this.cell.y,
    x         = this.cell.x,
    sheet     = this.layout.s,
    current   = this.dom.current.style,
    cell_attr = sheet.cell(y, x),
    cell_pos  = sheet.cell_offset(y, x),
    formula   = (cell_attr.formula) || "";

    if( this.formula ) {
        this.formula.value = formula;
    }

    current.top    = (cell_pos.top-1)  + "px";
    current.left   = (cell_pos.left-1) + "px";
    current.height = (sheet.row_height(y)-1) + "px";
    current.width  = (sheet.col_width(x)-1) + "px";
};

HN.Layout.Selection.prototype.show_selection = function()
{
    var sheet  = this.layout.s;
    var bounds = this.order_bounds(this.bounds);
    var coords = sheet.cell_offset(bounds.y1, bounds.x1);
    var dom    = this.dom
    var col    = dom.colselection;
    var row    = dom.rowselection;
    var startx = bounds.x1, endx = bounds.x2+1;
    var starty = bounds.y1, endy = bounds.y2+1;

    for( var x = startx, width = 0; x < endx; x++ ) {
        width += sheet.col_width(x);
    }

    for( var y = starty, height = 0; y < endy; y++ ) {
        height += sheet.row_height(y);
    }

    row.style.top    = (coords.top) + "px";
    row.style.height = (height-1) + "px";

    col.style.left   = (coords.left) + "px";
    col.style.width  = (width-1) + "px";

    if( dom.name ) {
        dom.name.innerHTML = HN.Util.range_to_str(bounds);
    }

    var str = (bounds.x1 == bounds.x2 && bounds.y1 == bounds.y2)
        ? HN.Util.to_b26(bounds.x1) + bounds.y2
        : HN.Util.range_to_str(bounds);

    var old     = this.wrapper;
    var wrapper = old.cloneNode(true);
    var s       = wrapper.style;

    this.wrapper = wrapper;

    s.left    = (coords.left-1) + "px";
    s.top     = (coords.top-1)  + "px";
    s.width   = (width-2)   + "px";
    s.height  = (height-2)  + "px";
    s.display = "block";

    old.parentNode.replaceChild(wrapper, old);
};

HN.Layout.Selection.prototype.show_bounds = function(wrapper, bounds)
{
    var sheet  = this.layout.s;
    var coords = sheet.cell_offset(bounds.y1, bounds.x1);

    for( var x = bounds.x1, width = 0; x < bounds.x2+1; x++ ) {
        width += sheet.col_width(x);
    }
    for( var y = bounds.y1, height = 0; y < bounds.y2+1; y++ ) {
        height += sheet.row_height(y);
    }

    wrapper.style.left    = (coords.left-1) + "px";
    wrapper.style.top     = (coords.top-1)  + "px";
    wrapper.style.width   = (width-2)   + "px";
    wrapper.style.height  = (height-2)  + "px";
    wrapper.style.display = "block";
};

HN.Layout.Selection.prototype.build_clipboard_str = function()
{
    var str = "";
    var tocopy = HN.Util.id("copyformula").checked ? "formula" : "value";
    this.bounds = this.order_bounds(this.bounds);
    for( var y = this.bounds.y1; y <= this.bounds.y2; y++ ) {
        for( var x = this.bounds.x1; x <= this.bounds.x2; x++ ) {
            var text = this.layout.s.cell(y, x)[tocopy] || "";
            str += text.replace(/\n|\t/g, "");
            if( x < this.bounds.x2 ) {
                str += "\t";
            }
        }
        if(y < this.bounds.y2) {
            str += "\n";
        }
    }
    return str;
};

HN.Layout.Selection.prototype.is_editing = function()
{
    return this.is_full_edit() || this.is_light_edit();
};

HN.Layout.Selection.prototype.is_full_edit = function()
{
    return this.state == HN.States.EDIT_FULL_CELL;
};

HN.Layout.Selection.prototype.is_light_edit = function()
{
    return this.state == HN.States.EDIT_LIGHT_CELL
        || this.state == HN.States.EDIT_LIGHT_RANGE;
};

HN.Layout.Selection.prototype.is_cell = function()
{
    return this.state == HN.States.EDIT_FULL_CELL
        || this.state == HN.States.EDIT_LIGHT_CELL
        || this.state == HN.States.SELECTED_CELL;
};

HN.Layout.Selection.prototype.is_selected = function()
{
    return this.state == HN.States.SELECTED_RANGE
        || this.state == HN.States.SELECTED_CELL;
};

HN.Layout.Selection.prototype.menuItemPicked = function(e)
{
    if( typeof e == "undefined" ) {
        return true;
    };

    if( e.parentNode && e.parentNode.className.match("disabled") ) {
        return false;
    }

    var action = e.getAttribute("name");
    if( action !== null && typeof this["menuAction_"+action] == "function") {
        return this["menuAction_"+action](e);
    }
};

HN.Layout.Selection.prototype.menuAction_deleterows = function(e)
{
    HN.Callbacks.deleteRowCol(this.layout.s.data.data.path, this.bounds, Y);
    return true;
};

HN.Layout.Selection.prototype.menuAction_insertrowsafter = function(e)
{
    HN.Callbacks.insertRowCol(this.layout.s.data.data.path, this.bounds, Y, "after");
    return true;
};

HN.Layout.Selection.prototype.menuAction_insertrowsbefore = function(e)
{
    HN.Callbacks.insertRowCol(this.layout.s.data.data.path, 
                              this.bounds, Y, "before");
    return true;
};

HN.Layout.Selection.prototype.menuAction_deletecols = function(e)
{
    HN.Callbacks.deleteRowCol(this.layout.s.data.data.path, this.bounds, X);
    return true;
};

HN.Layout.Selection.prototype.menuAction_insertcolsafter = function(e)
{
    HN.Callbacks.insertRowCol(this.layout.s.data.data.path, 
                              this.bounds, X, "after");
    return true;
};

HN.Layout.Selection.prototype.menuAction_insertcolsbefore = function(e)
{
    HN.Callbacks.insertRowCol(this.layout.s.data.data.path, 
                              this.bounds, X, "before");
    return true;
};

HN.Layout.Selection.prototype.menuAction_import = function(e)
{
    HN.UIActions.open_dialog(this.layout, "import");
    return true;
};

HN.Layout.Selection.prototype.menuAction_delete = function(e)
{
    HN.Callbacks.deletePage();
    return true;
};

HN.Layout.Selection.prototype.menuAction_functions = function(e)
{
    HN.Util.id("functions").style.display = "block";
    return true;
};

HN.Layout.Selection.prototype.menuAction_feedback = function(e)
{
    HN.UIActions.open_dialog(this.layout, "mark");
    return true;
};

HN.Layout.Selection.prototype.menuAction_paste = function(e)
{
    this.paste(JSON.parse(unescape(localStorage.copied)));
    return true;
};

HN.Layout.Selection.prototype.menuAction_pastevalue = function(e)
{
    var clipboard = JSON.parse(unescape(localStorage.copied));
    this.pasteText(clipboard.values);
    return true;
};

HN.Layout.Selection.prototype.menuAction_pastelink = function(e)
{
    var clipboard = JSON.parse(unescape(localStorage.copied));
    var b         = clipboard.bounds;
    var arr       = [];

    for( var y = 0; y <= b.y2 - b.y1; y++ ) {
        arr[y] = [];
        for( var x = 0; x <= b.x2 - b.x1; x++) {
            arr[y][x] = "="+clipboard.path
                + HN.Util.coord_to_ref({"x":x+b.x1, "y":y+b.y1});
        }
    }
    HN.Callbacks.pasteValues(this.layout.s.data.data.path, this.bounds, arr);

    this.editPasteBounds(this.bounds, clipboard.bounds);
    return true;
};

HN.Layout.Selection.prototype.menuAction_copy = function(e)
{
    this.copy();
    return true;
};

HN.Layout.Selection.prototype.menuAction_copyFormula = function(e)
{
    if( $("#copyformula").is(":checked") ) {
        $("#copyformula").removeAttr("checked");
    } else { 
        $("#copyformula").attr("checked", "checked");
    }
    return false;
};

HN.Layout.Selection.prototype.menuAction_clearboth = function(e)
{
    HN.Callbacks.clear(this.layout.s.data.data.path, this.bounds, "all");
    return true;
};

HN.Layout.Selection.prototype.menuAction_clearformat = function(e)
{
    HN.Callbacks.clear(this.layout.s.data.data.path, this.bounds, "style");
    return true;
};

HN.Layout.Selection.prototype.menuAction_clearcontent = function(e)
{
    HN.Callbacks.clear(this.layout.s.data.data.path, this.bounds, "contents");
    return true;
};

HN.Layout.Selection.prototype.menuAction_newview = function(e)
{    
    HN.Callbacks.setMark({"value": "New View Created"});
    window.location.hash = hn.hashUrl.setParam("view", "create");
    return true;
};

HN.Layout.Selection.prototype.menuAction_view = function(e)
{
    var view = $(e).attr("data-path");
    HN.Callbacks.setMark({"value": "View: "+view+" Opened"});
    window.location.hash = hn.hashUrl.setParam("view", view);
    return true;
};