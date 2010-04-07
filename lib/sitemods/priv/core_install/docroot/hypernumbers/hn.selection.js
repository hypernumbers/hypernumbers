/**
 * @class HN.Layout
 * Rename this to tabUI
 * Handles the main layout controls, the grid / axis / formula bar and toolbar
 */

HN.Layout.Selection = function(layout, sheet)
{
    var public = {};
    
    public.state      = HN.States.SELECTED_CELL;
    public.formula    = HN.Util.id("formula");
    public.wrapper    = HN.Util.id("selection");
    public.start_drag = false;
    public.tmpbounds  = {};

    public.dom = {
        "rowselection" : HN.Util.id("rowselection"),
        "colselection" : HN.Util.id("colselection"),
        "name"         : HN.Util.id("name"),
        "current"      : HN.Util.id("current")
    };

    public.has_scrolled   = false;
    public.last_click     = null;
    public.cell_dom       = null;
    public.cell           = null;
    public.in_formula_bar = false;
    public.copied_str     = "";
    public.copied         = {"x1":1, "y1": 1, "x2": 1, "y2": 1};
    public.bounds         = {"x1":1, "y1": 1, "x2": 1, "y2": 1};
    public.dragpos        = {"x1":0, "y1": 0, "x2": 0, "y2": 0};

    public.panes = new HN.Layout.Panes(layout, public, sheet)
    public.c     = new HN.Layout.Cell(public, sheet);

    var dragged    = HN.Util.id("dragged");

    public.currentCellValue = function() {
        return (public.is_editing() && public.c.value()) || 
            sheet.cell(public.cell.y, public.cell.x).value || "";
    };

    public.currentCellFormula = function() {
        return (public.is_editing() && public.c.value()) || 
            sheet.cell(public.cell.y, public.cell.x).formula || "";
    };

    public.currentSelectedCell = function() {
        return public.cell;
    };

    public.currentSelectedBounds = function() {
        return public.bounds;
    };

    public.selectFullSheet = function() {
        public.bounds = { 
            "x1" : 1, 
            "y1" : 1,
            "x2" : sheet.max.x,
            "y2" : sheet.max.y
        };
        public.show_selection();
    };

    public.clearMenu = function(e, type)
    {
        HN.Callbacks.clear(public.bounds, type);
    };

    public.gridContextMenu = function(e)
    {
        var tmp = sheet.cell(public.cell.y, public.cell.x);
        var el  = HN.Util.id("gridcontextmenu");

        var unMerge = public.bounds.x1 == public.bounds.x2 && 
            public.bounds.y1 == public.bounds.y2 &&
            tmp.merge;
        $("#mergetext").text( (unMerge) ? "Unmerge" : "Merge");
        
        el.style.display = "block";
        el.style.top     = e.clientY+"px";
        el.style.left    = e.clientX+"px";

        var hide = function(e) {

            if( HN.Util.is_inside(e.target, el) ) {
                
                var target = ( e.target.nodeName != "A" )
                    ? $(e.target).parents("a")[0] : e.target;
                
                if( !target || !public.menuItemPicked(target) ) {
                    return;
                }
            }
            
            HN.Util.removeEvent(document, "mousedown", hide);
            el.style.display = "none";
        };

        HN.Util.addEvent(document, "mousedown", hide);

        e.preventDefault();
        return false;
    };

    public.axisContextMenu = function(e, axis)
    {
        var el = (axis == Y)
            ? HN.Util.id("axiscontextmenurow")
            : HN.Util.id("axiscontextmenucol");
        el.style.display = "block";
        el.style.top     = e.clientY+"px";
        el.style.left    = e.clientX+"px";
        
        var hide = function(e) {

            if( HN.Util.is_inside(e.target, el) ) {
                
                var target = ( e.target.nodeName != "A" )
                    ? $(e.target).parents("a")[0] : e.target;
                
                if( !target || !public.menuItemPicked(target) ) {
                    return;
                }
            }
            
            HN.Util.removeEvent(document, "mousedown", hide);
            el.style.display = "none";
        };

        HN.Util.addEvent(document, "mousedown", hide);

        e.preventDefault();
        return false;
    };

    public.mouseDownAxis = function(e, xy)
    {
        e.preventDefault();

        if( e.target.className == "handle" && e.button != 2 ) {

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
                    HN.Callbacks.setHeight(sheet.path(), 
                                           index, new_val+4);
                } else {
                    HN.Callbacks.setWidth(sheet.path(),
                                          index, new_val+4);
                }
            };

            var mousemove = function( e ) {
                new_val = orig + (e[attr] - start);
                if( new_val > 0 ) {
                    public.shiftAxis(cell, xy, index, (e[attr] - start));
                    cell.style[xy.dimension] =  new_val + "px";
                }
            };

            HN.Util.addEvent(document, "mouseup", mouseup);
            HN.Util.addEvent(document, "mousemove", mousemove);

        } else {

            var attr     = (xy == Y) ? "clientY" : "clientX";
            var offset   = (xy == Y) 
                ? HN.Util.y_pos($("#scroller")[0])
                : HN.Util.x_pos($("#scroller")[0]);

            var max      = (xy == Y) ? sheet.max.x : sheet.max.y;
            var b        = public.order_bounds(public.bounds);
            var start    = public.getNewOffset(e[attr] - offset, xy);

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
                public.bounds = {
                    "y1" : start.index, "x1" : 1, 
                    "y2" : start.index, "x2" : max
                };
                public.set_cell(start.index, 1);
            } else {
                public.bounds = {
                    "x1" : start.index, "y1" : 1, 
                    "x2" : start.index, "y2" : max
                };
                public.set_cell(1, start.index);
            }

            public.show_selection();

            var mouseup = function( e ) {
                HN.Util.id("overlay").style.display = "none";
                document.body.style.cursor = "";

                HN.Util.removeEvent(document, "mouseup", mouseup);
                HN.Util.removeEvent(document, "mousemove", mousemove);
            };

            var mousemove = function( e ) {
                var end = public.getNewOffset(e[attr] - offset, xy);

                if( xy == Y ) {
                    public.bounds = { 
                        "y1":Math.min(start.index, end.index), "x1":1,
                        "y2":Math.max(start.index, end.index), "x2":max 
                    };                    
                } else {
                    public.bounds = { 
                        "x1":Math.min(start.index, end.index), "y1":1,
                        "x2":Math.max(start.index, end.index), "y2":max
                    };
                }
                public.show_selection();
                e.preventDefault();
                return false;
            };

            HN.Util.addEvent(document, "mouseup", mouseup);
            HN.Util.addEvent(document, "mousemove", mousemove);
        }
    };

    public.shiftAxis = function(element, axis, index, diff)
    {
        while( element.nextSibling && ++index) {
            element = element.nextSibling;
            var tmp = (axis == Y)
                ? sheet.cell_offset(index, 1)
                : sheet.cell_offset(1, index);
            element.style[axis.coord] = (tmp[axis.coord] + diff) + "px";
        }
    };

    public.is_viewable = function(y, x) {
       
        var ret   = {"x":true, "y":true};
        var coord = sheet.cell_offset(y, x);
        var top   = Math.abs(layout.gridPos.top());
        var left  = Math.abs(layout.gridPos.left());

        var is_above = coord.top < top;
        var is_below = (coord.top+sheet.row_height(y)) 
            > top + layout.gridSizePix().y;

        var is_left  = coord.left < left;
        var is_right = (coord.left+sheet.col_width(x)) 
            > left + layout.gridSizePix().x;

      
        if( !is_above != !is_below ) {
            ret.y = false;
        }
        
        if( !is_left != !is_right ) {
            ret.x = false;
        }

        return ret;
    };

    public.scrollIntoView = function(y, x, dir) {

        var tmp = public.is_viewable(y, x);
        
        if( tmp.x && tmp.y ) {
            public.has_scrolled = false;
            return;
        }
        
        if( !tmp.x && (dir === true) ) {
            sheet.offset.x = x;
            public.panes.moveTo(sheet.offset.y, x);
        } else if ( !tmp.x ) {
            layout.scrollBy1(dir == HN.RIGHT, X);
            public.scrollIntoView(y, x, dir);
        }
        
        else if( !tmp.y && (dir === true) ) {
            sheet.offset.y = y;
            public.panes.moveTo(y, sheet.offset.x);
        } else if( !tmp.y ) {
            layout.scrollBy1(dir == HN.DOWN, Y);
            public.scrollIntoView(y, x, dir);
        }
        public.has_scrolled = false;
    };

    public.set_cell = function(y, x, dir) {

        var current = public.dom.current.style;

        public.cell = {"y":y, "x":x};

        if( dir ) {
            var arg = (!public.has_scrolled) ? dir : public.has_scrolled;
            public.scrollIntoView(y, x, arg);
        }

        public.cell_dom = public.panes.get_cell(y, x);

        var cell_attr = sheet.cell(y, x);
        var cell_pos  = sheet.cell_offset(y, x);
        var formula   = (cell_attr.formula) || "";

        if( public.formula ) {
            public.formula.value = formula;
        }

        var width  = cell_attr.mergeWidth  || sheet.col_width(x); 
        var height = cell_attr.mergeHeight || sheet.row_height(y); 
        
        current.top    = (cell_pos.top-1)  + "px";
        current.left   = (cell_pos.left-1) + "px";
        current.width  = width + "px";
        current.height = height + "px";
    };

    public.canEdit = function()
    {
        return true;
    };

    public.keydown = function(e)
    {
        if( !layout.gridHasFocus() ) {
            return;
        }

        if( e.keyCode == HN.Keys.BACKSPACE && public.is_selected()
            && !public.in_formula_bar ) {
            e.preventDefault();
        }

        public.do_keydown(e);

        if(!(e.keyCode == HN.Keys.DOWN || e.keyCode == HN.Keys.UP || 
             e.keyCode == HN.Keys.TAB  || e.keyCode == HN.Keys.RETURN || 
             e.keyCode == HN.Keys.LEFT || e.keyCode == HN.Keys.RIGHT) ) {
            return;
        }

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
                public.do_keydown(e);
            }, 30);
        };

        HN.Util.addEvent(document, "keyup", cleanup);
        HN.Util.addEvent(window, "blur", cleanup);

        timeout = window.setTimeout(set_interval, 300);
    };

    public.do_keydown = function(e)
    {
        if( public.in_formula_bar ) {

            if(e.keyCode == HN.Keys.ENTER || e.keyCode == HN.Keys.TAB) {
                public.select(e.keyCode, e);
            } else {
                if( public.is_selected() ) {
                    public.startEditing();
                    public.formula.focus();
                }
                public.c.read_formula_bar();
            }
        }
        else if( (e.keyCode == HN.Keys.ENTER || e.keyCode == HN.Keys.TAB)
                 && (public.is_editing() || public.is_selected()) )
        {
            if( public.is_cell() ) {
                public.select(e.keyCode, e);
            } else {
                public.select_next(e.keyCode, e);
            }
        }
        else if( (public.is_light_edit() || public.is_selected())
                 && (e.keyCode == HN.Keys.UP || e.keyCode == HN.Keys.DOWN
                     || e.keyCode == HN.Keys.LEFT || e.keyCode == HN.Keys.RIGHT) )
        {
            e.preventDefault();
            public.select(e.keyCode, e);
        }
        else if(public.canEdit() && public.is_selected()
                && (e.keyCode == HN.Keys.DELETE || e.keyCode == HN.Keys.BACKSPACE))
        {
            public.bounds = public.order_bounds(public.bounds);
            HN.Callbacks.clear(sheet.path(), 
                               public.bounds, "contents");
        }
        else if( public.is_selected() ){

            if( (e.ctrlKey || e.metaKey) && e.keyCode == 86  && public.canEdit()) {

                var inp = HN.Util.id("paste");
                inp.value = "";
                inp.focus();

                setTimeout( function() {
                    public.pasteText(inp.value);
                    inp.blur();
                },0);

                // Copy
            } else if( (e.ctrlKey || e.metaKey) && e.keyCode == 67 ) {
                var str = public.copy();
                var inp = HN.Util.id("paste");
                inp.value = str;
                inp.select();
                inp.focus();
                setTimeout( function() {
                    inp.blur();
                },0);
            }

            else if ( HN.Util.is_char(e.keyCode) && 
                      public.canEdit() && 
                      !(e.ctrlKey || e.metaKey)) {
                public.scrollIntoView(public.cell.y, public.cell.x, true);
                public.startEditing("");
            }
        }
        else if( public.is_editing() ) {
            setTimeout(function() {
                public.formula.value = public.c.value();                
                public.c.calculateWidth();
            },0);
        }
    };

    public.select = function(key, event)
    {
        if( event.shiftKey || key == HN.Keys.TAB ) {
            event.preventDefault();
        }

        var b    = public.tmpbounds;
        var cell = HN.Util.clone(public.cell);
        var dir  = null;

        var select_cell = function() {
            public.tmpbounds = HN.Util.clone(b);
            public.bounds    = HN.Util.clone(b);
            public.set_cell(b.y1, b.x1, dir);
            public.state = HN.States.SELECTED_CELL;
            public.show_selection();
        };

        var select_range = function() {
            public.tmpbounds    = HN.Util.clone(b);
            public.bounds       = HN.Util.clone(b);
            public.has_scrolled = true;
            public.state        = HN.States.SELECTED_RANGE;
            public.show_selection();
        };

        if( public.is_editing() ) {
            if( public.c.is_formula() &&
                (key != HN.Keys.ENTER && key != HN.Keys.TAB) ) {
                
                select_cell = function() {

                    var arg = (!public.has_scrolled) ? dir : public.has_scrolled;
                    public.scrollIntoView(b.y1, b.x1, arg);

                    var tmp = public.tmpbounds = b;
                    public.c.insert_range(tmp);
                    public.highlightRange(HN.Util.id("dragfill"), tmp);
                };

                select_range = function() {
                    var tmp = public.tmpbounds = b;
                    public.c.insert_range(tmp);
                    public.highlightRange(HN.Util.id("dragfill"), tmp);
                };

                if( !public.start_drag ) {
                    public.start_drag = true;
                    b = { "x1":b.x1, "y1":b.y1, "x2":b.x1, "y2":b.y2 };
                }

                cell = { "x":b.x1, "y":b.y1 };

            } else {
                public.c.end_edit();
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
                public.scrollIntoView(b.y1, b.x1, HN.DOWN);
            } else if( key == HN.Keys.DOWN ) {
                b.y2++;
                public.scrollIntoView(b.y2, b.x2, HN.DOWN);
            } else if( key == HN.Keys.UP && cell.y == b.y1 && b.y2 > 1 ) {
                b.y2--;
                public.scrollIntoView(b.y2, b.x2, HN.UP);
            } else if( key == HN.Keys.RIGHT && cell.x == b.x2 && b.x1 != b.x2 ) {
                b.x1++;
                public.scrollIntoView(b.y1, b.x1, HN.RIGHT);
            } else if( key == HN.Keys.RIGHT ) {
                b.x2++;
                public.scrollIntoView(b.y2, b.x2, HN.RIGHT);
            } else if( key == HN.Keys.LEFT && public.cell.x == b.x1 && b.x2 > 1 ) {
                b.x2--;
                public.scrollIntoView(b.y2, b.x2, HN.LEFT);
            }

            select_range();
        }
    };

    public.select_next = function(key, event)
    {
        public.c.end_edit();

        var cell = public.cell;
        var b    = public.order_bounds(public.bounds);

        if( key == HN.Keys.ENTER ) {
            if( cell.y < b.y2 ) {
                public.set_cell(cell.y+1, cell.x, HN.DOWN);
            } else if( cell.y == b.y2 && cell.x == b.x2 ) {
                public.has_scrolled = true;
                public.set_cell(b.y1, b.x1);
            } else {
                public.has_scrolled = true;
                public.set_cell(b.y1, cell.x+1, HN.RIGHT);
            }
        }
        else if( key == HN.Keys.TAB )
        {
            event.preventDefault();

            if( cell.x < b.x2 ) {
                public.set_cell(cell.y, cell.x+1, HN.RIGHT);
            } else if( cell.y == b.y2 && cell.x == b.x2 ) {
                public.has_scrolled = true;
                public.set_cell(b.y1, b.x1);
            } else {
                public.has_scrolled = true;
                public.set_cell(cell.y+1, b.x1, HN.DOWN);
            }
        }
        public.state = HN.States.SELECTED_RANGE;
    };

    public.editPasteBounds = function(b, r2)
    {
        if(b.x1 == b.x2 && b.y1 == b.y2) {
            b.x2 = b.x1 + (r2.x2 - r2.x1);
            b.y2 = b.y1 + (r2.y2 - r2.y1);
            public.show_selection();
        } else if(
            ((b.x2 - b.x1 + 1) % (r2.x2 - r2.x1 + 1)) != 0 ||
                ((b.y2 - b.y1 + 1) % (r2.y2 - r2.y1 + 1)) != 0) {
            b.x2 = b.x1 + (r2.x2 - r2.x1);
            b.y2 = b.y1 + (r2.y2 - r2.y1);
            public.show_selection();
        }
    };

    public.paste = function(copied)
    {
        public.state = HN.States.SELECTED_RANGE;
        HN.Callbacks.pasteRange(sheet.path(), public.bounds, 
                                copied.host+copied.path, 
                                copied.bounds);
        public.editPasteBounds(public.bounds, copied.bounds);
    };

    public.copy = function()
    {
        $(".paste").removeClass("disabled");

        var str = public.build_clipboard_str();

        localStorage.copied = escape(JSON.stringify({
            "bounds" : public.bounds,
            "host"   : sheet.key("host"),
            "path"   : hn.currentPath(),
            "values" : str
        }));

        public.copied = HN.Util.clone(public.bounds);
        public.highlightRange(HN.Util.id("copied"), public.copied);

        return str;
    };

    public.pasteText = function(val)
    {
        var copied = JSON.parse(unescape(localStorage.copied));

        // if the text pasted (ctrl+v) is the same as what was copied
        // use "proper" paste
        if( copied && val == copied.values ) {
            public.paste(copied);
            return;
        }

        if(val.match("\t") === null && val.match("\n") === null) {
            public.startEditing(val);
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

            public.state = HN.States.SELECTED_RANGE;
            HN.Callbacks.pasteValues(sheet.path(), 
                                     public.bounds,lines);
            var tmp = {"x1":1, "y1":1, "x2":lines[0].length, "y2":lines.length};
            public.editPasteBounds(public.bounds, tmp);
        }
    };

    public.getNewOffset = function(position, xy) {

        var i     = 1;
        var total = 0;
        var f     = ( xy == Y ) ? "row_height" : "col_width";
        
        while( true ) {

            total += sheet[f](i);
            
            if( total > position ) {
                return {
                    "index"    : i, 
                    "position" : total-sheet[f](i) 
                };
            }

            ++i;
        };
    };

    public.startEditing = function(val)
    {
        public.c.start_edit(val);

        HN.Util.id("copied").style.display = "none"; 

        public.state = (public.state == HN.States.SELECTED_RANGE)
            ? HN.States.EDIT_LIGHT_RANGE
            : HN.States.EDIT_LIGHT_CELL;
    };

    public.is_in_grid = function(auto, e)
    {
        var axis = auto.args.axis;
        var dir  = auto.args.forward;
        var x    = e.clientX;
        var y    = e.clientY;
        var gridSize = layout.gridSizePix();
        var xmargin = layout.gridMargin.x();
        var ymargin = layout.gridMargin.y();

        return (axis == Y && dir  && !(y > ymargin + gridSize.y)) || 
            (axis == X && dir  && !(x > xmargin + gridSize.x)) || 
            (axis == Y && !dir && !(y < ymargin)) || 
            (axis == X && !dir && !(x < xmargin));
    };

    public.auto_scroll_coords = function(e)
    {
        var gridSize = layout.gridSizePix();

        if( e.clientY > layout.gridMargin.y() + gridSize.y) { 
            return {"forward":true, "axis":Y};
        } else if( e.clientY < layout.gridMargin.y() ) { 
            return {"forward":false, "axis":Y};
        } else if( e.clientX > layout.gridMargin.x() + gridSize.x) {
            return {"forward":true, "axis":X};
        } else if( e.clientX < layout.gridMargin.x() ) {
            return {"forward":false, "axis":X};
        }
        return false;
    };

    public.dragdown = function(e)
    {
        e.preventDefault();

        var style = layout.inner.style;
        var auto  = {"timer":null, "axis":null};
        var offy   = layout.gridMargin.y() + layout.gridPos.top();
        var offx   = layout.gridMargin.x() + layout.gridPos.left();

        public.state        = HN.States.DRAGGING;
        public.has_scrolled = true;
        public.dragpos      = null;

        var auto_scroll = function() {
            layout.scrollBy1(auto.args.forward, auto.args.axis);
            offy   = layout.gridMargin.y() + layout.gridPos.top();
            offx   = layout.gridMargin.x() + layout.gridPos.left();
            var y = public.getNewOffset(auto.mouse.y - offy, Y);
            var x = public.getNewOffset(auto.mouse.x - offx, X);
            public.show_drag_selection({"x":x.index, "y":y.index});
        };

        var stop_auto_scroll = function() {
            window.clearInterval(auto.timer);
            auto = {"timer":null, "axis":null, "forward":null, "mouse":null};
        };

        var mousemove = function( e ) {
            if( !auto.timer ) {
                var args = public.auto_scroll_coords(e);
                if( args ) {
                    auto = { "args":  args,
                             "mouse": {"y":e.clientY, "x":e.clientX},
                             "timer": setInterval(auto_scroll, 30) };
                } else {
                    var y = public.getNewOffset(e.clientY - offy, Y);
                    var x = public.getNewOffset(e.clientX - offx, X);
                    public.show_drag_selection({x:x.index, y:y.index});
                }
            }
            else if( public.is_in_grid(auto, e) ) {
                stop_auto_scroll();
            }
        };

        var mouseup = function(e) {

            if( auto.timer ) {
                window.clearInterval(auto.timer);
            }

            HN.Util.removeEvent(document, "mouseup", mouseup);
            HN.Util.removeEvent(document, "mousemove", mousemove);
            
            if( !public.dragpos ) { 
                return;
            }

            HN.Callbacks.drag(sheet.path(), 
                              public.order_bounds(public.bounds), 
                              public.dragpos);

            public.bounds = {
                "y1": Math.min(public.bounds.y1, public.dragpos.y1),
                "x1": Math.min(public.bounds.x1, public.dragpos.x1),
                "y2": Math.max(public.bounds.y2, public.dragpos.y2),
                "x2": Math.max(public.bounds.x2, public.dragpos.x2)
            };

            public.resumeSelection();

            public.show_selection();
            dragged.style.display = "none";
        };

        HN.Util.addEvent(document, "mouseup", mouseup);
        HN.Util.addEvent(document, "mousemove", mousemove);
    };

    public.resetPanePos = function() {
        
    };

    public.resumeSelection = function()
    {
        public.state = (public.bounds.y1 == public.bounds.y2
                        && public.bounds.x1 == public.bounds.x2)
            ? HN.States.SELECTED_CELL
            : HN.States.SELECTED_RANGE;
    };

    public.resume_editing = function()
    {
        public.state = (public.bounds.y1 == public.bounds.y2
                      && public.bounds.x1 == public.bounds.x2)
            ? HN.States.EDIT_LIGHT_CELL
            : HN.States.EDIT_LIGHT_RANGE;
    };


    public.dblclick = function(e)
    {
        if( !layout.gridHasFocus() || !public.canEdit() ) {
            return;
        }

        if( public.state != HN.States.EDIT_FULL_CELL ) {
            var val = sheet.cell(public.cell.y, public.cell.x).formula || "";
            public.c.start_edit(val);
            public.state = HN.States.EDIT_FULL_CELL;
        }
    };

    public.mousedown = function(e) {
        
        if( $(e.target).parents("#grid").length == 0 && 
            $(e.target).attr("id") != "formula") {
            
            if( layout.gridHasFocus() ) {
                layout.grabFocus();
            }

            return;
        } else if ( !layout.gridHasFocus() ) {
            // In case url has focus
            $("#paste")[0].focus();            
            layout.resumeSelection();
        }

        // Dont interfere with right clicks on selections
        if( e.button == 2 && e.target == public.wrapper || 
            e.button == 2 && e.target == public.dom.current || 
            e.target.nodeName == "TEXTAREA") {
            return true;
        }

        // Let use click on links in cells
        if( e.target.nodeName == "A" && (e.ctrlKey || e.metaKey) ) {
            document.location.href = e.target.getAttribute("href");
        }

        e.preventDefault();

        var style  = layout.inner.style;
        var auto   = {};
        var bounds = {};
        var offy   = layout.gridMargin.y() + layout.gridPos.top();
        var offx   = layout.gridMargin.x() + layout.gridPos.left();

        public.has_scrolled = true;

        // Stop double clicks being screwed with
        if( public.last_click && new Date().getTime() - 250 < 
            public.last_click ) {
            return true;
        } else {
            public.last_click = new Date().getTime();
        }


        if( public.in_formula_bar ) {
            public.in_formula_bar = false;
            if( e.target == public.c.input ) {
                return true;
            }
        }

        var select = function() {
            public.bounds = bounds;
            public.show_selection();
        };

        var select_start = function() {
            
            var cell = sheet.cell(bounds.y1, bounds.x1);
            if( cell.mergeTo ) { 
                bounds = {
                    "x1":cell.mergeTo.x,
                    "x2":cell.mergeTo.x,
                    "y1":cell.mergeTo.y,
                    "y2":cell.mergeTo.y,
                };
            }

            public.bounds = bounds;
            public.set_cell(bounds.y1, bounds.x1);
            public.tmpbounds = bounds;
            public.show_selection();
        };

        var select_end = function() {
            public.resumeSelection();
        };

        if( public.is_editing() ) {
            if( public.c.is_formula() ) {
                select = function() {
                    var tmp = public.order_bounds(bounds);
                    public.c.insert_range(tmp);
                    public.highlightRange(HN.Util.id("dragfill"), tmp);
                };
                select_start = function() {
                    var tmp = public.order_bounds(bounds);
                    public.tmpbounds = bounds;
                    public.c.insert_range(tmp);
                    public.highlightRange(HN.Util.id("dragfill"), tmp);
                };
                select_end = function() { };

            } else {
                public.c.end_edit();
            }
        }

        if( e.shiftKey ) {
            var y = public.getNewOffset(e.clientY - offy, Y);
            var x = public.getNewOffset(e.clientX - offx, X);
            bounds = { "x1":public.tmpbounds.x1, "x2":x.index,
                       "y1":public.tmpbounds.y1, "y2":y.index };
            select();
            setTimeout( function() { select_end(); }, 1000);

            return true;
        }

        var auto_scroll = function() {

            layout.scrollBy1(auto.args.forward, auto.args.axis);
            offy   = layout.gridMargin.y() + layout.gridPos.top();
            offx   = layout.gridMargin.x() + layout.gridPos.left();
            
            var y = public.getNewOffset(auto.mouse.y - offy, Y);
            var x = public.getNewOffset(auto.mouse.x - offx, X);

            bounds.y2 = y.index;
            bounds.x2 = x.index;
            select();
        };

        var stop_auto_scroll = function() {
            window.clearInterval(auto.timer);
            auto = {"timer":null, "axis":null, "forward":null, "mouse":null};
        };

        var y = public.getNewOffset(e.clientY - offy, Y);
        var x = public.getNewOffset(e.clientX - offx, X);

        bounds = {"y1":y.index, "x1":x.index, "y2":y.index, "x2":x.index};

        select_start();

        var mousemove = function( e ) {
            e.preventDefault();
            if( !auto.timer ) {
                var args = public.auto_scroll_coords(e);
                if( args ) {
                    auto = { 
                        "args":  args,
                        "mouse": {"y":e.clientY, "x":e.clientX},
                        "timer": setInterval(auto_scroll, 30) 
                    };
                } else {

                    var y2 = public.getNewOffset(e.clientY - offy, Y);
                    var x2 = public.getNewOffset(e.clientX - offx, X);

                    bounds.y2 = y2.index;
                    bounds.x2 = x2.index;

                    select();
                }
            }
            else if( public.is_in_grid(auto, e) ) {
                stop_auto_scroll();
            }
        };

        var mouseup = function( e ) {

            if( auto.timer ) {
                window.clearInterval(auto.timer);
            }

            document.body.style.cursor = "";
            HN.Util.id("overlay").style.display = "none";
            public.mouse = {"x1":0, "y1": 0, "x2": 0, "y2": 0};
            HN.Util.removeEvent(document, "mouseup", mouseup);
            HN.Util.removeEvent(document, "mousemove", mousemove);
            select_end();
        };

        document.body.style.cursor = "cell";
        HN.Util.id("overlay").style.display = "block";
        HN.Util.addEvent(document, "mouseup", mouseup);
        HN.Util.addEvent(document, "mousemove", mousemove);
    };

    public.show_drag_selection = function(drag)
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

        var npos = pos(drag, public.order_bounds(public.bounds));

        if( npos ) {
            public.dragpos = { 
                "y1": npos[0], "x1": npos[1],
                "y2": npos[2], "x2": npos[3]
            };
            public.highlightRange(dragged, public.dragpos);
        }
    };

    public.order_bounds = function(b)
    {
        return {
            "x1": Math.min(b.x1, b.x2),
            "x2": Math.max(b.x1, b.x2),
            "y1": Math.min(b.y1, b.y2),
            "y2": Math.max(b.y1, b.y2)
        };
    };

    public.show_cell = function() {

        var current   = public.dom.current.style;
        var cell_attr = sheet.cell(public.cell.y, public.cell.x);
        var cell_pos  = sheet.cell_offset(public.cell.y, public.cell.x);
        var formula   = (cell_attr.formula) || "";

        if( public.formula ) {
            public.formula.value = formula;
        }

        current.top    = (cell_pos.top-1)  + "px";
        current.left   = (cell_pos.left-1) + "px";
        current.height = (sheet.row_height(public.cell.y)-1) + "px";
        current.width  = (sheet.col_width(public.cell.x)-1) + "px";
    };

    public.show_selection = function()
    {
        var bounds = public.order_bounds(public.bounds);
        var coords = public.highlightRange(public.wrapper, bounds);

        var col    = public.dom.colselection;
        var row    = public.dom.rowselection;

        public.dom.name.innerHTML = HN.Util.range_to_str(bounds);
        
        row.style.top    = (coords.top) + "px";
        row.style.height = (coords.height) + "px";
        
        col.style.left   = (coords.left) + "px";
        col.style.width  = (coords.width) + "px";
    };

    public.highlightRange = function(highlight, range)
    {
        var style  = highlight.style;
        var coords = sheet.cell_offset(range.y1, range.x1);
        var cell;
        var width = 0;

        for( var y = range.y1, height = 0; y < range.y2+1; y++ ) {

            var rowh = sheet.row_height(y); 
            
            for( var x=range.x1, tmpw=0; x < range.x2+1; x++ ) {
                
                cell = sheet.cell(y, x);
                
                if( cell.merge ) { 
                    
                    tmpw += cell.mergeWidth;
                    rowh = cell.mergeHeight;
                    
                    x += cell.merge.right;
                    y += cell.merge.down;
                    
                } else { 
                    tmpw += sheet.col_width(x);
                }
            }
            if( tmpw > width ) { 
                width = tmpw;
            }
            
            height += rowh;
        }

        style.left    = (coords.left-1) + "px";
        style.top     = (coords.top-1)  + "px";
        style.width   = (width-2)   + "px";
        style.height  = (height-2)  + "px";
        style.display = "block";

        return { 
            "width"  : width, 
            "height" : height, 
            "top"    : coords.top,   
            "left"   : coords.left 
        };
    };

    public.build_clipboard_str = function()
    {
        var str = "";
        // var tocopy = HN.Util.id("copyformula").checked ? "formula" : "value";
        var tocopy = "formula";
        public.bounds = public.order_bounds(public.bounds);
        for( var y = public.bounds.y1; y <= public.bounds.y2; y++ ) {
            for( var x = public.bounds.x1; x <= public.bounds.x2; x++ ) {
                var text = sheet.cell(y, x)[tocopy] || "";
                str += text.replace(/\n|\t/g, "");
                if( x < public.bounds.x2 ) {
                    str += "\t";
                }
            }
            if(y < public.bounds.y2) {
                str += "\n";
            }
        }
        if( str == "" ) { 
            str = " ";
        }
        return str;
    };

    public.is_editing = function()
    {
        return public.is_full_edit() || public.is_light_edit();
    };

    public.is_full_edit = function()
    {
        return public.state == HN.States.EDIT_FULL_CELL;
    };

    public.is_light_edit = function()
    {
        return public.state == HN.States.EDIT_LIGHT_CELL
            || public.state == HN.States.EDIT_LIGHT_RANGE;
    };

    public.is_cell = function()
    {
        return public.state == HN.States.EDIT_FULL_CELL
            || public.state == HN.States.EDIT_LIGHT_CELL
            || public.state == HN.States.SELECTED_CELL;
    };

    public.is_selected = function()
    {
        return public.state == HN.States.SELECTED_RANGE
            || public.state == HN.States.SELECTED_CELL;
    };

    public.menuItemPicked = function(e)
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

    public.menuAction_deleterows = function(e)
    {
        HN.Callbacks.deleteRowCol(sheet.path(), public.bounds, Y);
        return true;
    };

    public.menuAction_insertrowsafter = function(e)
    {
        HN.Callbacks.insertRowCol(sheet.path(), public.bounds, Y, "after");
        return true;
    };

    public.menuAction_insertrowsbefore = function(e)
    {
        HN.Callbacks.insertRowCol(sheet.path(), 
                                  public.bounds, Y, "before");
        return true;
    };

    public.menuAction_deletecols = function(e)
    {
        HN.Callbacks.deleteRowCol(sheet.path(), public.bounds, X);
        return true;
    };

    public.menuAction_insertcolsafter = function(e)
    {
        HN.Callbacks.insertRowCol(sheet.path(), 
                                  public.bounds, X, "after");
        return true;
    };

    public.menuAction_insertcolsbefore = function(e)
    {
        HN.Callbacks.insertRowCol(sheet.path(), 
                                  public.bounds, X, "before");
        return true;
    };

    public.menuAction_import = function(e)
    {
        HN.UI.open_dialog(layout, "import");
        return true;
    };

    public.menuAction_delete = function(e)
    {
        HN.Callbacks.deletePage();
        return true;
    };

    public.menuAction_mergecells = function(e)
    {
        HN.Callbacks.mergeCells(sheet.path(), public.bounds);
        return true;
    };

    public.menuAction_functions = function(e)
    {
        HN.Util.id("functions").style.display = "block";
        return true;
    };

    public.menuAction_feedback = function(e)
    {
        HN.UI.open_dialog(layout, "mark");
        return true;
    };

    public.menuAction_paste = function(e)
    {
        public.paste(JSON.parse(unescape(localStorage.copied)));
        return true;
    };

    public.menuAction_pastevalue = function(e)
    {
        var clipboard = JSON.parse(unescape(localStorage.copied));
        public.pasteText(clipboard.values);
        return true;
    };

    public.menuAction_pastelink = function(e)
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
        HN.Callbacks.pasteValues(sheet.path(), public.bounds, arr);

        public.editPasteBounds(public.bounds, clipboard.bounds);
        return true;
    };

    public.menuAction_copy = function(e)
    {
        public.copy();
        return true;
    };

    public.menuAction_copyFormula = function(e)
    {
        if( $("#copyformula").is(":checked") ) {
            $("#copyformula").removeAttr("checked");
        } else { 
            $("#copyformula").attr("checked", "checked");
        }
        return false;
    };

    public.menuAction_clearboth = function(e)
    {
        HN.Callbacks.clear(sheet.path(), public.bounds, "all");
        return true;
    };

    public.menuAction_clearformat = function(e)
    {
        HN.Callbacks.clear(sheet.path(), public.bounds, "style");
        return true;
    };

    public.menuAction_clearcontent = function(e)
    {
        HN.Callbacks.clear(sheet.path(), public.bounds, "contents");
        return true;
    };

    public.menuAction_newview = function(e)
    {    
        HN.Callbacks.setMark({"value": "New View Created"});
        window.location.hash = hn.hashUrl.setParam("view", "create");
        return true;
    };

    public.menuAction_view = function(e)
    {
        var view = $(e).attr("data-path");
        HN.Callbacks.setMark({"value": "View: "+view+" Opened"});
        window.location.hash = hn.hashUrl.setParam("view", view);
        return true;
    };

    public.set_cell(1, 1);
    public.show_selection();

    return public;
};

HN.Layout.Cell = function(selection, sheet) {

    var public = {};
    
    var layout = selection.layout;
    var input  = HN.Util.id("input");
    var hidden = HN.Util.id("hidden_input");

    public.value = function() {
        return input.value;
    }

    public.calculateWidth = function() {

        var index  = 0;
        var val    = null;
        var tmp    = 0;
        var fn     = null;

        hidden.innerHTML = input.value;
        
        if( input.style.whiteSpace == "normal" ) {
            var xy = Y;
            var cell = sheet.cell(selection.cell.y, selection.cell.x);
            var t  = ((cell && cell.merge && cell.mergeWidth 
                       || sheet.col_width(selection.cell.x))-2) +"px";
            val                = hidden.clientHeight + 5;
            fn                 = "row_height";
            hidden.style.width = t;
            input.style.width  = t;
        } else {
            var xy = X;
            val    = hidden.clientWidth+5;
            fn     = "col_width";
            input.style.height =
                (sheet.row_height(selection.cell.y)-2)+"px";
        }
        
        do {
            tmp += sheet[fn](selection.cell[xy.str] + index);
            input.style[xy.dimension] = (tmp-2) + "px";
            index++;
        } while( (val+10) > tmp );
    };

    public.start_edit = function(val) {

        var s         = input.style;
        var cell      = selection.cell;
        var cell_attr = sheet.cell(cell.y, cell.x);
        var cell_pos  = sheet.cell_offset(cell.y, cell.x);        
        var style     = "background:#FFF;"+sheet.lookupCSS(cell.y, cell.x)
            +"text-align:left; display:block;outline:none;resize: none;";
        
        selection.cell_dom.innerHTML = "";
        hidden.setAttribute("style", style);
                
        input.value = val;
        input.setAttribute("style", style);
        s.width  = "0px";
        s.height = (sheet.row_height(cell.y)-2) + "px";
        s.top    = (cell_pos.top-1)  + "px";
        s.left   = (cell_pos.left+0) + "px";
        
        public.calculateWidth();
        input.focus();
    };

    public.end_edit = function(val) {

        if( selection.in_formula_bar ) {
            selection.in_formula_bar = false;
        }
        if( selection.formula ) {
            selection.formula.blur();
        }
        
        input.blur();
        input.style.display = "none";
        
        HN.Util.id("dragfill").style.display = "none";
        selection.start_drag = false;
        
        var c = sheet.cell(selection.cell.y, selection.cell.x);
        
        if( selection.is_light_edit() ) {
            selection.cell_dom.innerHTML = input.value;
            if( c ) {
                c.value = input.value;
            }

            HN.Callbacks.set_cell(sheet.path(), 
                                  selection.cell, input.value);
        } else if( selection.is_full_edit() ) {
            var formula = (c && c.formula) || "";
            if( input.value != formula ) {
                selection.cell_dom.innerHTML = input.value;

                if( c ) {
                    c.value = input.value;
                }

                HN.Callbacks.set_cell(sheet.path(),
                                      selection.cell, input.value);
            } else {
                selection.cell_dom.innerHTML = (c && c.value) || "";
            }
        }
    };

    public.is_formula = function(val) {
        return input.value[0] == "=";
    };

    public.insert_range = function(range) {

        var f = input.value;
        var index = Math.max( f.lastIndexOf("="), f.lastIndexOf(","),
                              f.lastIndexOf("("), f.lastIndexOf("+"),
                              f.lastIndexOf("-"),
                              f.lastIndexOf("*"), f.lastIndexOf("/") );
        var formula = f.substring(0, index+1) + HN.Util.range_to_str2(range);
        input.value = formula;
        selection.formula.value = formula;
    };
    
    public.read_formula_bar = function(range) {
        setTimeout( function() {
            input.value = selection.formula.value;
            public.calculateWidth();
        },0);
    };
    
    return public;
};
