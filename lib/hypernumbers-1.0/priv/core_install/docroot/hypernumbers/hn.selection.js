/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false */

HN.Layout.Selection = function (layout, sheet)
{
    var api   = {},
    domeditor = HN.Util.id("editor"),
    dragged   = HN.Util.id("dragged");
    
    api.state      = HN.States.SELECTED_CELL;
    api.formula    = HN.Util.id("formula");
    api.wrapper    = HN.Util.id("selection");
    api.start_drag = false;
    api.tmpbounds  = {};

    api.dom = {
        "rowselection" : HN.Util.id("rowselection"),
        "colselection" : HN.Util.id("colselection"),
        "name"         : HN.Util.id("name"),
        "current"      : HN.Util.id("current")
    };
    
    api.has_scrolled   = false;
    api.last_click     = null;
    api.cell_dom       = null;
    api.cell           = null;
    api.in_formula_bar = false;
    api.copied_str     = "";
    api.copied         = {"x1": 1, "y1": 1, "x2": 1, "y2": 1};
    api.bounds         = {"x1": 1, "y1": 1, "x2": 1, "y2": 1};
    api.dragpos        = {"x1": 0, "y1": 0, "x2": 0, "y2": 0};

    api.panes = new HN.Layout.Panes(layout, api, sheet);
    
    api.editor = new HN.Layout.Editor(api, sheet);
    
    api.currentCellValue = function () {
        return (api.is_editing() && api.editor.value()) || 
            sheet.cell(api.cell.y, api.cell.x).value || "";
    };

    api.currentCellFormula = function () {
        return (api.is_editing() && api.editor.value()) || 
            sheet.cell(api.cell.y, api.cell.x).formula || "";
    };

    api.currentSelectedCell = function () {
        return api.cell;
    };

    api.currentSelectedBounds = function () {
        return api.bounds;
    };

    api.selectFullSheet = function () {
        api.bounds = { 
            "x1" : 1, 
            "y1" : 1,
            "x2" : sheet.max.x,
            "y2" : sheet.max.y
        };
        api.show_selection();
    };

    api.clearMenu = function (e, type) {
        HN.Callbacks.clear(api.bounds, type);
    };

    api.gridContextMenu = function (e) {
        
        var tmp = sheet.cell(api.cell.y, api.cell.x),
        el = HN.Util.id("gridcontextmenu"),
        unMerge = api.bounds.x1 === api.bounds.x2 && 
            api.bounds.y1 === api.bounds.y2 && tmp.merge,
        hide = function (e) {
            
            if (HN.Util.is_inside(e.target, el)) {
                
                var target = (e.target.nodeName !== "A") ? 
                    $(e.target).parents("a")[0] : e.target;
                
                if (!target || !api.menuItemPicked(target)) {
                    return;
                }
            }
            
            HN.Util.removeEvent(document, "mousedown", hide);
            el.style.display = "none";
        };
                
        $("#mergetext").text((unMerge) ? "Unmerge" : "Merge");

        var height = $(el).height();
        var top = ((e.clientY + height) > document.body.clientHeight) ?
            e.clientY - height : e.clientY;
        
        el.style.display = "block";
        el.style.top     = top + "px";
        el.style.left    = e.clientX + "px";

        HN.Util.addEvent(document, "mousedown", hide);

        e.preventDefault();
        return false;
    };

    api.axisContextMenu = function (e, axis)
    {
        var el = (axis === Y) ? HN.Util.id("axiscontextmenurow")
            : HN.Util.id("axiscontextmenucol"),        
        hide = function (e) {

            if (HN.Util.is_inside(e.target, el)) {
                
                var target = (e.target.nodeName !== "A") ? 
                    $(e.target).parents("a")[0] : e.target;
                
                if (!target || !api.menuItemPicked(target)) {
                    return;
                }
            }
            
            HN.Util.removeEvent(document, "mousedown", hide);
            el.style.display = "none";
        };

        el.style.display = "block";
        el.style.top  = e.clientY + "px";
        el.style.left = e.clientX + "px";

        HN.Util.addEvent(document, "mousedown", hide);

        e.preventDefault();
        return false;
    };

    api.mouseDownAxis = function (e, xy) {

        e.preventDefault();

        if (e.target.className === "handle" && e.button !== 2) {

            var attr = (xy === Y) ? "clientY" : "clientX",
            start   = e[attr],
            handle  = e.target,
            new_val = 0,
            cell    = handle.parentNode,
            orig    = parseInt(cell.style[xy.dimension], 10),
            index   = (xy === Y) ? parseInt(cell.textContent, 10)
                : HN.Util.from_b26(cell.textContent),
            axis    = (xy === Y) ? HN.Util.id("rsel") : HN.Util.id("csel"),
            mousemove, mouseup;
            
            axis.style.display           = "none";
            handle.style.backgroundColor = "#FF9966";
            document.body.style.cursor   = (xy === Y) ? "ns-resize" : "ew-resize";
            HN.Util.id("overlay").style.display = "block";


            mouseup = function (e) {

                HN.Util.id("overlay").style.display = "none";
                handle.style.backgroundColor        = "";
                document.body.style.cursor          = "";
                axis.style.display                  = "block";
                HN.Util.removeEvent(document, "mouseup", mouseup);
                HN.Util.removeEvent(document, "mousemove", mousemove);

                if (xy === Y) {
                    HN.Callbacks.setHeight(sheet.path(), 
                                           index, new_val + 4);
                } else {
                    HN.Callbacks.setWidth(sheet.path(),
                                          index, new_val + 4);
                }
            };

            mousemove = function (e) {
                new_val = orig + (e[attr] - start);
                if (new_val > 0) {
                    api.shiftAxis(cell, xy, index, (e[attr] - start));
                    cell.style[xy.dimension] =  new_val + "px";
                }
            };

            HN.Util.addEvent(document, "mouseup", mouseup);
            HN.Util.addEvent(document, "mousemove", mousemove);

        } else {

            var attr = (xy === Y) ? "clientY" : "clientX",
            offset   = (xy === Y) ? HN.Util.y_pos($("#scroller")[0])
                : HN.Util.x_pos($("#scroller")[0]),
            max  = (xy === Y) ? sheet.max.x : sheet.max.y,
            b = api.order_bounds(api.bounds),
            start = api.getNewOffset(e[attr] - offset, xy);

            if (e.button === 2 &&
                ((xy === Y && start.index >= b.y1 && 
                  start.index <= b.y2 && b.x1 === 1) ||
                 (xy === X && start.index >= b.x1 && 
                  start.index <= b.x2 && b.y1 === 1))) {
                return;
            }

            document.body.style.cursor = (xy === Y) ? "e-resize" 
                : "s-resize";
            HN.Util.id("overlay").style.display = "block";

            if (xy === Y) {
                api.bounds = {
                    "y1" : start.index, 
                    "x1" : 1, 
                    "y2" : start.index, 
                    "x2" : max
                };
                api.set_cell(start.index, 1);
            } else {
                api.bounds = {
                    "x1" : start.index, 
                    "y1" : 1, 
                    "x2" : start.index, 
                    "y2" : max
                };
                api.set_cell(1, start.index);
            }

            api.show_selection();

            var mouseup = function (e) {
                HN.Util.id("overlay").style.display = "none";
                document.body.style.cursor = "";

                HN.Util.removeEvent(document, "mouseup", mouseup);
                HN.Util.removeEvent(document, "mousemove", mousemove);
            };

            var mousemove = function (e) {
                var end = api.getNewOffset(e[attr] - offset, xy);

                if (xy === Y) {
                    api.bounds = { 
                        "y1": Math.min(start.index, end.index), 
                        "x1": 1,
                        "y2": Math.max(start.index, end.index), 
                        "x2": max 
                    };                    
                } else {
                    api.bounds = { 
                        "x1": Math.min(start.index, end.index), 
                        "y1": 1,
                        "x2": Math.max(start.index, end.index), 
                        "y2": max
                    };
                }
                api.show_selection();
                e.preventDefault();
                return false;
            };

            HN.Util.addEvent(document, "mouseup", mouseup);
            HN.Util.addEvent(document, "mousemove", mousemove);
        }
    };

    api.shiftAxis = function (element, axis, index, diff) {
        while (element.nextSibling && ++index) {
            element = element.nextSibling;
            var tmp = (axis === Y) ? sheet.cell_offset(index, 1)
                : sheet.cell_offset(1, index);
            element.style[axis.coord] = (tmp[axis.coord] + diff) + "px";
        }
    };
    
    api.is_viewable = function (y, x) {
        
        var ret = {"x": true, "y": true},
        coord   = sheet.cell_offset(y, x),
        top     = Math.abs(layout.gridPos.top()),
        left    = Math.abs(layout.gridPos.left()),
        
        is_above = coord.top < top,
        is_below = (coord.top + sheet.row_height(y)) > 
            top + layout.gridSizePix().y,

        is_left  = coord.left < left,
        is_right = (coord.left + sheet.col_width(x)) > 
            left + layout.gridSizePix().x;
      
        if (!is_above !== !is_below && y !== sheet.offset.y) {
            ret.y = false;
        }
        
        if (!is_left !== !is_right && x !== sheet.offset.x) {
            ret.x = false;
        }

        return ret;
    };

    api.scrollIntoView = function (y, x, dir) {

        var tmp = api.is_viewable(y, x);

        if (tmp.x && tmp.y) {
            api.has_scrolled = false;
            return;
        }
        
        if (!tmp.x && (dir === true)) {
            sheet.offset.x = x;
            api.panes.moveTo(sheet.offset.y, x);
        } else if (!tmp.x) {
            layout.scrollBy1(dir === HN.RIGHT, X);
            return api.scrollIntoView(y, x, dir);
        }
        
        else if (!tmp.y && (dir === true)) {
            sheet.offset.y = y;
            api.panes.moveTo(y, sheet.offset.x);
        } else if (!tmp.y) {
            layout.scrollBy1(dir === HN.DOWN, Y);
            return api.scrollIntoView(y, x, dir);
        }
        api.has_scrolled = false;
    };

    api.set_cell = function (y, x, dir) {

        var current = api.dom.current.style, arg,
        cell_attr   = sheet.cell(y, x),
        cell_pos    = sheet.cell_offset(y, x),
        formula     = (cell_attr.formula) || "",
        width       = cell_attr.mergeWidth  || sheet.col_width(x),
        height      = cell_attr.mergeHeight || sheet.row_height(y); 

        api.cell = {"y": y, "x": x};X

        if (dir) {
            arg = (!api.has_scrolled) ? dir : api.has_scrolled;
            api.scrollIntoView(y, x, arg);
        }

        api.cell_dom = api.panes.get_cell(y, x);
        api.editor.setFormula(formula);
        
        current.top    = (cell_pos.top - 1)  + "px";
        current.left   = (cell_pos.left - 1) + "px";
        current.width  = width + "px";
        current.height = height + "px";
    };

    api.canEdit = function () {
        return true;
    };

    api.keydown = function (e) {

        if (!layout.gridHasFocus()) {
            return;
        }

        if (e.keyCode === HN.Keys.BACKSPACE && api.is_selected() && 
            !api.in_formula_bar) {
            e.preventDefault();
        }

        api.do_keydown(e);
        
        if (!(e.keyCode === HN.Keys.DOWN || 
              e.keyCode === HN.Keys.UP || 
              e.keyCode === HN.Keys.TAB  || 
              e.keyCode === HN.Keys.RETURN || 
              e.keyCode === HN.Keys.LEFT || 
              e.keyCode === HN.Keys.RIGHT)) {
            return;
        }

        var timeout  = null,
        timer        = null,
        set_interval = function () {
            timer = setInterval(function () {
                api.do_keydown(e);
            }, 30);
        };
        
        function cleanup() {
            window.clearTimeout(timeout);
            HN.Util.removeEvent(document, "keyup", cleanup);
            HN.Util.removeEvent(window, "blur", cleanup);
            clearInterval(timer);
        }
        
        HN.Util.addEvent(document, "keyup", cleanup);
        HN.Util.addEvent(window, "blur", cleanup);

        timeout = window.setTimeout(set_interval, 300);
    };

    api.do_keydown = function (e) {

        if (api.in_formula_bar) {
            if (e.keyCode === HN.Keys.ENTER || 
                e.keyCode === HN.Keys.TAB) {
                api.select(e.keyCode, e);
            } else {
                if (api.is_selected()) {
                    api.startEditing();
                    api.formula.focus();
                }
                api.editor.read_formula_bar();
            }
        } else if ((e.keyCode === HN.Keys.ENTER || 
                    e.keyCode === HN.Keys.TAB) && 
                   (api.is_editing() || api.is_selected())) {

            if (e.shiftKey && !api.editor.is_formula()) {
               setTimeout(function () {
                   api.editor.setFormula(api.editor.value());
                   api.editor.calculateWidth();
               }, 0);
               return;
            }
            
            if (api.is_cell()) {
                api.select(e.keyCode, e);
            } else {
                api.select_next(e.keyCode, e);
            }
        } else if ((api.is_light_edit() || api.is_selected()) && 
                 (e.keyCode === HN.Keys.UP || 
                  e.keyCode === HN.Keys.DOWN || 
                  e.keyCode === HN.Keys.LEFT || 
                  e.keyCode === HN.Keys.RIGHT)) {
            e.preventDefault();
            api.select(e.keyCode, e);
        } else if (api.canEdit() && api.is_selected() && 
                   (e.keyCode === HN.Keys.DELETE || 
                    e.keyCode === HN.Keys.BACKSPACE)) {
            api.bounds = api.order_bounds(api.bounds);
            HN.Callbacks.clear(sheet.path(), api.bounds, "contents");
        }
        else if (api.is_selected()) {
            
            var inp = HN.Util.id("paste");

            if ((e.ctrlKey || e.metaKey) && e.keyCode === 86 && 
                api.canEdit()) {
                
                inp.value = "";
                inp.focus();

                setTimeout(function () {
                    api.pasteText(inp.value);
                    inp.blur();
                }, 0);

                // Copy
            } else if ((e.ctrlKey || e.metaKey) && e.keyCode === 67) {
                inp.value = api.copy();
                inp.select();
                inp.focus();
                setTimeout(function () {
                    inp.blur();
                }, 0);
            }
            
            else if (HN.Util.is_char(e.keyCode) && 
                     api.canEdit() && 
                     !(e.ctrlKey || e.metaKey)) {
                api.scrollIntoView(api.cell.y, api.cell.x, true);
                api.startEditing("");
            }
        } else if (api.is_editing()) {            
            setTimeout(function () {
                api.editor.setFormula(api.editor.value());
                api.editor.calculateWidth();
            }, 0);
        }
    };

    api.select = function (key, event) {

        if (event.shiftKey || key === HN.Keys.TAB) {
            event.preventDefault();
        }

        var b       = api.tmpbounds,
        cell        = HN.Util.clone(api.cell),
        cellAttr    = sheet.cell(cell.y, cell.x),
        down        = cellAttr.merge && cellAttr.merge.down + 1 || 1,
        right       = cellAttr.merge && cellAttr.merge.right + 1 || 1,
        dir         = null,
        selectCell, selectRange;
        
        selectCell = function () {
            
            // If cell is invisible due to merge, shift selection
            var tmp = sheet.cell(b.y1, b.x1);
            if (tmp.mergeTo) { 
                b.x1 = b.x2 = tmp.mergeTo.x;
                b.y1 = b.y2 = tmp.mergeTo.y;
            }

            api.tmpbounds = HN.Util.clone(b);
            api.bounds    = HN.Util.clone(b);
            api.set_cell(b.y1, b.x1, dir);
            api.state = HN.States.SELECTED_CELL;
            api.show_selection();
        };

        selectRange = function () {
            api.tmpbounds    = HN.Util.clone(b);
            api.bounds       = HN.Util.clone(b);
            api.has_scrolled = true;
            api.state        = HN.States.SELECTED_RANGE;
            api.show_selection();
        };


        if (api.is_editing()) {

            if (api.editor.is_formula() &&
                (key !== HN.Keys.ENTER && key !== HN.Keys.TAB)) {

                selectCell = function () {

                    var arg = (!api.has_scrolled) ? 
                        dir : api.has_scrolled, 
                    tmp;

                    api.scrollIntoView(b.y1, b.x1, arg);
                    
                    tmp = api.tmpbounds = b;
                    api.editor.insert_range(tmp);
                    api.highlightRange(HN.Util.id("dragfill"), tmp);
                };

                selectRange = function () {
                    var tmp = api.tmpbounds = b;
                    api.editor.insert_range(tmp);
                    api.highlightRange(HN.Util.id("dragfill"), tmp);
                };
                
                if (!api.start_drag) {
                    api.start_drag = true;
                    b = { "x1": b.x1, 
                          "y1": b.y1, 
                          "x2": b.x1, 
                          "y2": b.y2 };
                }

                cell = { "x": b.x1, "y": b.y1 };

            } else {
                api.editor.end_edit();
            }
        }

        if (!event.shiftKey) {

            if (key === HN.Keys.ENTER || key === HN.Keys.DOWN) {
                b.y1 = b.y2 = cell.y + down;
                b.x1 = b.x2 = cell.x;
                dir = HN.DOWN;
            } else if (key === HN.Keys.UP && cell.y > 1) {
                b.y1 = b.y2 = cell.y - 1;
                b.x1 = b.x2 = cell.x;
                dir = HN.UP;
            } else if (key === HN.Keys.LEFT && cell.x > 1) {
                b.y1 = b.y2 = cell.y;
                b.x1 = b.x2 = cell.x - 1;
                dir = HN.LEFT;
            } else if (key === HN.Keys.TAB || key === HN.Keys.RIGHT) {
                b.y1 = b.y2 = cell.y;
                b.x1 = b.x2 = cell.x + right;
                dir = HN.RIGHT;
            }
            
            if (dir) {
                selectCell();
            }

        } else {

            if (key === HN.Keys.DOWN && 
                cell.y === b.y2 && b.y1 !== b.y2) {
                b.y1 += 1;
                api.scrollIntoView(b.y1, b.x1, HN.DOWN);
            } else if (key === HN.Keys.DOWN) {
                b.y2 += 1;
                api.scrollIntoView(b.y2, b.x2, HN.DOWN);
            } else if (key === HN.Keys.UP && 
                       cell.y === b.y1 && b.y2 > 1) {
                b.y2 -= 1;
                api.scrollIntoView(b.y2, b.x2, HN.UP);
            } else if (key === HN.Keys.RIGHT && 
                       cell.x === b.x2 && b.x1 !== b.x2) {
                b.x1 += 1;
                api.scrollIntoView(b.y1, b.x1, HN.RIGHT);
            } else if (key === HN.Keys.RIGHT) {
                b.x2 += 1;
                api.scrollIntoView(b.y2, b.x2, HN.RIGHT);
            } else if (key === HN.Keys.LEFT && 
                       api.cell.x === b.x1 && b.x2 > 1) {
                b.x2 -= 1;
                api.scrollIntoView(b.y2, b.x2, HN.LEFT);
            }
            
            selectRange();
        }
    };

    api.select_next = function (key, event)
    {
        api.editor.end_edit();

        var cell = api.cell,
               b = api.order_bounds(api.bounds);

        if (key === HN.Keys.ENTER) {
            if (cell.y < b.y2) {
                api.set_cell(cell.y + 1, cell.x, HN.DOWN);
            } else if (cell.y === b.y2 && cell.x === b.x2) {
                api.has_scrolled = true;
                api.set_cell(b.y1, b.x1);
            } else {
                api.has_scrolled = true;
                api.set_cell(b.y1, cell.x + 1, HN.RIGHT);
            }
        }
        else if (key === HN.Keys.TAB)
        {
            event.preventDefault();

            if (cell.x < b.x2) {
                api.set_cell(cell.y, cell.x + 1, HN.RIGHT);
            } else if (cell.y === b.y2 && cell.x === b.x2) {
                api.has_scrolled = true;
                api.set_cell(b.y1, b.x1);
            } else {
                api.has_scrolled = true;
                api.set_cell(cell.y + 1, b.x1, HN.DOWN);
            }
        }
        api.state = HN.States.SELECTED_RANGE;
    };

    api.editPasteBounds = function (b, r2)
    {
        if (b.x1 === b.x2 && b.y1 === b.y2) {
            b.x2 = b.x1 + (r2.x2 - r2.x1);
            b.y2 = b.y1 + (r2.y2 - r2.y1);
            api.show_selection();
        } else if (
            ((b.x2 - b.x1 + 1) % (r2.x2 - r2.x1 + 1)) !== 0 ||
                ((b.y2 - b.y1 + 1) % (r2.y2 - r2.y1 + 1)) !== 0) {
            b.x2 = b.x1 + (r2.x2 - r2.x1);
            b.y2 = b.y1 + (r2.y2 - r2.y1);
            api.show_selection();
        }
    };

    api.paste = function (copied) {

        api.state = HN.States.SELECTED_RANGE;
        HN.Callbacks.pasteRange(sheet.path(), api.bounds, 
                                copied.host + copied.path, 
                                copied.bounds);
        api.editPasteBounds(api.bounds, copied.bounds);
    };

    api.copy = function () {
        $(".paste").removeClass("disabled");

        var str = api.build_clipboard_str();

        localStorage.copied = escape(JSON.stringify({
            "bounds" : api.bounds,
            "host"   : sheet.key("host"),
            "path"   : hn.currentPath(),
            "values" : str
        }));

        api.copied = HN.Util.clone(api.bounds);
        api.highlightRange(HN.Util.id("copied"), api.copied);

        return str;
    };

    api.pasteText = function (val) {

        var copied = JSON.parse(unescape(localStorage.copied)), 
        lines, i, tmp;
        
        // if the text pasted (ctrl+v) is the same as what was copied
        // use "proper" paste
        if (copied && val === copied.values) {
            api.paste(copied);
            return;
        }
        
        if (val.match("\t") === null && 
            val.match("\n") === null) {
            api.startEditing(val);
        } else {
            lines = val.split("\n");
            for (i = 0; i < lines.length; i = i + 1) {
                lines[i] = lines[i].split("\t");
                if (lines[i][lines[i].length] === "") {
                    lines[i].length -= 1;
                }
            }
            if (lines[lines.length - 1] === "") {
                lines.length -= 1;
            }

            api.state = HN.States.SELECTED_RANGE;
            HN.Callbacks.pasteValues(sheet.path(), api.bounds, lines);
            tmp = { 
                "x1": 1, 
                "y1": 1, 
                "x2": lines[0].length, 
                "y2": lines.length
            };
            api.editPasteBounds(api.bounds, tmp);
        }
    };

    api.getNewOffset = function (position, xy) {

        var i = 1,
        total = 0,
        f     = (xy === Y) ? "row_height" : "col_width";
        
        while (true) {

            total += sheet[f](i);
            
            if (total > position) {
                return {
                    "index"    : i, 
                    "position" : total - sheet[f](i) 
                };
            }

            i += 1;
        }
    };

    api.startEditing = function (val) {
        api.editor.start_edit(val);

        HN.Util.id("copied").style.display = "none"; 

        api.state = (api.state === HN.States.SELECTED_RANGE) ? 
            HN.States.EDIT_LIGHT_RANGE
            : HN.States.EDIT_LIGHT_CELL;
    };

    api.is_in_grid = function (auto, e)
    {
        var axis = auto.args.axis,
        dir  = auto.args.forward,
        x    = e.clientX,
        y    = e.clientY,
        gridSize = layout.gridSizePix(),
        xmargin = layout.gridMargin.x(),
        ymargin = layout.gridMargin.y();

        return (axis === Y && dir && !(y > ymargin + gridSize.y)) || 
            (axis === X && dir  && !(x > xmargin + gridSize.x)) || 
            (axis === Y && !dir && !(y < ymargin)) || 
            (axis === X && !dir && !(x < xmargin));
    };

    api.auto_scroll_coords = function (e)
    {
        var gridSize = layout.gridSizePix();

        if (e.clientY > layout.gridMargin.y() + gridSize.y) { 
            return {"forward": true, "axis": Y};
        } else if (e.clientY < layout.gridMargin.y()) { 
            return {"forward": false, "axis": Y};
        } else if (e.clientX > layout.gridMargin.x() + gridSize.x) {
            return {"forward": true, "axis": X};
        } else if (e.clientX < layout.gridMargin.x()) {
            return {"forward": false, "axis": X};
        }
        return false;
    };
    
    api.dragdown = function (e) {
        e.preventDefault();
        
        var style = layout.inner.style,
        auto      = {"timer": null, "axis": null},
        offy      = layout.gridMargin.y() + layout.gridPos.top(),
        offx      = layout.gridMargin.x() + layout.gridPos.left(),

        auto_scroll, stop_auto_scroll, mousemove, mouseup, x, y;

        api.state        = HN.States.DRAGGING;
        api.has_scrolled = true;
        api.dragpos      = null;

        auto_scroll = function () {
            layout.scrollBy1(auto.args.forward, auto.args.axis);
            offy   = layout.gridMargin.y() + layout.gridPos.top();
            offx   = layout.gridMargin.x() + layout.gridPos.left();
            api.show_drag_selection({
                "x": api.getNewOffset(auto.mouse.x - offx, X).index, 
                "y": api.getNewOffset(auto.mouse.y - offy, Y).index
            });
        };

        stop_auto_scroll = function () {
            window.clearInterval(auto.timer);
            auto = {
                "timer": null, 
                "axis": null, 
                "forward": null, 
                "mouse": null
            };
        };

        mousemove = function (e) {
            if (!auto.timer) {
                var args = api.auto_scroll_coords(e);
                if (args) {
                    auto = { "args":  args,
                             "mouse": {"y": e.clientY, "x": e.clientX},
                             "timer": setInterval(auto_scroll, 30) };
                } else {
                    api.show_drag_selection({
                        "x": api.getNewOffset(e.clientX - offx, X).index, 
                        "y": api.getNewOffset(e.clientY - offy, Y).index
                    });               
                }
            }
            else if (api.is_in_grid(auto, e)) {
                stop_auto_scroll();
            }
        };

        mouseup = function (e) {

            if (auto.timer) {
                window.clearInterval(auto.timer);
            }

            HN.Util.removeEvent(document, "mouseup", mouseup);
            HN.Util.removeEvent(document, "mousemove", mousemove);
            
            if (!api.dragpos) { 
                return;
            }

            HN.Callbacks.drag(sheet.path(), 
                              api.order_bounds(api.bounds), 
                              api.dragpos);

            api.bounds = {
                "y1": Math.min(api.bounds.y1, api.dragpos.y1),
                "x1": Math.min(api.bounds.x1, api.dragpos.x1),
                "y2": Math.max(api.bounds.y2, api.dragpos.y2),
                "x2": Math.max(api.bounds.x2, api.dragpos.x2)
            };

            api.resumeSelection();

            api.show_selection();
            dragged.style.display = "none";
        };

        HN.Util.addEvent(document, "mouseup", mouseup);
        HN.Util.addEvent(document, "mousemove", mousemove);
    };

    api.resumeSelection = function () {
        api.state = (api.bounds.y1 === api.bounds.y2 && 
                     api.bounds.x1 === api.bounds.x2) ? 
            HN.States.SELECTED_CELL : HN.States.SELECTED_RANGE;
    };

    api.resume_editing = function () {
        api.state = (api.bounds.y1 === api.bounds.y2 && 
                     api.bounds.x1 === api.bounds.x2) ? 
            HN.States.EDIT_LIGHT_CELL : HN.States.EDIT_LIGHT_RANGE;
    };

    api.dblclick = function (e) {
        if (!layout.gridHasFocus() || !api.canEdit()) {
            return;
        }

        if (api.state !== HN.States.EDIT_FULL_CELL) {
            var val = sheet.cell(api.cell.y, api.cell.x).formula || "";
            api.editor.start_edit(val);
            api.state = HN.States.EDIT_FULL_CELL;
        }
    };

    api.mousedown = function (e) {
        
        if ($(e.target).parents("#grid").length === 0 && 
            $(e.target).attr("id") !== "formula") {
            
            if (layout.gridHasFocus()) {
                layout.grabFocus();
            }
            return;
        } else if (!layout.gridHasFocus()) {
            // In case url has focus
            $("#paste")[0].focus();
            layout.resumeSelection();
        }

        // Dont interfere with right clicks on selections
        if (e.button === 2 && e.target === api.wrapper || 
            e.button === 2 && e.target === api.dom.current) {
            return true;
        }

        // Ignore clicks inside active formula
        if (api.is_editing() && HN.Util.is_inside(e.target, domeditor)) {
            return true;
        }

        // Let click on links in cells 
        if (e.target.nodeName === "A" && (e.ctrlKey || e.metaKey)) {
            document.location.href = e.target.getAttribute("href");
        }

        e.preventDefault();

        var style  = layout.inner.style,
        auto   = {},
        bounds = {},
        offy   = layout.gridMargin.y() + layout.gridPos.top(),
        offx   = layout.gridMargin.x() + layout.gridPos.left(),

        select, select_start, select_end, auto_scroll, stop_auto_scroll,
        mouseup, mousemove;

        api.has_scrolled = true;

        // Stop double clicks being screwed with
        if (api.last_click && 
            new Date().getTime() - 250 < api.last_click) {
            return true;
        } else {
            api.last_click = new Date().getTime();
        }


        if (api.in_formula_bar) {
            api.in_formula_bar = false;
            if (e.target === api.editor.input) {
                return true;
            }
        }

        select = function () {
            api.bounds = bounds;
            api.show_selection();
        };

        select_start = function () {
            
            var cell = sheet.cell(bounds.y1, bounds.x1);
            if (cell.mergeTo) { 
                bounds = {
                    "x1": cell.mergeTo.x,
                    "x2": cell.mergeTo.x,
                    "y1": cell.mergeTo.y,
                    "y2": cell.mergeTo.y
                };
            }

            api.bounds = bounds;
            api.set_cell(bounds.y1, bounds.x1);
            api.tmpbounds = bounds;
            api.show_selection();
        };

        select_end = function () {
            api.resumeSelection();
        };

        if (api.is_editing()) {
            if (api.editor.is_formula()) {
                select = function () {
                    var tmp = api.order_bounds(bounds);
                    api.editor.insert_range(tmp);
                    api.highlightRange(HN.Util.id("dragfill"), tmp);
                };
                select_start = function () {
                    var tmp = api.order_bounds(bounds);
                    api.tmpbounds = bounds;
                    api.editor.insert_range(tmp);
                    api.highlightRange(HN.Util.id("dragfill"), tmp);
                };
                select_end = function () { };

            } else {
                api.editor.end_edit();
            }
        }

        if (e.shiftKey) {
            bounds = { 
                "x1": api.tmpbounds.x1, 
                "x2": api.getNewOffset(e.clientX - offx, X).index,
                "y1": api.tmpbounds.y1, 
                "y2": api.getNewOffset(e.clientY - offy, Y).index 
            };
            select();
            setTimeout(function () { 
                select_end(); 
            }, 1000);

            return true;
        }

        auto_scroll = function () {

            layout.scrollBy1(auto.args.forward, auto.args.axis);
            offy   = layout.gridMargin.y() + layout.gridPos.top();
            offx   = layout.gridMargin.x() + layout.gridPos.left();
            
            bounds.y2 = api.getNewOffset(auto.mouse.y - offy, Y).index;
            bounds.x2 = api.getNewOffset(auto.mouse.x - offx, X).index;
            select();
        };

        stop_auto_scroll = function () {
            window.clearInterval(auto.timer);
            auto = {
                "timer"   : null, 
                "axis"    : null, 
                "forward" : null, 
                "mouse"   : null
            };
        };

        var y = api.getNewOffset(e.clientY - offy, Y);
        var x = api.getNewOffset(e.clientX - offx, X);

        bounds = {
            "y1": y.index, 
            "x1": x.index, 
            "y2": y.index, 
            "x2": x.index
        };

        select_start();
        
        mousemove = function (e) {

            e.preventDefault();
            
            if (!auto.timer) {
                var args = api.auto_scroll_coords(e);
                if (args) {
                    auto = { 
                        "args":  args,
                        "mouse": {"y": e.clientY, "x": e.clientX},
                        "timer": setInterval(auto_scroll, 30) 
                    };
                } else {

                    var y2 = api.getNewOffset(e.clientY - offy, Y);
                    var x2 = api.getNewOffset(e.clientX - offx, X);

                    bounds.y2 = y2.index;
                    bounds.x2 = x2.index;

                    select();
                }
            }
            else if (api.is_in_grid(auto, e)) {
                stop_auto_scroll();
            }
        };

        mouseup = function (e) {

            if (auto.timer) {
                window.clearInterval(auto.timer);
            }

            document.body.style.cursor = "";
            HN.Util.id("overlay").style.display = "none";
            api.mouse = {"x1": 0, "y1": 0, "x2": 0, "y2": 0};
            HN.Util.removeEvent(document, "mouseup", mouseup);
            HN.Util.removeEvent(document, "mousemove", mousemove);
            select_end();
        };

        document.body.style.cursor = "cell";
        HN.Util.id("overlay").style.display = "block";
        HN.Util.addEvent(document, "mouseup", mouseup);
        HN.Util.addEvent(document, "mousemove", mousemove);
    };

    api.show_drag_selection = function (drag) {
        var pos = function (d, b) {
            if (d.x >= b.x1 && d.x <= b.x2 && d.y > b.y2) {
                return [b.y2 + 1, b.x1, d.y, b.x2];
            } else if (d.x >= b.x1 && d.x <= b.x2 && d.y < b.y1) {
                return [d.y, b.x1, b.y1 - 1, b.x2];
            } else if (d.y >= b.y1 && d.y <= b.y2 && d.x > b.x2) {
                return [b.y1, b.x2 + 1, b.y2, d.x];
            } else if (d.y >= b.y1 && d.y <= b.y2 && d.x < b.x1) {
                return [b.y1, d.x, b.y2, b.x1 - 1];
            }
        },
        npos = pos(drag, api.order_bounds(api.bounds));

        if (npos) {
            api.dragpos = { 
                "y1": npos[0], 
                "x1": npos[1],
                "y2": npos[2], 
                "x2": npos[3]
            };
            api.highlightRange(dragged, api.dragpos);
        }
    };
    
    api.order_bounds = function (range) {
        return {
            "x1": Math.min(range.x1, range.x2),
            "x2": Math.max(range.x1, range.x2),
            "y1": Math.min(range.y1, range.y2),
            "y2": Math.max(range.y1, range.y2)
        };
    };

    api.show_cell = function () {
        
        var current = api.dom.current.style,
        cellAttr    = sheet.cell(api.cell.y, api.cell.x),
        cellPos     = sheet.cell_offset(api.cell.y, api.cell.x),
        formula     = (cellAttr.formula) || "",
        width       = sheet.cellWidth(cellAttr, api.cell.x),
        height      = sheet.cellHeight(cellAttr, api.cell.y);
        
        api.editor.setFormula(formula);

        current.top       = (cellPos.top - 1) + "px";
        current.left      = (cellPos.left - 1) + "px";
        current.height    = height + "px";
        current.width     = width + "px";
    };

    api.show_selection = function () {

        var bounds = api.order_bounds(api.bounds),
        coords     = api.highlightRange(api.wrapper, bounds),
        col        = api.dom.colselection,
        row        = api.dom.rowselection;

        api.dom.name.innerHTML = HN.Util.range_to_str(bounds);
        
        row.style.top    = (coords.top) + "px";
        row.style.height = (coords.height) + "px";
        
        col.style.left   = (coords.left) + "px";
        col.style.width  = (coords.width) + "px";
    };

    api.highlightRange = function (highlight, range) {

        var style = highlight.style,
        coords    = sheet.cell_offset(range.y1, range.x1),
        width     = 0, height, 
        cell, y, x, rowh, tmpw;

        for (y = range.y1, height = 0; y < range.y2 + 1; y += 1) {

            rowh = sheet.row_height(y); 
            
            for (x = range.x1, tmpw = 0; x < range.x2 + 1; x += 1) {
                
                cell = sheet.cell(y, x);
                
                if (cell.merge) { 
                    
                    tmpw += cell.mergeWidth;
                    rowh = cell.mergeHeight;
                    
                    x += cell.merge.right;
                    y += cell.merge.down;
                    
                } else { 
                    tmpw += sheet.col_width(x);
                }
            }
            if (tmpw > width) { 
                width = tmpw;
            }
            
            height += rowh;
        }

        style.left    = (coords.left - 1) + "px";
        style.top     = (coords.top - 1)  + "px";
        style.width   = (width - 2)   + "px";
        style.height  = (height - 2)  + "px";
        style.display = "block";

        return { 
            "width"  : width, 
            "height" : height, 
            "top"    : coords.top,   
            "left"   : coords.left 
        };
    };

    api.build_clipboard_str = function () {

        var str = "", text, 
        tocopy = "formula", y, x;

        api.bounds = api.order_bounds(api.bounds);
        for (y = api.bounds.y1; y <= api.bounds.y2; y += 1) {
            for (x = api.bounds.x1; x <= api.bounds.x2; x += 1) {
                text = sheet.cell(y, x)[tocopy] || "";
                str += text.replace(/\n|\t/g, "");
                if (x < api.bounds.x2) {
                    str += "\t";
                }
            }
            if (y < api.bounds.y2) {
                str += "\n";
            }
        }
        if (str === "") { 
            str = " ";
        }
        return str;
    };

    api.is_editing = function () {
        return api.is_full_edit() || api.is_light_edit();
    };

    api.is_full_edit = function () {
        return api.state === HN.States.EDIT_FULL_CELL;
    };

    api.is_light_edit = function () {
        return api.state === HN.States.EDIT_LIGHT_CELL || 
            api.state === HN.States.EDIT_LIGHT_RANGE;
    };

    api.is_cell = function () {
        return api.state === HN.States.EDIT_FULL_CELL || 
            api.state === HN.States.EDIT_LIGHT_CELL || 
            api.state === HN.States.SELECTED_CELL;
    };

    api.is_selected = function () {
        return api.state === HN.States.SELECTED_RANGE || 
            api.state === HN.States.SELECTED_CELL;
    };

    api.menuItemPicked = function (e) {

        if (typeof e === "undefined") {
            return true;
        }

        if (e.parentNode && e.parentNode.className.match("disabled")) {
            return false;
        }

        var action = e.getAttribute("name");
        if (action !== null && 
            typeof this["menuAction_" + action] === "function") {
            return this["menuAction_" + action](e);
        }
    };

    api.menuAction_deleterows = function (e) {
        HN.Callbacks.deleteRowCol(sheet.path(), api.bounds, Y);
        return true;
    };
    api.menuAction_deletecols = function (e) {
        HN.Callbacks.deleteRowCol(sheet.path(), api.bounds, X);
        return true;
    };
    api.menuAction_deleteleft = function (e) {
        HN.Callbacks.deleteRange(sheet.path(), api.bounds, "horizontal")
        return true;
    };
    api.menuAction_deleteup = function (e) {
        HN.Callbacks.deleteRange(sheet.path(), api.bounds, "vertical")
        return true;
    };

    api.menuAction_insertright = function (e) {
        HN.Callbacks.insertRange(sheet.path(), api.bounds, "horizontal")
        return true;
    };
    api.menuAction_insertdown = function (e) {
        HN.Callbacks.insertRange(sheet.path(), api.bounds, "vertical")
        return true;
    };
    api.menuAction_insertrowsafter = function (e) {
        HN.Callbacks.insertRowCol(sheet.path(), api.bounds, Y, "after");
        return true;
    };
    api.menuAction_insertrowsbefore = function (e) {
        HN.Callbacks.insertRowCol(sheet.path(), 
                                  api.bounds, Y, "before");
        return true;
    };
    api.menuAction_insertcolsafter = function (e) {
        HN.Callbacks.insertRowCol(sheet.path(), 
                                  api.bounds, X, "after");
        return true;
    };
    api.menuAction_insertcolsbefore = function (e) {
        HN.Callbacks.insertRowCol(sheet.path(), 
                                  api.bounds, X, "before");
        return true;
    };

    api.menuAction_import = function (e) {
        HN.UI.open_dialog(layout, "import");
        return true;
    };

    api.menuAction_delete = function (e) {
        HN.Callbacks.deletePage();
        return true;
    };

    api.menuAction_mergecells = function (e) {
        HN.Callbacks.mergeCells(sheet.path(), 
                                api.order_bounds(api.bounds));
        return true;
    };

    api.menuAction_functions = function (e) {
        HN.Util.id("functions").style.display = "block";
        return true;
    };

    api.menuAction_feedback = function (e) {
        HN.UI.open_dialog(layout, "mark");
        return true;
    };

    api.menuAction_paste = function (e) {
        api.paste(JSON.parse(unescape(localStorage.copied)));
        return true;
    };

    api.menuAction_pastevalue = function (e) {
        var clipboard = JSON.parse(unescape(localStorage.copied));
        api.pasteText(clipboard.values);
        return true;
    };

    api.menuAction_pastelink = function (e) {
        var clipboard = JSON.parse(unescape(localStorage.copied)),
        b         = clipboard.bounds,
        arr       = [], y, x;
        
        for (y = 0; y <= b.y2 - b.y1; y += 1) {
            arr[y] = [];
            for (x = 0; x <= b.x2 - b.x1; x += 1) {
                arr[y][x] = "=" + clipboard.path +
                    HN.Util.coord_to_ref({
                        "x": x + b.x1, 
                        "y": y + b.y1
                    });
            }
        }
        HN.Callbacks.pasteValues(sheet.path(), api.bounds, arr);

        api.editPasteBounds(api.bounds, clipboard.bounds);
        return true;
    };

    api.menuAction_copy = function (e) {
        api.copy();
        return true;
    };

    api.menuAction_copyFormula = function (e) {
        if ($("#copyformula").is(":checked")) {
            $("#copyformula").removeAttr("checked");
        } else { 
            $("#copyformula").attr("checked", "checked");
        }
        return false;
    };

    api.menuAction_clearboth = function (e) {
        HN.Callbacks.clear(sheet.path(), api.bounds, "all");
        return true;
    };

    api.menuAction_clearformat = function (e) {
        HN.Callbacks.clear(sheet.path(), api.bounds, "style");
        return true;
    };

    api.menuAction_clearcontent = function (e) {
        HN.Callbacks.clear(sheet.path(), api.bounds, "contents");
        return true;
    };

    api.menuAction_newview = function (e) {    
        HN.Callbacks.setMark({"value": "New View Created"});
        window.location.hash = hn.hashUrl.setParam("view", "create");
        return true;
    };

    api.menuAction_view = function (e) {
        var view = $(e).attr("data-path");
        HN.Callbacks.setMark({"value": "View: " + view + " Opened"});
        window.location.hash = hn.hashUrl.setParam("view", view);
        return true;
    };

    api.set_cell(1, 1);
    api.show_selection();

    return api;
};



HN.Layout.Editor = function (selection, sheet) {

    var api = {},
    layout = selection.layout,
    input  = HN.Util.id("input"),
    editor = document.getElementById("editor"),
    hidden = HN.Util.id("hidden_input");

//    editor.contentDocument.designMode = "On";

//    var head = editor.contentWindow.document.getElementsByTagName("head")[0];
//    var link = editor.contentWindow.document.createElement("link");
//    link.setAttribute("rel", "stylesheet");
//    link.setAttribute("href", "/hypernumbers/hn.style.css");
//    head.appendChild(link);


    api.value = function () {
        return editor.innerHTML;//.replace(/<br>$/, "");
    };

    api.set = function (value) { 
        editor.innerHTML = value || "<br />";
    };

    api.calculateWidth = function () {

        var index = 0,
        val       = 0,
        tmp       = 0,
        fn        = "col_width", xy = X, cell, t;

        hidden.innerHTML = api.value() || "&nbsp;";
        input.style.height = hidden.clientHeight + "px";
        
        val = hidden.clientWidth + 5;

        do {
            tmp += sheet[fn](selection.cell[xy.str] + index);
            input.style[xy.dimension] = tmp + "px";
            index += 1;
        } while ((val + 10) > tmp);
    };

    api.start_edit = function (val) {

        var s     = input.style,
        cell      = selection.cell,
        cell_attr = sheet.cell(cell.y, cell.x),
        cell_pos  = sheet.cell_offset(cell.y, cell.x),
        style     = "background:#FFF;" + 
            sheet.lookupCSS(cell.y, cell.x) +
            "text-align:left; display:block;outline:none;resize: none;";
        
        selection.cell_dom.innerHTML = "";
        hidden.setAttribute("style", style);
                
        s.width  = "0px";
        s.height = (sheet.row_height(cell.y) - 2) + "px";
        s.top    = (cell_pos.top - 1)  + "px";
        s.left   = (cell_pos.left - 1) + "px";

        editor.setAttribute("style", style);
        editor.innerHTML = val || "<br />";
        api.calculateWidth();       
        s.display = "block";

        editor.setAttribute("contentEditable", "true");
        editor.focus();

        //setSelectionToEnd();
    };

    api.end_edit = function (val) {
        
        if (selection.in_formula_bar) {
            selection.in_formula_bar = false;
        }
        if (selection.formula) {
            selection.formula.blur();
        }
        
        editor.blur();
        editor.removeAttribute("contentEditable");
        
        HN.Util.id("dragfill").style.display = "none";
        selection.start_drag = false;
        
        var c   = sheet.cell(selection.cell.y, selection.cell.x),
        formula = (c && c.formula) || "",
        val = api.value().replace(/<br>$/, "");

        if (val.indexOf("&lt;") === 0 || val.indexOf("=") === 0) { 
            val = entitiesToHTML(val);
        }

        if (selection.is_light_edit()) {
            selection.cell_dom.innerHTML = val;
            if (c) {
                c.value = val;
            }

            HN.Callbacks.set_cell(sheet.path(), selection.cell, val);
        } else if (selection.is_full_edit()) {

            if (val !== formula) {
                selection.cell_dom.innerHTML = val;

                if (c) {
                    c.value = val;
                }

                HN.Callbacks.set_cell(sheet.path(),
                                      selection.cell, val);
            } else {
                selection.cell_dom.innerHTML = (c && c.value) || "";
            }
        }
        
        hidden.innerHTML = "";
        editor.innerHTML = "";
        input.style.display = "none";
    };

    api.is_formula = function (val) {
        return api.value()[0] === "=";
    };
    
    api.insert_range = function (range) {
        var f = api.value(),
        index = Math.max(f.lastIndexOf("="), f.lastIndexOf(","),
                         f.lastIndexOf("("), f.lastIndexOf("+"),
                         f.lastIndexOf("-"),
                         f.lastIndexOf("*"), f.lastIndexOf("/")),
        formula = f.substring(0, index + 1) + 
            HN.Util.range_to_str2(range);

        api.set(formula);
        
        selection.formula.innerHTML = formula;
        setSelectionToEnd();
    };
    
    api.read_formula_bar = function (range) {
        setTimeout(function () {
            api.set(selection.formula.innerHTML);
            api.calculateWidth();
        }, 0);
    };

    api.setFormula = function (value) { 
        selection.formula.innerHTML = value;
    };
    
    function setSelectionToEnd() { 
        // 1 seems to put the selection at the end, no idea why
        // works for now (TODO:fix for webkit)
        var range = window.getSelection().getRangeAt(0);
        range.setStart(editor, 1);
        range.setEnd(editor, 1);
    };

    function entitiesToHTML(str) { 
        return str.replace(/&amp;/g, "&")
            .replace(/&gt;/g, ">")
            .replace(/&lt;/g, "<");
    };

    
    return api;
};
