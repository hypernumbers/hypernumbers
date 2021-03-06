/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false */

HN.Layout.Selection = function (layout, sheet) {

    var api       = {},
        pasteDom  = HN.Util.id("paste"),
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
    api.wikiselect     = [];
    api.in_formula_bar = false;
    api.copied_str     = "";
    api.copied         = {"x1": 0, "y1": 0, "x2": 0, "y2": 0};
    api.bounds         = {"x1": 1, "y1": 1, "x2": 1, "y2": 1};
    api.dragpos        = {"x1": 0, "y1": 0, "x2": 0, "y2": 0};
    api.panes = new HN.Layout.Panes(layout, api, sheet);
    api.editor = new HN.Layout.Editor(layout, api, sheet);

    api.currentWikiSelection = function () {
        return api.wikiselect;
    };

    api.currentCellValue = function () {
        return (api.is_editing() && api.editor.value()) ||
            sheet.cell(api.cell.y, api.cell.x).value || "";
    };

    api.currentInlineDropdown = function () {
        return sheet.getInlineDropdown(api.cell.x, api.cell.y);
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

        var tmp     = sheet.cell(api.cell.y, api.cell.x),
            el      = HN.Util.id("gridcontextmenu"),
            unMerge = api.bounds.x1 === api.bounds.x2 &&
                api.bounds.y1 === api.bounds.y2 && tmp.merge,
            height, target, top,
            hide = function (e) {

            if (HN.Util.is_inside(e.target, el)) {

                target = (e.target.nodeName !== "A") ?
                    $(e.target).parents("a")[0] : e.target;

                if (!target || !api.menuItemPicked(target)) {
                    return;
                }
            }

            HN.Util.removeEvent(document, "mousedown", hide);
            el.style.display = "none";
        };

        $("#mergetext").text((unMerge) ? "Unmerge" : "Merge");

        height = $(el).height();
        top = ((e.clientY + height) > document.body.clientHeight) ?
            e.clientY - height : e.clientY;

        el.style.display = "block";
        el.style.top     = top + "px";
        el.style.left    = e.clientX + "px";

        // if it is a cell selected and not a range then show the logs sub-menu
        if ((api.bounds.x1 === api.bounds.x2) &&
            (api.bounds.y1 === api.bounds.y2)) {
            $(".viewlogs").show();
        } else {
            $(".viewlogs").hide();
        }
        HN.Util.addEvent(document, "mousedown", hide);

        e.preventDefault();
        return false;
    };

    api.axisContextMenu = function (e, axis)  {

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

    api.formulaFocus = function () {
        if (!api.in_formula_bar) {
            $("#formulawrapper").addClass("active");
            api.in_formula_bar = true;
            api.state = HN.States.EDIT_FULL_CELL;
            api.editor.start_edit("");
        }
        return true;
    };

    api.mouseDownAxis = function (e, xy) {

        var attr, start, handle, new_val, cell, orig, index, axis,
        mousemove, mouseup, offset, max, b, new_width, new_height;

        e.preventDefault();

        if (e.target.className === "handle" && e.button !== 2) {

            attr = (xy === Y) ? "clientY" : "clientX";
            start   = e[attr];
            handle  = e.target;
            new_val = 0;
            cell    = handle.parentNode;
            orig    = parseInt(cell.style[xy.dimension], 10);
            index   = (xy === Y) ? parseInt(cell.textContent, 10)
                : HN.Util.from_b26(cell.textContent);
            axis    = (xy === Y) ? HN.Util.id("rsel") : HN.Util.id("csel");

            axis.style.display           = "none";
            handle.style.backgroundColor = "#FF9966";
            document.body.style.cursor   = (xy === Y) ?
                "ns-resize" : "ew-resize";
            HN.Util.id("overlay").style.display = "block";

            mouseup = function (e) {

                HN.Util.id("overlay").style.display = "none";
                handle.style.backgroundColor        = "";
                document.body.style.cursor          = "";
                axis.style.display                  = "block";
                HN.Util.removeEvent(document, "mouseup", mouseup);
                HN.Util.removeEvent(document, "mousemove", mousemove);

                if (xy === Y) {
                    HN.Util.postRow(sheet.path(), index, index,
                                    {"set": {"fixedHeight": true}});

                    new_height = HN.Util.snapHeight(new_val + 4);
                    HN.Callbacks.setHeight(sheet.path(),
                                           index, index, new_height);
                } else {
                    new_width = HN.Util.snapWidth(new_val + 4);
                    HN.Callbacks.setWidth(sheet.path(),
                                          index, index, new_width);
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

            attr   = (xy === Y) ? "clientY" : "clientX";
            offset = (xy === Y) ? HN.Util.y_pos($("#scroller")[0])
                : HN.Util.x_pos($("#scroller")[0]);
            max    = (xy === Y) ? sheet.max.x : sheet.max.y;
            b      = api.order_bounds(api.bounds);
            start  = api.getNewOffset(e[attr] - offset, xy);

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

            mouseup = function (e) {
                HN.Util.id("overlay").style.display = "none";
                document.body.style.cursor = "";

                HN.Util.removeEvent(document, "mouseup", mouseup);
                HN.Util.removeEvent(document, "mousemove", mousemove);
            };

            mousemove = function (e) {
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

        var ret      = {"x": true, "y": true},
            coord    = sheet.cell_offset(y, x),
            top      = Math.abs(layout.gridPos.top()),
            left     = Math.abs(layout.gridPos.left()),

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
            return false;
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
        return true;
    };

    api.set_cell = function (y, x, dir) {

        var current    = api.dom.current.style, arg,
            cell_attr  = sheet.cell(y, x),
            cell_pos   = sheet.cell_offset(y, x),
            rawformula = cell_attr.formula || "",
            formula    = HN.Util.htmlEncode(rawformula),
            width      = cell_attr.mergeWidth  || sheet.col_width(x),
            height     = cell_attr.mergeHeight || sheet.row_height(y),
            wikiselect = [];

        api.cell = {"y": y, "x": x};

        if (dir) {
            arg = (!api.has_scrolled) ? dir : api.has_scrolled;
            api.scrollIntoView(y, x, arg);
        }

        api.cell_dom = api.panes.get_cell(y, x);
        api.editor.setFormula(formula);
        if (cell_attr.input && cell_attr.input !== "inline") {
            if (cell_attr.input.select) {
                wikiselect = cell_attr.input.select;
            } else if (cell_attr.input.dynamic_select) {
                wikiselect = cell_attr.input.dynamic_select;
            }
        };
        api.wikiselect = wikiselect;

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

        var timeout      = null,
            timer        = null,
            set_interval = function () {
                timer = setInterval(function () {
                    api.do_keydown(e);
                }, 60);
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

        if (e.keyCode === HN.Keys.ESC) {
            e.preventDefault();
            api.editor.quitEdit();
            return;
        }

        if (api.in_formula_bar) {
            if (e.keyCode === HN.Keys.ENTER ||
                e.keyCode === HN.Keys.TAB) {
                if (e.shiftKey && !api.editor.is_formula()) {
                    setTimeout(function () {
                        api.editor.read_formula_bar();
                    }, 0);
                    return;
                } else {
                    api.select(e.keyCode, e);
                }
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

	          // TODO different behaviour in shift+Enter and shift+Tab -
	          // but shift+Tab in Excel does not trigger this. What
	          // about Google Docs?

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

            if ((e.ctrlKey || e.metaKey) && e.keyCode === 86 &&
                api.canEdit()) {

                pasteDom.value = "";
                pasteDom.focus();

                setTimeout(function () {
                    api.pasteText(pasteDom.value);
                }, 0);

                // Copy
            } else if ((e.ctrlKey || e.metaKey) && e.keyCode === 67) {
                pasteDom.value = api.copy();
                pasteDom.select();
            } else if (HN.Util.is_char(e.keyCode) && api.canEdit() &&
                     !(e.ctrlKey || e.metaKey)) {
                api.scrollIntoView(api.cell.y, api.cell.x, true);
                api.startEditing("");
                // Bold (see also is_editing)
            } else if ((e.ctrlKey) && e.keyCode === 66) {
                $("#bold").mousedown();
                e.preventDefault();
                // Italic (see also is_editing)
            } else if ((e.ctrlKey) && e.keyCode === 73) {
                $("#italic").mousedown();
                e.preventDefault();
            }

        } else if (api.is_editing()) {
            // Bold (see also not in is_editing)
            if ((e.ctrlKey) && e.keyCode === 66) {
                $("#bold").mousedown();
                e.preventDefault();
                // Italic (see also not in is_editing)
            } else if ((e.ctrlKey) && e.keyCode === 73) {
                $("#italic").mousedown();
                e.preventDefault();
            }
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

        var b        = api.tmpbounds,
            cell     = HN.Util.clone(api.cell),
            cellAttr = sheet.cell(cell.y, cell.x),
            down     = cellAttr.merge && cellAttr.merge.down + 1 || 1,
            right    = cellAttr.merge && cellAttr.merge.right + 1 || 1,
            dir      = null,
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

                    var arg = (!api.has_scrolled) ? dir : api.has_scrolled,
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

            if (key === HN.Keys.TAB && cell.x > 1) {
                b.y1 = b.y2 = cell.y;
                b.x1 = b.x2 = cell.x - 1;
                dir = HN.LEFT;
            } else if (key === HN.Keys.DOWN &&
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

	    if (dir) {
                selectCell();
	    } else {
		selectRange();
	    }
        }
    };

    api.select_next = function (key, event) {

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

    api.editPasteBounds = function (b, r2) {

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

    api.pasteValue = function (copied) {
        api.state = HN.States.SELECTED_RANGE;
        HN.Callbacks.pasteValue(sheet.path(), api.bounds,
                                copied.host + copied.path,
                                copied.bounds);
        api.editPasteBounds(api.bounds, copied.bounds);
    };

    api.pasteStyle = function (copied) {
        HN.Callbacks.pasteStyle(sheet.path(), api.bounds,
                                copied.host + copied.path, copied.bounds);
        api.bounds = copied.bounds;
    };

    api.show_copied = function () {
        api.highlightRange(HN.Util.id("copied"), api.copied);
    };

    api.copy = function () {

        var str = api.build_clipboard_str();

        if (HN.Util.localStorage()) {
            $(".paste").removeClass("disabled");


            localStorage.copied = escape(JSON.stringify({
                "bounds" : api.bounds,
                "host"   : sheet.key("host"),
                "path"   : hn.currentPath(),
                "values" : str
            }));
        }
        // reset the copied bounds
        api.copied = HN.Util.clone(api.bounds);
        api.show_copied();

        return str;
    };

    api.pasteText = function (val) {
        var copied = (HN.Util.localStorage()) ?
            JSON.parse(unescape(localStorage.copied)) : false,
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

            if (lines[lines.length - 1].length === 1 && lines[lines.length - 1][0] === "") {
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

        var i     = 1,
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

        api.editor.start_edit(val, true);

        HN.Util.id("copied").style.display = "none";

        api.state = (api.state === HN.States.SELECTED_RANGE) ?
            HN.States.EDIT_LIGHT_RANGE
            : HN.States.EDIT_LIGHT_CELL;
    };

    api.is_in_grid = function (auto, e) {
        var axis     = auto.args.axis,
            dir      = auto.args.forward,
            x        = e.clientX,
            y        = e.clientY,
            gridSize = layout.gridSizePix(),
            xmargin  = layout.gridMargin.x(),
            ymargin  = layout.gridMargin.y();

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

        pasteDom.value = "";
        pasteDom.focus();

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
            var rawformula = sheet.cell(api.cell.y, api.cell.x).formula || "",
                val = HN.Util.htmlEncode(rawformula);
            api.editor.start_edit(val, true);
            api.state = HN.States.EDIT_FULL_CELL;
        }
    };

    api.mousedown = function (e) {

        var style, auto, bounds, offy, offx, select, select_start, select_end,
            auto_scroll, stop_auto_scroll, mouseup, mousemove, y, x, href;

        if ($(e.target).parents("#grid").length === 0) {
            // e.preventDefault();
            return;
        }

        // Dont interfere with right clicks on selections
        if (e.button === 2 && e.target === api.wrapper ||
            e.button === 2 && e.target === api.dom.current) {
            return true;
        }

        // Ignore clicks inside active formula
        if (api.is_editing() && HN.Util.is_inside(e.target, domeditor)) {
            if (api.in_formula_bar) {
                $("#formulawrapper").removeClass("active");
                api.in_formula_bar = false;
            }
            return true;
        }

        // Let click on links in cells, open internal links
        if (e.target.nodeName === "A") {
            if (e.ctrlKey || e.metaKey) {
                document.location.href = e.target.getAttribute("href");
            } else {
                href = e.target.getAttribute("href");
                if (href[0] === "/") {
                    window.location.hash = hn.hashUrl.setParam("path", href);
                }
            }
        }
        style  = layout.inner.style;
        auto   = {};
        bounds = {};
        offy   = layout.gridMargin.y() + layout.gridPos.top();
        offx   = layout.gridMargin.x() + layout.gridPos.left();

        api.has_scrolled = true;

        y = api.getNewOffset(e.clientY - offy, Y);
        x = api.getNewOffset(e.clientX - offx, X);

        bounds = {
            "y1": y.index,
            "x1": x.index,
            "y2": y.index,
            "x2": x.index
        };

        e.preventDefault();

        // Stop double clicks being screwed with
        if (api.last_click &&
            new Date().getTime() - 250 < api.last_click) {
            return true;
        } else {
            api.last_click = new Date().getTime();
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
        select_start();

        mousemove = function (e) {

            var args, y2, x2;
            e.preventDefault();

            if (!auto.timer) {
                args = api.auto_scroll_coords(e);
                if (args) {
                    auto = {
                        "args":  args,
                        "mouse": {"y": e.clientY, "x": e.clientX},
                        "timer": setInterval(auto_scroll, 30)
                    };
                } else {

                    y2 = api.getNewOffset(e.clientY - offy, Y);
                    x2 = api.getNewOffset(e.clientX - offx, X);

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
            return false;

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

        var current  = api.dom.current.style,
            cellAttr = sheet.cell(api.cell.y, api.cell.x),
            cellPos  = sheet.cell_offset(api.cell.y, api.cell.x),
            formula  = (cellAttr.formula) || "",
            width    = sheet.cellWidth(cellAttr, api.cell.x),
            height   = sheet.cellHeight(cellAttr, api.cell.y);

        api.editor.setFormula(formula);

        current.top       = (cellPos.top - 1) + "px";
        current.left      = (cellPos.left - 1) + "px";
        current.height    = height + "px";
        current.width     = width + "px";
    };

    api.show_selection = function () {
        if ($.browser.webkit && ! api.is_editing()) {
            pasteDom.value = "";
            pasteDom.focus();
        }

        var bounds = api.order_bounds(api.bounds),
            coords = api.highlightRange(api.wrapper, bounds),
            col    = api.dom.colselection,
            row    = api.dom.rowselection;

        //api.dom.name.innerHTML = HN.Util.range_to_str(bounds);

        row.style.top    = (coords.top) + "px";
        row.style.height = (coords.height) + "px";

        col.style.left   = (coords.left) + "px";
        col.style.width  = (coords.width) + "px";
    };

    api.highlightRange = function (highlight, range) {
        var style  = highlight.style,
            coords = sheet.cell_offset(range.y1, range.x1),
            width  = 0, height,
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
        // you need to have special case for no selection
        // it is a copied range of 0, 0, 0, 0
        if (range.x1 === 0 && range.x2 === 0 && range.y1 === 0 && range.y2 === 0)
        {
            style.display = "none";
        } else {
            style.display = "block";
        }
        return {
            "width"  : width,
            "height" : height,
            "top"    : coords.top,
            "left"   : coords.left
        };
    };

    api.build_clipboard_str = function () {

        var str = "", text, formula,
        tocopy = "formula", y, x;

        api.bounds = api.order_bounds(api.bounds);
        for (y = api.bounds.y1; y <= api.bounds.y2; y += 1) {
            for (x = api.bounds.x1; x <= api.bounds.x2; x += 1) {
                formula = sheet.cell(y, x)[tocopy] || "";
                if (typeof formula === "number") {
                    text = formula.toString();
                } else if (typeof formula === "string") {
                    text = formula;
                }
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
        return true;
    };

    api.menuAction_insertwiki = function (e) {
        var path, cell, range, input, status = "";
        path  = layout.currentSheet().path();
        cell  = layout.tabUI().currentSelectedCell();
        range = layout.tabUI().currentSelectedBounds();
        input = layout.currentSheet().cell(cell.y, cell.x).input;
        status = "";
        if (input === "inline") {
            status = "none";
        } else {
            status = "inline";
        }
        HN.Callbacks.set_input(path, range, status);
    };
    api.menuAction_insertwikirichtext = function (e) {
        var path, cell, range, box, cellrange, input, status = "";
        path  = layout.currentSheet().path();
        cell  = layout.tabUI().currentSelectedCell();
        range = layout.tabUI().currentSelectedBounds();
        box = {"y1" : cell.y,
               "x1" : cell.x,
               "y2" : cell.y + 10,
               "x2" : cell.x + 6};
        cellrange = {"y1" : cell.y,
                     "x1" : cell.x,
                     "y2" : cell.y,
                     "x2" : cell.x};
        input = layout.currentSheet().cell(cell.y, cell.x).input;
        status = "";
        if (input === "inlinerich") {
            status = "none";
        } else {
            status = "inlinerich";
        }
        HN.Callbacks.set_input(path, cellrange, status);
        // if the status is 'inlinerich' then merge up the cells
        if (status === "inlinerich") {
            HN.Callbacks.mergeCells(layout.currentSheet().path(),box);
        }
    };
    api.menuAction_insertwikidropdown = function (e) {
        var dropdownref = layout.tabUI().currentInlineDropdown();
        $("#hn_insertwikiselect").val(dropdownref);
        HN.UI.open_dialog(layout, "hn_makewikiselect");
    };
    api.menuAction_viewlogs = function (e) {
        var Ref = HN.Util.coord_to_ref({x : api.bounds.x1, y : api.bounds.y1});
        window.open(sheet.path() + Ref + "?view=logs", "View Logs");
        return true;
    };
    api.menuAction_deleterows = function (e) {
        HN.Callbacks.deleteRowCol(sheet.path(), api.bounds, Y);
        return true;
    };
    api.menuAction_deletecols = function (e) {
        HN.Callbacks.deleteRowCol(sheet.path(), api.bounds, X);
        return true;
    };
    api.menuAction_defaultwidthcols = function (e) {
        HN.Callbacks.setWidth(sheet.path(), api.bounds.x1, api.bounds.x2,
                        HN.Sheet.CELL_WIDTH);
    };
    api.menuAction_defaultheightrows = function (e) {
        HN.Util.postRow(sheet.path(), api.bounds.y1, api.bounds.y2,
                        {"set": {"fixedHeight": true}});
        HN.Callbacks.setHeight(sheet.path(), api.bounds.y1, api.bounds.y2,
                               HN.Sheet.CELL_HEIGHT);
    };
    api.menuAction_deleteleft = function (e) {
        HN.Callbacks.deleteRange(sheet.path(), api.bounds, "horizontal");
        return true;
    };
    api.menuAction_deleteup = function (e) {
        HN.Callbacks.deleteRange(sheet.path(), api.bounds, "vertical");
        return true;
    };

    api.menuAction_insertright = function (e) {
        HN.Callbacks.insertRange(sheet.path(), api.bounds, "horizontal");
        return true;
    };

    api.menuAction_insertdown = function (e) {
        HN.Callbacks.insertRange(sheet.path(), api.bounds, "vertical");
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

    api.menuAction_pastestyle = function (e) {
        api.pasteStyle(JSON.parse(unescape(localStorage.copied)));
        return true;
    };

    api.menuAction_pastevalue = function (e) {
        //var clipboard = JSON.parse(unescape(localStorage.copied));
        //api.pasteValues(clipboard.values);
        api.pasteValue(JSON.parse(unescape(localStorage.copied)));
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

    api.menuAction_createform = function (e) {

        var x, y, obj, inp,
            button = "=form.button(\"Submit\")",
            label  = "Question",
            replies = "<a href=\"./_replies/\">replies</a>",
            path   = sheet.path(),
            bnd    = api.order_bounds(api.bounds),
            width  = bnd.x2 - bnd.x1,
            height = bnd.y2 - bnd.y1;

        if (width === 0 && height === 0) {
            HN.Callbacks.set_cell(path, {"x": bnd.x1, "y": bnd.y1}, button);
        } else if (width > 0) {
            for (y = 0; y < height; y = y + 1) {
                obj = {"x": bnd.x1, "y": bnd.y1 + y};
                inp = "=form.input(" + HN.Util.coord_to_ref(obj) + ")";
                HN.Callbacks.set_cell(path, {"x": bnd.x1 + 1, "y": bnd.y1 + y}, inp);
                HN.Callbacks.set_cell(path, obj, label + " " + (y + 1) + ":");
            }
            HN.Callbacks.set_cell(path, {"x": bnd.x1, "y": bnd.y2}, replies);
            HN.Callbacks.set_cell(path, {"x": bnd.x1 + 1, "y": bnd.y2}, button);
        } else {
            for (y = 0; y < height - 2; y += 2) {
                obj = {"x": bnd.x1, "y": bnd.y1 + y};
                inp = "=input(" + HN.Util.coord_to_ref(obj) + ")";
                HN.Callbacks.set_cell(path, {"x": bnd.x1, "y": bnd.y1 + y + 1}, inp);
                HN.Callbacks.set_cell(path, obj, label + " " + (1 + y / 2) + ":");
            }
            HN.Callbacks.set_cell(path, {"x": bnd.x1, "y": bnd.y2 - 1}, button);
            HN.Callbacks.set_cell(path, {"x": bnd.x1, "y": bnd.y2}, replies);
        }
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

HN.Layout.Editor = function (layout, selection, sheet) {

    var api    = {}, saved,
        input  = HN.Util.id("input"),
        editor = document.getElementById("editor"),
        hidden = HN.Util.id("hidden_input");

    // not sure if the selection thingie is cross-brower compatible
    function setRange(startNode, startOffset, endNode, endOffset) {
        var selection, range;
        selection = (window.getSelection) ?
            window.getSelection() : document.getSelection();

        // FF returns a selected range, Safari and Chrome don't so fix up...
        if (selection.rangeCount === 0) {
            selection.addRange(document.createRange());
        }

        range = selection.getRangeAt(0);
        range.setStart(startNode, startOffset);
        range.setEnd(endNode, endOffset);

        if ("empty" in selection) {
            selection.empty();
        }
        if ("addRange" in selection) {
            selection.addRange(range);
        }
    }

    function setSelectionToEnd() {
        var tmp = document.createTextNode("");
        editor.appendChild(tmp);
        setRange(tmp, 0, tmp, 0);
    }

    function entitiesToHTML(str) {
        return str.replace(/&amp;/g, "&")
            .replace(/&gt;/g, ">")
            .replace(/&lt;/g, "<");
    }
    api.value = function () {
        return editor.innerHTML;//.replace(/<br>$/, "");
    };

    api.set = function (value) {
        editor.innerHTML = value || "";
    };

    api.calculateWidth = function () {

        var index = 0,
            val   = 0,
            tmp   = 0,
            fn    = "col_width", xy = X, cell, t,
            sel_height = sheet.row_height(selection.cell.y);

        hidden.innerHTML = api.value() || "&nbsp;";
        if (sel_height > hidden.clientHeight) {
            input.style.height = sel_height;
        } else {
            input.style.height = hidden.clientHeight + "px";
        }
        val = hidden.clientWidth + 5;

        do {
            tmp += sheet[fn](selection.cell[xy.str] + index);
            input.style[xy.dimension] = tmp + "px";
            index += 1;
        } while ((val + 10) > tmp);
    };

    api.start_edit = function (val, focus) {

        var s         = input.style,
            cell      = selection.cell,
            cell_attr = sheet.cell(cell.y, cell.x),
            cell_pos  = sheet.cell_offset(cell.y, cell.x),
            style     = "background:#FFF;" +
                sheet.lookupCSS(cell.y, cell.x) +
                "text-align:left; display:block;outline:none;resize:none;";

        selection.cell_dom.innerHTML = "";
        hidden.setAttribute("style", style);

        s.width  = "0px";
        s.height = (sheet.row_height(cell.y) - 2) + "px";
        s.top    = (cell_pos.top - 1)  + "px";
        s.left   = (cell_pos.left - 1) + "px";

        editor.setAttribute("style", style);
        editor.innerHTML = val || "";
        api.calculateWidth();
        s.display = "block";

        editor.setAttribute("contentEditable", "true");

        if (focus) {
            editor.focus();
            setSelectionToEnd();
        } else {
            api.read_formula_bar();
        }
    };

    api.quitEdit = function () {

        selection.resumeSelection();

        var c = sheet.cell(selection.cell.y, selection.cell.x),
            preview;

        if (selection.in_formula_bar) {
            $("#formulawrapper").removeClass("active");
            selection.in_formula_bar = false;
        }
        // clear dragfill
        HN.Util.id("dragfill").style.display = "none";
        selection.formula.blur();
        api.setFormula(c.formula || "");
        editor.blur();
        editor.removeAttribute("contentEditable");

        if (c && c.value) {
            preview = HN.Util.getPreview(c);
        } else {
            preview = "";
        }
        selection.cell_dom.innerHTML = preview;

        hidden.innerHTML = "";
        editor.innerHTML = "";
        input.style.display = "none";
    };

    api.end_edit = function () {

        if (selection.in_formula_bar) {
            $("#formulawrapper").removeClass("active");
            selection.in_formula_bar = false;
        }

        selection.formula.blur();
        editor.blur();
        editor.removeAttribute("contentEditable");

        HN.Util.id("dragfill").style.display = "none";
        selection.start_drag = false;

        var tmpDiv,
            c         = sheet.cell(selection.cell.y, selection.cell.x),
            formula   = (c && c.formula) || "",
            val       = api.value().replace(/<br>$/, ""),
            isFormula = val.indexOf("=") === 0,
            isHTML    = val.indexOf("&lt;") === 0;

        if (isHTML) {
            val = entitiesToHTML(val);
        }

        if (isFormula) {
            tmpDiv = document.createElement("div");
            tmpDiv.innerHTML = val;
            val = tmpDiv.textContent; // I think this sanitizes it (GG)
        }
        if (selection.is_light_edit()) {
            selection.cell_dom.innerHTML = val;
            if (c) {
                c.value = val;
            }

            HN.Callbacks.set_cell(sheet.path(), selection.cell, val);
            if (!isFormula) {
                sheet.cellChanged(selection.cell.y, selection.cell.x);
                layout.dataUpdated();
            }

        } else if (selection.is_full_edit()) {

            if (val !== formula) {

                selection.cell_dom.innerHTML = val;

                if (c) {
                    c.value = val;
                }
                HN.Callbacks.set_cell(sheet.path(), selection.cell, val);
                if (!isFormula) {
                    sheet.cellChanged(selection.cell.y, selection.cell.x);
                    layout.dataUpdated();
                }

            } else {

                selection.cell_dom.innerHTML = val;
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
        var f       = api.value(),
            index   = Math.max(f.lastIndexOf("="), f.lastIndexOf(","),
                               f.lastIndexOf("("), f.lastIndexOf("+"),
                               f.lastIndexOf("-"),
                               f.lastIndexOf("*"), f.lastIndexOf("/")),
            formula = f.substring(0, index + 1) +
              HN.Util.range_to_str2(range);

        api.set(formula);

        selection.formula.innerHTML = formula;
        setSelectionToEnd();
    };

    api.read_formula_bar = function () {
        setTimeout(function () {
            api.set(selection.formula.innerHTML);
            api.calculateWidth();
        }, 0);
    };

    api.setFormula = function (value) {
        selection.formula.innerHTML = value;
    };

    // TODO: Stub functions, need to be able to save/restore selections
    // for when dialogs take focus from the current edit
    api.saveSelection = function () {
        var tmp = window.getSelection();

        saved = {
            "text"        : tmp.toString(),
            "startNode"   : tmp.anchorNode,
            "startOffset" : tmp.anchorOffset,
            "endNode"     : tmp.focusNode,
            "endOffset"   : tmp.focusOffset
        };
    };

    api.selectionText = function () {
        return saved.text;
    };

    api.deleteSelection = function () {
    };

    api.restoreSelection = function () {
        editor.focus();
        setRange(saved.startNode, saved.startOffset,
                 saved.endNode, saved.endOffset);
    };

    api.execCommand = function (cmd, ui, value) {
        document.execCommand(cmd, ui, value);
        api.calculateWidth();
    };

    return api;
};
