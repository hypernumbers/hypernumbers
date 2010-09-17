/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: false */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false , toolbar: false */
HN.namespace("Layout");

/**
 * @class HN.Layout
 * Handles the main layout controls, the grid / axis / 
 * formula bar and toolbar
 */

HN.Layout = function(data) {

    var api              = {},
        tabs             = {},
        currentTab       = null,
        pageHasFocus     = false,
        gridHasFocus     = true,
        pasteDom         = HN.Util.id("paste"),
        grid             = HN.Util.id("spreadsheet"),
        inner            = HN.Util.id("scroller"),
        scrollbar        = { "x" : HN.Util.id("bottom-bar"), 
                             "y" : HN.Util.id("right-bar") },
        gutter           = { "x" : HN.Util.id("bottom-gutter"), 
                             "y" : HN.Util.id("right-gutter") },
        axis_dom         = { "x" : HN.Util.id("columns"), 
                             "y" : HN.Util.id("rows") },
        gridSizePix      = { "x":0, "y":0 },
        scrollmax        = { "x":0, "y":0 },
        scrollBarPressed = {},
        ygrid            = HN.Util.y_pos(inner),
        xgrid            = HN.Util.x_pos(inner);
    
    api.axis_dom  = axis_dom;
    api.inner     = inner;
    
    api.gridMargin  = {
        "y" : function() { return ygrid; },
        "x" : function() { return xgrid; }
    };
    
    // Silly bug, need to be set expicitly before can read
    inner.style.top        = "0px";
    inner.style.left       = "0px";
    scrollbar.y.style.top  = "0px";
    scrollbar.x.style.left = "0px";
    
    api.setGridPos = {};
    api.setGridPos.top = function(top) { 
        inner.style.top      = top + "px";
        axis_dom.y.style.top = top + "px";
    };
    api.setGridPos.left = function(left) { 
        inner.style.left      = left + "px";
        axis_dom.x.style.left = left+ "px";
    };
    
    api.gridPos      = {};
    api.gridPos.top  = function() { 
        return parseInt(inner.style.top, 10); };
    api.gridPos.left = function() { 
        return parseInt(inner.style.left, 10); };

    api.dataUpdated = function() {
        currentTab.selection.panes.dataUpdated();
        currentTab.selection.show_selection();
        currentTab.selection.show_copied();
        currentTab.selection.show_cell();
    };

    api.gridSizePix  = function() { return gridSizePix; };
    api.currentSheet = function() { return currentTab.sheet; };
    api.tabUI        = function() { return currentTab.selection; };


    api.grabFocus = function() {
        gridHasFocus = false;
    };

    api.gridHasFocus = function() {
        return gridHasFocus;
    };

    api.resumeSelection = function() {
        
        gridHasFocus = true;

        if (!currentTab) {
            return;
        }
        
        if( currentTab.selection.is_editing() ) {
            currentTab.selection.resume_editing();
        } else {
            currentTab.selection.resumeSelection();
        }
    };

    api.switchTab = function(sheet) {

        var tab = tabs[sheet.path()],
        perms;

        if( !tab ) {
            tab = {
                "sheet"     : sheet,
                "selection" : new HN.Layout.Selection(api, sheet)
            };
            tabs[sheet.path()] = tab;
            currentTab = tab;
        } else {
            currentTab = tab;
        }

        currentTab.selection.panes.moveTo(sheet.offset.y, 
                                          sheet.offset.x);

        api.calcScrollbarSize(Y);
        api.calcScrollbarSize(X);       

        currentTab.selection.show_selection();
        currentTab.selection.show_copied();
        currentTab.selection.show_cell();
        
        // reset the views menu
        toolbar.loadViews();

        perms = sheet.key("permissions");
    };
    
    /**
     * Notify the grid user has scrolled, move bounds, check that grid
     * can be displayed within current bounds and pane tiles when
     * needed
     */
    function checkPaneBounds(dist, xy, upleft) {

        var axis      = xy.str,
            start     = axis + "1",
            end       = axis + "2",
            panes     = currentTab.selection.panes,
            firstPane = panes.firstPane(),
            n, 
            x;
        
        panes.bounds[start] += dist;
        panes.bounds[end]   += dist;
        
        // Todo could do with cleverer logic on how to shift cells when
        // scrolling large increments, wasting work here
        
        if( Math.abs(dist) > firstPane[xy.dimension] ) {
            n = Math.floor(Math.abs(dist) / firstPane[xy.dimension]);
            for( x = 0; x < n; x++ ) {
                panes["shift_"+axis](upleft);
            }
        }

        if( upleft && gridSizePix[xy.str] > panes.bounds[end] ) {
            panes["shift_"+axis](upleft);
        } else if( !upleft && panes.bounds[start] > -40 ) {
            panes["shift_"+axis](upleft);
        }
    }

    function doScroll(offset, dist, xy, canExpand) {

        var pane_pos = api.gridPos[xy.coord]() + dist,
        forward  = offset >= 0,
        spos;

        api.setGridPos[xy.coord](pane_pos);
        axis_dom[xy.str].style[xy.coord] = pane_pos + "px";

        currentTab.sheet.offset[xy.str] += offset;

        spos = api.calcScrollbarPos(xy);
        
        if ( (spos >= scrollmax[xy.str]) && forward && canExpand ) {
            currentTab.sheet.extend(xy, 2);
            api.calcScrollbarSize(xy);
            spos = api.calcScrollbarPos(xy);
        }
        
        if( xy === Y ) {
            HN.Util.id("rsel").style.top = pane_pos+"px";
        } else {
            HN.Util.id("csel").style.left = pane_pos+"px";
        }

        // Wrap panes if needed
        checkPaneBounds(dist, xy, forward);

        return spos;
    }

    function doScrollbarPos(pos, xy)  {
        if ( pos > scrollmax[xy.str] ) {
            pos = scrollmax[xy.str];
        } else if (pos < 0) {
            pos = 0;
        }
        scrollbar[xy.str].style[xy.coord] = pos + "px";
        return pos;
    }

    api.calcScrollbarPos = function(xy) {

        var dim = scrollbar[xy.str].style[xy.dimension],
            barS    = (dim === "") ? 0 : parseInt(dim,10),
            gutterS = gridSizePix[xy.str] - barS,
            panePos = -api.gridPos[xy.coord](),
            pos     = parseInt(gutterS * panePos /
                               currentTab.sheet[xy.dimension](), 10);
        return doScrollbarPos(pos, xy);
    };
    
    /**
     * Calculate Scrollbar size 
     */
    api.calcScrollbarSize = function(xy) {        
        // size[] is viewable area, s[] is scrollable area
        var gutterSize = gutter[xy.str][xy.client],
            barSize    = (gridSizePix[xy.str] / 
                          currentTab.sheet[xy.dimension]()) * gutterSize;
        barSize = parseInt(barSize, 10);
        
        if( barSize < 10 ) {
            barSize = 15;
        }

        scrollbar[xy.str].style[xy.dimension] = barSize + "px";
        scrollmax[xy.str]                     = gutterSize - barSize - 2;
    };

    api.gridResize = function() {

        var bodyWidth = $("body").width(),
            gridWidth = bodyWidth - HN.Util.x_pos(grid);
        
        grid.style.height = $("body").height() - HN.Util.y_pos(grid) + "px";
        grid.style.width  = gridWidth + "px";
        
        gridSizePix.y = parseInt($(inner).height(), 10);
        gridSizePix.x = parseInt($(inner).width(), 10);

        if( currentTab ) {
            currentTab.selection.panes.gridResized();
            api.calcScrollbarSize(Y);
            api.calcScrollbarSize(X);               
            api.calcScrollbarPos(Y);
            api.calcScrollbarPos(X);
        }

        $("#ctrlpanel").css("top", parseInt($("body").height() / 2, 10)+"px");
    };

    api.scrollBy1 = function(forward, xy) {

        var sheet = currentTab.sheet,
            f;

        if( !forward && sheet.offset[xy.str] === 1 ) {
            return;
        }
        
        f = ( xy === Y ) ? "row_height" : "col_width";
        if( forward ) {
            doScroll(1, -sheet[f](sheet.offset[xy.str]), xy, true);
        } else {
            doScroll(-1, sheet[f](sheet.offset[xy.str]-1), xy, true);
        }
    };
    
    /** 
     * Calculate cell offset, and snapped-distance required to move along 
     * a given vector until a target distance is reached.
     * 
     */
    function snapToDistance(forward, distance, xy) {

        var step,
            i     = currentTab.sheet.offset[xy.str],
            total = 0,
            f     = ( xy === Y ) ? "row_height" : "col_width";

        if( forward ) {
            step = 1;
        } else {
            step = -1; 
            i -= 1;
        }
        
        while( true ) 
        {
            total += currentTab.sheet[f](i);

            if( total > distance) {
                total -= currentTab.sheet[f](i);
                return { 
                    "offset" : i - currentTab.sheet.offset[xy.str] +
                        (forward ? 0 : 1), 
                    "dist"   : -step * total
                };
            } 

            i += step;

            if (i < 1) {
                return { 
                    "offset" : 1 - currentTab.sheet.offset[xy.str], 
                    "dist"   : -api.gridPos[xy.coord]()
                };
            }
        }
    }

    /**
     * Drag box pressed
     */

    scrollBarPressed.bar = function(clk_event, xy) {

        var gutter_size = gutter[xy.str][xy.client],
            attr        = (xy === Y) ? "clientY" : "clientX",
            offset      = (xy === Y) ?
            HN.Util.y_pos(gutter[Y.str]) - HN.Util.y_pos(scrollbar[Y.str]) :
            HN.Util.x_pos(gutter[X.str]) - HN.Util.x_pos(scrollbar[X.str]);
        
        offset += (clk_event[attr]);
        
        function scroll(mve_event) {
           
            var nscrollpos, percentageScrolled, 
            ngridpos, delta, res;

            nscrollpos = mve_event[attr] - offset;
            nscrollpos = (nscrollpos < 0) ? 0 : nscrollpos;
            nscrollpos = (nscrollpos > scrollmax[xy.str]) ?
                scrollmax[xy.str] : nscrollpos;

            scrollbar[xy.str].style[xy.coord] = nscrollpos + "px";
            
            percentageScrolled = 
                parseInt((nscrollpos / scrollmax[xy.str]) * 100, 10);
            ngridpos = ((currentTab.sheet[xy.dimension]() -
                         gridSizePix[xy.str]) / 100) *
                percentageScrolled;
            delta = ngridpos + api.gridPos[xy.coord]();
            res   = snapToDistance(delta > 0, Math.abs(delta), xy);
            
            if( res.dist ) {
                doScroll(res.offset, res.dist, xy, false);  
            }
        }

        function onMouseUp() {
            scrollbar[xy.str].className = "";
            HN.Util.removeEvent(document, "mouseup", onMouseUp);
            HN.Util.removeEvent(document, "mousemove", scroll);
        }
        
        HN.Util.addEvent(document, "mouseup", onMouseUp);
        HN.Util.addEvent(document, "mousemove", scroll);

        scrollbar[xy.str].className = "active";
    };

    /**
     * Scroll Button pressed
     */
    scrollBarPressed.button = function(event, xy) {

        var timer    = null,
            interval = 300,
            forward  =
            event.target.getAttribute("data-direction") === "forward";

        function halt() {
            event.target.className = "";
            HN.Util.removeEvent(document, "mouseup", halt);
            HN.Util.removeEvent(event.target, "mouseout", halt);
            clearTimeout(timer);
        }

        event.target.className = "active";
        HN.Util.addEvent(document, "mouseup", halt);
        HN.Util.addEvent(event.target, "mouseout", halt);

        // Continue scrolling while mouse is pressed
        function repeat() {
            api.scrollBy1(forward, xy);
            timer = setTimeout(repeat, interval);
            interval = Math.max(30, Math.floor(0.7 * interval));
        }
        repeat();
    };


    /** 
     * Gutter Pressed
     */
    scrollBarPressed.gutter = function(event, xy) {

        var timer    = null,
            interval = 200,
            gut      = gutter[xy.str],
            p        = (xy === Y) ?
              event.pageY - HN.Util.y_pos(gut) : 
              event.pageX - HN.Util.x_pos(gut),
            forward = p > parseInt(scrollbar[xy.str].style[xy.coord], 10);
        
        function halt() {
            HN.Util.removeEvent(document, "mouseup", halt);
            HN.Util.removeEvent(gut, "mouseout", halt);
            clearTimeout(timer);
        }

        HN.Util.addEvent(document, "mouseup", halt);
        HN.Util.addEvent(gut, "mouseout", halt);

        function gutterScroll() {
            var style     = scrollbar[xy.str].style,
                scrollPos = parseInt(style[xy.coord],10),
                barSize   = parseInt(scrollbar[xy.str].style[xy.dimension],10),
                res;
            if( p > (scrollPos + barSize) !== forward ) {
                halt();
                return;
            }
            res = snapToDistance(forward, gridSizePix[xy.str], xy);
            doScroll(res.offset, res.dist, xy, true);
        }

        // Continue scrolling while mouse is pressed
        function repeat() {
            gutterScroll();
            timer    = setTimeout(repeat, interval);
            interval = Math.max(15, Math.floor(0.7 * interval));
        }
        repeat();
    };

    function wheelScrolled(event, delta) {

        var N       = Math.min(Math.ceil(Math.abs(delta)),50),
            forward = delta < 0,
            axis    = event.shiftKey ? X : Y,
            i;

        for (i = N; i-->0; ) {
            api.scrollBy1(forward, axis);
        }
    }

    function initEvents() {
        
        // Duplicate paste commands to fix for safari
        if ( $.browser.safari ) {
            $("#paste")[0].addEventListener("beforecopy", function(e) { 
                pasteDom.value = currentTab.selection.copy();
                pasteDom.select();
            });
        }

        HN.Util.addEvent(inner, "dblclick", function(e) {
            currentTab.selection.dblclick(e);
        });
        
        var columns        = $("#columns")[0],
            rows           = $("#rows")[0],
            formulawrapper = $("#formulawrapper")[0],
            mousedown,
            add_axis_events;
        
        mousedown = function(e) {

            if (!pageHasFocus) {
                $(window).focus();
            }
            
            if( e.target.getAttribute("id") === "dragger" ) {
                currentTab.selection.dragdown(e);
            } else {
                if( HN.Util.is_inside(e.target, columns) || 
                    $(e.target).attr("id") === "colselection") {
                    currentTab.selection.mouseDownAxis(e, X);
                } else if( HN.Util.is_inside(e.target, rows) || 
                           $(e.target).attr("id") === "rowselection") {
                    currentTab.selection.mouseDownAxis(e, Y);
                } else if( HN.Util.is_inside(e.target, formulawrapper)) {
                    currentTab.selection.formulaFocus();
                } else if( $(e.target).attr("id") === "corner") {
                    currentTab.selection.selectFullSheet();
                } else {
                    currentTab.selection.mousedown(e);
                }
            } 
        };

        // Should probably make this some fancy dispatcher
        HN.Util.addEvent(document, "mousedown", mousedown);

        // TODO : combine into single document context menu
        HN.Util.addEvent(inner, "contextmenu", function(e) {
            currentTab.selection.gridContextMenu(e);
        });

        add_axis_events = function(id, axis) {
            HN.Util.addEvent(id, "contextmenu", function(e) {
                currentTab.selection.axisContextMenu(e, axis);
            });
        };

        add_axis_events(HN.Util.id("columnsout"), X);
        add_axis_events(HN.Util.id("rowsout"), Y);

        HN.Util.addEvent(window, "keydown", function(e) {
            currentTab.selection.keydown(e);
        });

        $("#scroller").bind('mousewheel', function(e, delta) {
            e.preventDefault();
            wheelScrolled(e, delta);
            return false;
        });

        $(window).bind("blur", function() {
            pageHasFocus = false;
        });
        $(window).bind("focus", function() { 
            pageHasFocus = true;
            api.resumeSelection();
        });
        $(window).focus();

        $(window).bind("resize", function() { 
            setTimeout(api.gridResize, 10);
        });
        api.gridResize();

        function addScrollBarEvents(id, xy) {
            HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {  
                e.preventDefault();
                e.stopPropagation();
                var type = e.target.getAttribute("data-type");
                scrollBarPressed[type](e, xy);
                return false;
            });
        }

        addScrollBarEvents("scrollbar-bottom-inner", X);
        addScrollBarEvents("scrollbar-right-inner", Y);

    }
 
    initEvents();
    
    return api;
};