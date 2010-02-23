HN.namespace("Layout");

/**
 * @class HN.Layout
 * Handles the main layout controls, the grid / axis / formula bar and toolbar
 */

HN.Layout = function(data) 
{
    var public = {};

    var tabs       = {};
    var currentTab = null;
    
    // I hate this
    public.states = { 
        "FULL_SPREADSHEET" : { "width" : 1 },
        "MOST_SPREADSHEET" : { "width" : 0.7 },
        "MOST_VIEWBUILDER" : { "width" : 0.4 }
    };

    public.state = public.states.FULL_SPREADSHEET;

    var grid      = HN.Util.id("spreadsheet");
    var inner     = HN.Util.id("scroller");

    var scrollbar = { "x" : HN.Util.id("bottom-bar"), 
                      "y" : HN.Util.id("right-bar") };
    var gutter    = { "x" : HN.Util.id("bottom-gutter"), 
                      "y" : HN.Util.id("right-gutter") };
    var axis_dom  = { "x" : HN.Util.id("columns"), 
                      "y" : HN.Util.id("rows") };

    var gridSizePix = { "x":0, "y":0 };
    var scrollmax   = { "x":0, "y":0 };

    var scrollBarPressed = {};
    
    public.axis_dom  = axis_dom;
    public.inner     = inner;

    var ygrid = HN.Util.y_pos(inner);
    var xgrid = HN.Util.x_pos(inner);
    
    public.gridMargin  = {
        "y" : function() { return ygrid; },
        "x" : function() { return xgrid; },
    };
        
    // Silly bug, need to be set expicitly before can read
    inner.style.top        = "0px";
    inner.style.left       = "0px";
    scrollbar.y.style.top  = "0px";
    scrollbar.x.style.left = "0px";

    
    public.setGridPos = {};
    public.setGridPos.top = function(top) { 
        inner.style.top      = top + "px";
        axis_dom.y.style.top = top + "px";
    };
    public.setGridPos.left = function(left) { 
        inner.style.left      = left + "px";
        axis_dom.x.style.left = left+ "px";
    };
    
    public.gridPos      = {};
    public.gridPos.top  = function() { return parseInt(inner.style.top, 10); };
    public.gridPos.left = function() { return parseInt(inner.style.left, 10); };

    public.dataUpdated = function() {
        currentTab.selection.panes.dataUpdated();
        currentTab.selection.show_selection();
        currentTab.selection.show_cell();
    };

    public.gridSizePix  = function() { return gridSizePix; };
    public.currentSheet = function() { return currentTab.sheet; };
    public.tabUI        = function() { return currentTab.selection; };


    public.grabFocus = function() {
        currentTab.selection.state = HN.States.NOT_EDITING;
    };
    
    public.resumeSelection = function() {
        if( currentTab.selection.is_editing() ) {
            currentTab.selection.resumeEditing();
        } else {
            currentTab.selection.resumeSelection();
        }
    };

    public.switchTab = function(sheet) {

        var tab = tabs[sheet.path()];

        if( !tab ) {
            tab = {
                "sheet"     : sheet,
                "selection" : new HN.Layout.Selection(public, sheet)
            };
            tabs[sheet.path()] = tab;
            currentTab = tab;
        } else {
            currentTab = tab;
        }

        currentTab.selection.panes.moveTo(sheet.offset.y, sheet.offset.x);

        public.calcScrollbarSize(Y);
        public.calcScrollbarSize(X);       

        currentTab.selection.show_selection();
        currentTab.selection.show_cell();
        
        public.resumeSelection();
    };
    
    public.calcScrollbarPos = function(xy) {

        var barS    = parseInt(scrollbar[xy.str].style[xy.dimension],10);
        var gutterS = gridSizePix[xy.str] - barS;
        var panePos = -public.gridPos[xy.coord]();
        var pos      = parseInt(gutterS * panePos / 
                                currentTab.sheet[xy.dimension]());
        return doScrollbarPos(pos, xy);
    };
    
    /**
     * Calculate Scrollbar size 
     */
    public.calcScrollbarSize = function(xy)
    {        
        // size[] is viewable area, s[] is scrollable area
        var gutterSize = gutter[xy.str][xy.client];
        var barSize    = (gridSizePix[xy.str] / 
                          currentTab.sheet[xy.dimension]()) * gutterSize;
        barSize        = parseInt(barSize, 10);
        
        if( barSize < 10 ) {
            barSize = 15;
        }

        scrollbar[xy.str].style[xy.dimension] = barSize + "px";
        scrollmax[xy.str]                     = gutterSize - barSize - 2;
    };

    public.gridResize = function() {

        var bodyWidth = $("body").width();
        var gridWidth = (bodyWidth - HN.Util.x_pos(grid)) * 
            public.state.width;
        
        grid.style.height = $("body").height() - HN.Util.y_pos(grid) + "px";
        grid.style.width  = gridWidth + "px";
        
        $("#viewbuilder").css({
            "left"  : gridWidth, 
            "width" : bodyWidth-gridWidth-2
        });

        gridSizePix.y = parseInt($(inner).height());
        gridSizePix.x = parseInt($(inner).width());

        if( currentTab ) {
            currentTab.selection.panes.gridResized();
            public.calcScrollbarSize(Y);
            public.calcScrollbarSize(X);               
            public.calcScrollbarPos(Y);
            public.calcScrollbarPos(X);
        }
    };

    public.scrollBy1 = function(forward, xy) {

        var sheet = currentTab.sheet;

        if( !forward && sheet.offset[xy.str] == 1 ) {
            return;
        }
        
        var f = ( xy == Y ) ? "row_height" : "col_width";
        if( forward ) {
            doScroll(1, -sheet[f](sheet.offset[xy.str]), xy, true);
        } else {
            doScroll(-1, sheet[f](sheet.offset[xy.str]-1), xy, true);
        }
    };

    function doScrollbarPos(pos, xy)  {
        if( pos > scrollmax[xy.str] ) {
            pos = scrollmax[xy.str];
        } else if (pos < 0) {
            pos = 0;
        }
        scrollbar[xy.str].style[xy.coord] = pos + "px";
        return pos;
    };
    
    /**
     * Drag box pressed
     */

    scrollBarPressed["bar"] = function(clk_event, xy) {

        var gutter_size = gutter[xy.str][xy.client];
        var attr        = (xy == Y) ? "clientY" : "clientX";
        var offset      = (xy == Y) 
            ? HN.Util.y_pos(gutter[Y.str]) - HN.Util.y_pos(scrollbar[Y.str])
            : HN.Util.x_pos(gutter[X.str]) - HN.Util.x_pos(scrollbar[X.str])
        
        offset += (clk_event[attr]);
        
        function scroll(mve_event) {
           
            var nscrollpos = mve_event[attr] - offset;
            nscrollpos = (nscrollpos < 0) ? 0 : nscrollpos;
            nscrollpos = (nscrollpos > scrollmax[xy.str]) 
                ? scrollmax[xy.str] : nscrollpos;

            scrollbar[xy.str].style[xy.coord] = nscrollpos + "px";
            
            var percentageScrolled = 
                parseInt((nscrollpos / scrollmax[xy.str]) * 100);
            var ngridpos = ((currentTab.sheet[xy.dimension]() - 
                             gridSizePix[xy.str]) / 100) 
                * percentageScrolled;
            var delta = ngridpos + public.gridPos[xy.coord]();
            var res   = snapToDistance(delta > 0, Math.abs(delta), xy);
            
            if( res.dist ) {
                doScroll(res.offset, res.dist, xy, false);  
            }
        };

        function onMouseUp() {
            scrollbar[xy.str].className = "";
            HN.Util.removeEvent(document, "mouseup", onMouseUp);
            HN.Util.removeEvent(document, "mousemove", scroll);
        };
        
        HN.Util.addEvent(document, "mouseup", onMouseUp);
        HN.Util.addEvent(document, "mousemove", scroll);

        scrollbar[xy.str].className = "active";
    };

    /**
     * Scroll Button pressed
     */
    scrollBarPressed["button"] = function(event, xy) {

        var timer    = null;
        var interval = 300;
        var forward  = 
            event.target.getAttribute("data-direction") == "forward";

        function halt() {
            event.target.className = "";
            HN.Util.removeEvent(document, "mouseup", halt);
            HN.Util.removeEvent(event.target, "mouseout", halt);
            clearTimeout(timer);
        };

        event.target.className = "active";
        HN.Util.addEvent(document, "mouseup", halt);
        HN.Util.addEvent(event.target, "mouseout", halt);

        // Continue scrolling while mouse is pressed
        function repeat() {
            public.scrollBy1(forward, xy);
            timer = setTimeout(repeat, interval);
            interval = Math.max(30, Math.floor(0.7 * interval));
        };
        repeat();
    };


    /** 
     * Gutter Pressed
     */
    scrollBarPressed["gutter"] = function(event, xy) {

        var timer    = null;
        var interval = 200;
        var gut      = gutter[xy.str];
        var p        = (xy == Y)
            ? event.pageY - HN.Util.y_pos(gut)
            : event.pageX - HN.Util.x_pos(gut);
        var forward = p > parseInt(scrollbar[xy.str].style[xy.coord], 10);
        
        function halt() {
            HN.Util.removeEvent(document, "mouseup", halt);
            HN.Util.removeEvent(gut, "mouseout", halt)
            clearTimeout(timer);
        }

        HN.Util.addEvent(document, "mouseup", halt);
        HN.Util.addEvent(gut, "mouseout", halt);

        function gutterScroll() {
            var style     = scrollbar[xy.str].style;
            var scrollPos = parseInt(style[xy.coord],10);
            var barSize   = parseInt(scrollbar[xy.str].style[xy.dimension],10);
            if( p > (scrollPos + barSize) != forward ) {
                halt();
                return;
            }
            var res = snapToDistance(forward, gridSizePix[xy.str], xy);
            doScroll(res.offset, res.dist, xy, true);
        };

        // Continue scrolling while mouse is pressed
        function repeat() {
            gutterScroll();
            timer    = setTimeout(repeat, interval);
            interval = Math.max(15, Math.floor(0.7 * interval));
        };
        repeat();
    };

    function wheelScrolled(event, delta) {

        var N       = Math.min(Math.ceil(Math.abs(delta)),50);
        var forward = delta < 0;
        var axis    = event.shiftKey ? X : Y;

        for( var i = N; i-->0; ) {
            public.scrollBy1(forward, axis);
        }
    };

    function doScroll(offset, dist, xy, canExpand) {

        var pane_pos = public.gridPos[xy.coord]() + dist;
        var forward  = offset >= 0;

        public.setGridPos[xy.coord](pane_pos);
        axis_dom[xy.str].style[xy.coord] = pane_pos + "px";

        currentTab.sheet.offset[xy.str] += offset;

        var spos = public.calcScrollbarPos(xy);
        
        if ( (spos >= scrollmax[xy.str]) && forward && canExpand ) {
            currentTab.sheet.extend(xy, 2);
            public.calcScrollbarSize(xy);
            spos = public.calcScrollbarPos(xy);
        }
        
        if( xy == Y ) {
            HN.Util.id("rsel").style.top = pane_pos+"px";
        } else {
            HN.Util.id("csel").style.left = pane_pos+"px";
        }

        // Wrap panes if needed
        checkPaneBounds(dist, xy, forward);

        return spos;
    };

    /**
     * Notify the grid user has scrolled, move bounds, check that grid
     * can be displayed within current bounds and pane tiles when
     * needed
     */
    function checkPaneBounds(dist, xy, upleft) {

        var axis      = xy.str;
        var start     = axis+"1";
        var end       = axis+"2";
        var panes     = currentTab.selection.panes;
        var firstPane = panes.firstPane();
        
        panes.bounds[start] += dist;
        panes.bounds[end]   += dist;
        
        // Todo could do with cleverer logic on how to shift cells when
        // scrolling large increments, wasting work here
        
        if( Math.abs(dist) > firstPane[xy.dimension] ) {
            var n = Math.floor(Math.abs(dist) / firstPane[xy.dimension]);
            for( var x = 0; x < n; x++ ) {
                panes["shift_"+axis](upleft);
            }
        }

        if( upleft && gridSizePix[xy.str] > panes.bounds[end] ) {
            panes["shift_"+axis](upleft);
        } else if( !upleft && panes.bounds[start] > -40 ) {
            panes["shift_"+axis](upleft);
        }
    };

    /** 
     * Calculate cell offset, and snapped-distance required to move along 
     * a given vector until a target distance is reached.
     * 
     */
    function snapToDistance(forward, distance, xy) {

        var step;
        var i     = currentTab.sheet.offset[xy.str];
        var total = 0;
        var f     = ( xy == Y ) ? "row_height" : "col_width";

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
                    "offset" : i - currentTab.sheet.offset[xy.str] 
                        + (forward ? 0 : 1), 
                    "dist"   : -step * total, 
                };
            } 

            i += step;

            if (i < 1) {
                return { 
                    "offset" : 1 - currentTab.sheet.offset[xy.str], 
                    "dist"   : -public.gridPos[xy.coord]()
                };
            }
        }
    };


    function initEvents() {

        HN.Util.addEvent(inner, "dblclick", function(e) {
            currentTab.selection.dblclick(e);
        });

        var columns = $("#columns")[0];
        var rows    = $("#rows")[0];

        // Should probably make this some fancy dispatcher
        HN.Util.addEvent(document, "mousedown", function(e) {
            if( e.target.getAttribute("id") == "dragger" ) {
                currentTab.selection.dragdown(e);
            } else {
                if( HN.Util.is_inside(e.target, columns) || 
                    $(e.target).attr("id") == "colselection") {
                    currentTab.selection.mouseDownAxis(e, X);
                } else if( HN.Util.is_inside(e.target, rows) || 
                           $(e.target).attr("id") == "rowselection") {
                    currentTab.selection.mouseDownAxis(e, Y);
                } else if( $(e.target).attr("id") == "formula") {
                    currentTab.selection.in_formula_bar = true;
                } else if( $(e.target).attr("id") == "corner") {
                    currentTab.selection.selectFullSheet();
                } else {
                    currentTab.selection.mousedown(e);
                }
            }
        });
        
        HN.Util.addEvent(inner, "contextmenu", function(e) {
            currentTab.selection.gridContextMenu(e);
        });

        var add_axis_events = function(id, axis) {
            HN.Util.addEvent(id, "contextmenu", function(e) {
                currentTab.selection.axisContextMenu(e, axis);
            });
        };

        add_axis_events(HN.Util.id("columnsout"), X);
        add_axis_events(HN.Util.id("rowsout"), Y);

        HN.Util.addEvent(document, "keydown", function(e) {
            currentTab.selection.keydown(e);
        });

        $("#scroller").bind('mousewheel', function(e, delta) {
            e.preventDefault();
            wheelScrolled(e, delta);
            return false;
        });

        $(window).bind("resize", function() { 
            setTimeout(public.gridResize, 10) 
        });
        public.gridResize();


        function addScrollBarEvents(id, xy) {
            HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {  
                e.preventDefault();
                e.stopPropagation();
                var type = e.target.getAttribute("data-type");
                scrollBarPressed[type](e, xy);
                return false;
            });
        };

        addScrollBarEvents("scrollbar-bottom-inner", X);
        addScrollBarEvents("scrollbar-right-inner", Y);

    };
 
    initEvents();
    
    return public;
}