HN.namespace("Layout");

/**
 * @class HN.Layout
 * Handles the main layout controls, the grid / axis / formula bar and toolbar
 */
HN.Layout = function(s /*sheet*/) 
{
    var public = {};
    public.s = s;


    /**
     * Calculate number of (5x8) panes needed to fill screen
     */
    function calcRowsCols()
    {
        return {"rows":Math.ceil(size.y / HN.Sheet.CELL_HEIGHT / 8)+1,
                "cols":Math.ceil(size.x / HN.Sheet.CELL_WIDTH  / 5)+1};
    };

    /**
     * Resize the window, recalculate scrollbar size / position
     */
    function windowResize() 
    {
        // Stupid bug where size isnt recalculated when firebux / search
        // pops up
        inner.style.top = (parseInt(inner.style.top)+1)+"px";
        size.y = inner.clientHeight;
        size.x = inner.clientWidth;
        inner.style.top = (parseInt(inner.style.top)-1)+"px";

        calcScrollbarSize(Y);
        calcScrollbarSize(X);

        calcScrollbarPos(X);
        calcScrollbarPos(Y);

        var tmp = calcRowsCols();
        panes.rows = tmp.rows;
        panes.cols = tmp.cols;
        panes.refresh();
    };

    public.calcScrollbarPos = calcScrollbarPos;
    function calcScrollbarPos(xy)
    {
        var bar_s = parseInt(scrollbar[xy.str].style[xy.dimension],10),
        gutter_s = size[xy.str] - 32 - bar_s,
        pane_pos = -parseInt(inner.style[xy.coord], 10),
        pos = gutter_s * pane_pos / s[xy.dimension];
        return doScrollbarPos(pos, xy);
    };

    function doScrollbarPos(pos, xy) 
    {
        if( pos > scrollmax[xy.str] ) 
            pos = scrollmax[xy.str];
        else if (pos < 0)
            pos = 0;
        scrollbar[xy.str].style[xy.coord] = pos + "px";
        return pos;
    };

    /**
     * Calculate Scrollbar size
     */
    public.calcScrollbarSize = calcScrollbarSize;
    function calcScrollbarSize(xy)
    {
        var track_s = size[xy.str] - 32,
        bar_s = size[xy.str] * track_s / s[xy.dimension];
        scrollbar[xy.str].style[xy.dimension] = bar_s + "px";
        scrollmax[xy.str] = track_s - bar_s;
    };

    /**
     * Drag box pressed
     */
    function dragBoxPressed(clk_event, xy)
    {
        var scroll_pos = parseInt(scrollbar[xy.str].style[xy.coord],10),
        attr = (xy == Y) ? "clientY" : "clientX",
        gutter_s = 
            size[xy.str]
            - 32
            - parseInt(scrollbar[xy.str].style[xy.dimension],10);
        if (xy == Y) {
            var offset = 
                HN.Util.y_pos(gutter[Y.str]) 
                - HN.Util.y_pos(scrollbar[Y.str])
        } else {
            var offset = 
                HN.Util.x_pos(gutter[X.str]) 
                - HN.Util.x_pos(scrollbar[X.str])
        }
        offset += (clk_event[attr]);

        function scroll(mve_event) {
            var last_pos = (scroll_pos / gutter_s) * s[xy.dimension],
            pcnt = ((mve_event[attr] - offset) / gutter_s);
            pcnt = Math.max(0, Math.min(1, pcnt));
            var delta = pcnt * s[xy.dimension] - last_pos,
            res = snapToDistance(delta > 0, Math.abs(delta), xy);

            if (res.dist) {
                scroll_pos = doScroll(res.offset, res.dist, xy, false);  
            } else {
                // temporarily move slider
                    //var temp_pos = scroll_pos + delta / s[xy.dimension] * gutter_s;
                    //scroll_pos = doScrollbarPos(temp_pos, xy);
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
    function scrollButtonPressed(event, forward, xy)
    {
        var timer = null, interval = 300;

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
            scrollBy1(forward, xy);
            timer = setTimeout(repeat, interval);
            interval = Math.max(30, Math.floor(0.7 * interval));
        };
        repeat();
    };


    /** 
     * Gutter Pressed
     */
    function gutterPressed(event, xy) 
    {
        var timer = null, interval = 200,
        gut = gutter[xy.str],
        p = (xy == Y)
            ? event.pageY - HN.Util.y_pos(gut)
            : event.pageX - HN.Util.x_pos(gut);
        forward = p > parseInt(scrollbar[xy.str].style[xy.coord], 10);
        
        function halt() {
            HN.Util.removeEvent(document, "mouseup", halt);
            HN.Util.removeEvent(gut, "mouseout", halt)
            clearTimeout(timer);
        }

        HN.Util.addEvent(document, "mouseup", halt);
        HN.Util.addEvent(gut, "mouseout", halt);

        function gutterScroll() {
            var scroll_pos = parseInt(scrollbar[xy.str].style[xy.coord],10),
            bar_size = parseInt(scrollbar[xy.str].style[xy.dimension],10);
            if ( p > (scroll_pos + bar_size) != forward) {
                halt();
                return;
            }
            var res = snapToDistance(forward, size[xy.str], xy);
            doScroll(res.offset, res.dist, xy, true);
        };

        // Continue scrolling while mouse is pressed
        function repeat() {
            gutterScroll();
            timer = setTimeout(repeat, interval);
            interval = Math.max(15, Math.floor(0.7 * interval));
        };
        repeat();
    };

    function wheelScrolled(event, delta) 
    {
        var N = Math.min(Math.ceil(Math.abs(delta)),50),
        forward = delta < 0,
        axis = event.shiftKey ? X : Y;

        for (var i = N; i-->0;)
            scrollBy1(forward, axis);
    };

    public.scrollBy1 = scrollBy1;
    function scrollBy1(forward, xy)
    {
        if (!forward && s.offset[xy.str] == 1) 
            return;

        var f = ( xy == Y ) ? "row_height" : "col_width";
        if (forward)
            doScroll(1, -s[f](s.offset[xy.str]), xy, true);
        else
            doScroll(-1, s[f](s.offset[xy.str]-1), xy, true);
    };

    function doScroll(offset, dist, xy, canExpand)
    {
        var pane_pos = parseInt(inner.style[xy.coord],10) + dist,
        forward = offset >= 0;

        inner.style[xy.coord] = pane_pos + "px";
        axis_dom[xy.str].style[xy.coord] = pane_pos + "px";
        s.offset[xy.str] += offset;

        var spos = calcScrollbarPos(xy);

        if ( (spos >= scrollmax[xy.str]) && forward && canExpand ) {
            s.extend(xy, 2);
            calcScrollbarSize(xy);
            spos = calcScrollbarPos(xy);
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
    function checkPaneBounds(dist, xy, upleft)
    {
        var axis = xy.str, start = axis+"1", end = axis+"2";

        panes.bounds[start] += dist;
        panes.bounds[end]   += dist;

        // Todo could do with cleverer logic on how to shift cells when
        // scrolling large increments, wasting work here
        
        if( Math.abs(dist) > panes.p[0][0][xy.dimension] ) {
            var n = Math.floor(Math.abs(dist) / panes.p[0][0][xy.dimension]);
            for( var x = 0; x < n; x++ ) {
                panes["shift_"+axis](upleft);
            }
        }

        if( upleft && size[xy.str] > panes.bounds[end] ) {
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
    function snapToDistance(forward, distance, xy)
    {
        var i = s.offset[xy.str], total = 0,
        f = ( xy == Y ) ? "row_height" : "col_width";
        if (forward) {
            step = 1;
        } else {
            step = -1; 
            i -= 1;
        }
        
        while( true ) 
        {
            total += s[f](i);

            if( total > distance) {
                total -= s[f](i);
                return { offset: i - s.offset[xy.str] + (forward ? 0 : 1), 
                         dist: -step * total, };
            } 

            i += step;
            if (i < 1) {
                return { offset: 1 - s.offset[xy.str], 
                         dist: -parseInt(inner.style[xy.coord],10)};
            }
        }
    };


    $("#footer").click(function(e) {
        if(e.target.nodeName == "SPAN") {
            selection.state = HN.States.COPY_URL;
            HN.Util.select(e.target);
            return true;
        } else {
            return false;
        }
    });

    $("#scroller").bind('mousewheel', function(e, delta) {
        e.preventDefault();
        wheelScrolled(e, delta);
        return false;
    });

    var buttonF = function(id, mag, axis) {
        HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {
            scrollButtonPressed(e, mag, axis);
            e.preventDefault();
            if (e.stopPropagation) e.stopPropagation();
            else e.cancelBubble = true;
            return false;
        });
    };

    var barF = function(el, axis) {
        HN.Util.addEvent(el, "mousedown", function(e) {
            dragBoxPressed(e, axis);
            e.preventDefault();
            if (e.stopPropagation) e.stopPropagation();
            else e.cancelBubble = true;
            return false;
        });
    };

    var gutterF = function(el, axis) {
        HN.Util.addEvent(el, "mousedown", function(e) {
            gutterPressed(e, axis);
            e.preventDefault();
            if (e.stopPropagation) e.stopPropagation();
            else e.cancelBubble = true;
            return false;
        });
    };


    var inner = HN.Util.id("scroller");
    public.inner = inner;
    
    var size  = {"x":inner.clientWidth, "y":inner.clientHeight};
    public.size = size;

    var scrollmax = {"x":0, "y":0},
    scrollbar = {"x":HN.Util.id("bottom-bar"), 
                 "y":HN.Util.id("right-bar")},
    gutter    = {"x":HN.Util.id("scrollbar-bottom"), 
                 "y":HN.Util.id("scrollbar-right")},
    axis_dom  = {"x":HN.Util.id("columns"), 
                 "y":HN.Util.id("rows")};
    public.axis_dom = axis_dom;

    var panes = new HN.Layout.Panes(public, calcRowsCols());
    public.panes = panes;

    var selection = new HN.Layout.Selection(public);
    public.selection = selection;

    // Silly bug, need to be set expicitly before can read
    inner.style.top        = "0px";
    inner.style.left       = "0px";
    scrollbar.y.style.top  = "0px";
    scrollbar.x.style.left = "0px";

    calcScrollbarSize(Y);
    calcScrollbarSize(X);

    buttonF("down",  true,  Y);
    buttonF("up",    false, Y);
    buttonF("right", true,  X);
    buttonF("left",  false, X);

    barF(scrollbar.y, Y);
    barF(scrollbar.x, X);

    gutterF(gutter.y, Y);
    gutterF(gutter.x, X);

    HN.Util.addEvent(window, "resize", windowResize);

    return public;
}