/**
 * @class HN.Layout
 * Handles the main layout controls, the grid / axis / formula bar and toolbar
 */
HN.Layout = function(sheet)
{
    this.s = sheet;

    this.inner = HN.Util.id("scroller");

    // size of viewable area
    this.size      = {x:this.inner.clientWidth, y:this.inner.clientHeight};
    // maximum (top/right) position of scrollbar
    this.scrollmax = {x:0, y:0};
    // cache dom elements
    this.axis_dom  = {x:HN.Util.id("columns"),          y:HN.Util.id("rows")};
    this.scrollbar = {x:HN.Util.id("bottom-bar"),       y:HN.Util.id("right-bar")};
    this.gutter    = {x:HN.Util.id("scrollbar-bottom"), y:HN.Util.id("scrollbar-right")};

    // Silly bug, need to be set expicitly before can read
    this.inner.style.top = "0px";
    this.inner.style.left = "0px";
    this.scrollbar.y.style.top  = "0px";
    this.scrollbar.x.style.left = "0px";

    this.calcScrollbarSize(Y);
    this.calcScrollbarSize(X);

    this.panes     = new HN.Layout.Panes(this, this.calcRowsCols());
    this.selection = new HN.Layout.Selection(this);

    var that = this;

    var button = function(id, mag, axis) {
        HN.Util.addEvent(HN.Util.id(id), "mousedown", function(e) {
            that.scrollButtonPressed(e, mag, axis);
            e.preventDefault();
            if (e.stopPropagation) e.stopPropagation();
            else e.cancelBubble = true;
            return false
        });
    };

    var bar = function(el, axis) {
        HN.Util.addEvent(el, "mousedown", function(e) {
            that.dragBoxPressed(e, axis);
            e.preventDefault();
            if (e.stopPropagation) e.stopPropagation();
            else e.cancelBubble = true;
            return false;
        });
    };

    var gutter = function(el, axis) {
        HN.Util.addEvent(el, "mousedown", function(e) {
            e.preventDefault();
            that.gutterPressed(e, axis);
            return false;
        });
    };

    HN.Util.addEvent(window, "resize", function() {
        that.windowResize();
    });

    $("#footer").click(function(e) {
        if(e.target.nodeName == "SPAN") {
            that.selection.state = HN.States.COPY_URL;
            HN.Util.select(e.target);
            return true;
        } else {
            return false;
        }
    });

    $("#scroller").bind('mousewheel', function(e, delta) {
        e.preventDefault();
        that.wheelScrolled(e, delta);
        return false;
    });

    button("down",  true,  Y);
    button("up",    false, Y);
    button("right", true,  X);
    button("left",  false, X);

    bar(this.scrollbar.y, Y);
    bar(this.scrollbar.x, X);

    gutter(this.gutter.y, Y);
    gutter(this.gutter.x, X);
};

/**
 * Calculate number of (5x8) panes needed to fill screen
 */
HN.Layout.prototype.calcRowsCols = function()
{
    return {rows:Math.ceil(this.size.y / HN.Sheet.CELL_HEIGHT / 8)+1,
            cols:Math.ceil(this.size.x / HN.Sheet.CELL_WIDTH  / 5)+1};
};

/**
 * Resize the window, recalculate scrollbar size / position
 */
HN.Layout.prototype.windowResize = function()
{
    // Stupid bug where size isnt recalculated when firebux / search
    // pops up
    this.inner.style.top = (parseInt(this.inner.style.top)+1)+"px";
    this.size.y = this.inner.clientHeight;
    this.size.x = this.inner.clientWidth;
    this.inner.style.top = (parseInt(this.inner.style.top)-1)+"px";

    this.calcScrollbarSize(Y);
    this.calcScrollbarSize(X);

    this.calcScrollbarPos(X);
    this.calcScrollbarPos(Y);

    var tmp = this.calcRowsCols();
    this.panes.rows = tmp.rows;
    this.panes.cols = tmp.cols;
    this.panes.refresh();
};

HN.Layout.prototype.calcScrollbarPos = function(xy)
{
    var track_s = this.size[xy.str] - 32,
    pane_pos = -parseInt(this.inner.style[xy.coord], 10),
    pos = track_s * pane_pos / this.s[xy.dimension];
    return this.doScrollbarPos(pos, xy);
};

HN.Layout.prototype.doScrollbarPos = function(pos, xy) 
{
    if( pos > this.scrollmax[xy.str] ) 
        pos = this.scrollmax[xy.str];
    else if (pos < 0)
        pos = 0;
    this.scrollbar[xy.str].style[xy.coord] = pos + "px";
    return pos;
};

/**
 * Calculate Scrollbar size
 */
HN.Layout.prototype.calcScrollbarSize = function(xy)
{
    var track_s = this.size[xy.str] - 32,
    bar_s = track_s * track_s / this.s[xy.dimension]; // track_s * %
    this.scrollbar[xy.str].style[xy.dimension] = bar_s + "px";
    this.scrollmax[xy.str] = track_s - bar_s;
};

/**
 * Drag box pressed
 */
HN.Layout.prototype.dragBoxPressed = function(c_event, xy)
{
    var that = this,
    bar = this.scrollbar[xy.str],
    attr = (xy == Y) ? "clientY" : "clientX",
    origin = prev = c_event[attr],
    track_s = this.size[xy.str] - 32;
    
    var scroll = function(m_event) {
        var delta = m_event[attr] - origin;
        var est_dist = that.s[xy.dimension] * Math.abs(delta) / track_s;
        var res = that.snap_to_distance(delta > 0, est_dist, xy);
        if (res.dist != 0) {
            // snapped to a new cell
            that.doScroll(res.offset, res.dist, xy);  
            origin = m_event[attr];
        } else {
            // temporarily move slider
            var pos = parseInt(that.scrollbar[xy.str].style[xy.coord],10);
            that.doScrollbarPos(pos + (m_event[attr]-prev), xy);
        }
        prev = m_event[attr];
    };

    var onmouseup = function() {
        bar.className = "";
        HN.Util.removeEvent(document, "mouseup", onmouseup);
        HN.Util.removeEvent(document, "mousemove", scroll);
    };

    HN.Util.addEvent(document, "mouseup", onmouseup);
    HN.Util.addEvent(document, "mousemove", scroll);

    bar.className = "active";
};

/**
 * Scroll Button pressed
 */
HN.Layout.prototype.scrollButtonPressed = function(event, forward, xy)
{
    var that = this, timer = null, interval = 300;

    var halt = function() {
        event.target.className = "";
        HN.Util.removeEvent(document, "mouseup", halt);
        HN.Util.removeEvent(event.target, "mouseout", halt);
        clearTimeout(timer);
    };

    event.target.className = "active";
    HN.Util.addEvent(document, "mouseup", halt);
    HN.Util.addEvent(event.target, "mouseout", halt);

    // Continue scrolling while mouse is pressed
    var repeat = function() {
        that.scrollBy1(forward, xy);
        timer = setTimeout(repeat, interval);
        interval = Math.max(30, Math.floor(0.7 * interval));
    };
    repeat();
};


/** 
 * Gutter Pressed
 */
HN.Layout.prototype.gutterPressed = function(event, xy) 
{
    var that = this, timer = null, interval = 200;
    gut = this.gutter[xy.str],
    gutter_pos = $(gut).offset(),
    p = (xy == Y)
        ? event.pageY - gutter_pos.top
        : event.pageX - gutter_pos.left,
    forward = p > parseInt(that.scrollbar[xy.str].style[xy.coord], 10);
    
    var halt = function() {
        HN.Util.removeEvent(document, "mouseup", halt);
        HN.Util.removeEvent(gut, "mouseout", halt)
        clearTimeout(timer);
    };

    HN.Util.addEvent(document, "mouseup", halt);
    HN.Util.addEvent(gut, "mouseout", halt);

    var gutterf = function() {
        var scroll_pos = parseInt(that.scrollbar[xy.str].style[xy.coord],10),
        bar_size = parseInt(that.scrollbar[xy.str].style[xy.dimension],10);
        if ( p > (scroll_pos + bar_size) != forward) {
            halt();
            return;
        }
        res = that.snap_to_distance(forward, that.size[xy.str], xy);
        that.doScroll(res.offset, res.dist, xy);
    };

    // Continue scrolling while mouse is pressed
    var repeat = function() {
        gutterf();
        timer = setTimeout(repeat, interval);
        interval = Math.max(30, Math.floor(0.7 * interval));
    };
    repeat();
};

HN.Layout.prototype.wheelScrolled = function(event, delta) 
{
    var N = Math.min(Math.ceil(Math.abs(delta)),50);
    forward = delta < 0,
    axis = event.shiftKey ? X : Y;

    for (var i = N; i-->0;)
        this.scrollBy1(forward, axis);
};

HN.Layout.prototype.scrollBy1 = function(forward, xy)
{
    if (!forward && this.s.offset[xy.str] == 1) 
        return;

    var f = ( xy == Y ) ? "row_height" : "col_width";
    if (forward)
        this.doScroll(1, -this.s[f](this.s.offset[xy.str]), xy);
    else
        this.doScroll(-1, this.s[f](this.s.offset[xy.str]-1), xy);
};


HN.Layout.prototype.doScroll = function(offset, dist, xy)
{
    var pane_pos = parseInt(this.inner.style[xy.coord],10) + dist,
    forward = offset >= 0;

    this.inner.style[xy.coord] = pane_pos + "px";
    this.axis_dom[xy.str].style[xy.coord] = pane_pos + "px";
    this.s.offset[xy.str] += offset;

    if ((this.calcScrollbarPos(xy) >= this.scrollmax[xy.str]) && forward) {
        this.s.extend(xy, 2);
        this.calcScrollbarSize(xy);
        this.calcScrollbarPos(xy);
    }
    
    if( xy == Y ) {
        HN.Util.id("rsel").style.top = pane_pos+"px";
    } else {
        HN.Util.id("csel").style.left = pane_pos+"px";
    }

    // Wrap panes if needed
    this.check_pane_bounds(dist, xy, forward);
}

/**
 * Notify the grid user has scrolled, move bounds, check that grid
 * can be displayed within current bounds and pane tiles when
 * needed
 */
HN.Layout.prototype.check_pane_bounds = function(dist, xy, upleft)
{
    var axis = xy.str, start = axis+"1", end = axis+"2",
    p = this.panes;

    p.bounds[start] += dist;
    p.bounds[end]   += dist;

    // Todo could do with cleverer logic on how to shift cells when
    // scrolling large increments, wasting work here
    if( Math.abs(dist) > p.p[0][0][xy.dimension] ) {
        var n = Math.floor(Math.abs(dist) / p.p[0][0][xy.dimension]);
        for( var x = 0; x < n; x++ ) {
            p["shift_"+axis](upleft);
        }
    }

    if( upleft && this.size[xy.str] > p.bounds[end] ) {
        p["shift_"+axis](upleft);
    } else if( !upleft && p.bounds[start] > -40 ) {
        p["shift_"+axis](upleft);
    }
};


/** 
 * Calculate cell offset, and snapped-distance required to move along a given vector
 * until a target distance is reached.
 */
HN.Layout.prototype.snap_to_distance = function(forward, distance, xy)
{
    var s = this.s, i = s.offset[xy.str], total = 0,
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
                     dist: -parseInt(this.inner.style[xy.coord],10)};
        }
    }
};


// TODO: Used by selection... needs to merge with above.
HN.Layout.prototype.get_new_offset = function(position, xy)
{
    var s = this.s, i = 1, total = 0,
    f = ( xy == Y ) ? "row_height" : "col_width";
    
    while( true ) {
        total += s[f](i);
        
        if( total > position ) {
            return {index:i, position:total-s[f](i)};
        }
        
        ++i;
    };
};