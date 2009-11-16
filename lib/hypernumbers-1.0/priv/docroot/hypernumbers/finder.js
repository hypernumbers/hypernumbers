Finder = function(element, json, opts) 
{
    var public = {}, 
    display    = [],
    $wrapper   = element.find(".pane");

    addToList(["home"], json, makeList(["home"], json.children));
    var listWidth = parseInt($wrapper.find("ul").css("width"), 10);

    $(document).bind("mousedown", clicked);

    public.active = false;

    public.activate = function() {
        if( !public.active ) {

            if( opts.activate ) {
                opts.activate.apply(public, []);
            }

            $(document).bind("keydown.finder", keydown);
            public.active = true;
        }
    };

    public.deactivate = function() {

        if( public.active ) {

            if( opts.deactivate ) {
                opts.deactivate.apply(public, []);
            }

            $(document).unbind(".finder");
            public.active = false;
        }
    };

    public.select = function(path) {

        public.path = path;

        var start = findStartOffset(path),
        current   = findChild(display[start-1].object.children, path[start]),
        npath     = path.slice(0, start);

        display[start-1].dom.find(".selected").removeClass("selected");
        
        true == clearAfterOffset(start);
        
        for( var x = start, len = path.length; x <= len-1; x++ ) {
            
            if( typeof current.children != "undefined" ) {
                npath.push(path[x]);                
                addToList(npath, current, makeList(npath, current.children));
                current = findChild(current.children, path[x+1]);
            }

            if( typeof display[x-1] != "undefined" ) {
                display[x-1].dom.find("[name="+path[x]+"]")
                    .addClass("selected");
            } 
        }

        scrollIntoView();
    };

    // Most of this stuff is nasty, should rejig the tree a bit
    public.selectDown = function() {
        var selection = getSelected(), 
        children = selection.parent.object.children;
        if( selection.index+1 < children.length ) {
            var path = selection.parent.path.slice();
            path.push(children[selection.index+1].name);
            public.select(path);
        }
    };

    public.selectUp = function() {
        var selection = getSelected(), 
        children = selection.parent.object.children;
        if( selection.index > 0 ) {
            var path = selection.parent.path.slice();
            path.push(children[selection.index-1].name);
            public.select(path);
        }
    };

    public.selectRight = function() {
        var selection = getSelected(), 
        children = selection.parent.object.children;   
        if( typeof children[selection.index].children != "undefined" ) {
            var path = public.path.slice();
            path.push(children[selection.index].children[0].name);
            public.select(path);
        }
    };

    public.selectLeft = function() {
        if( public.path.length > 2 ) {
            public.select(public.path.slice(0, -1));
        }
    };

    // should be all nice with animations, ok for now
    function scrollIntoView() {
        var last = $wrapper.find(".selected:last");
        if( last.length > 0 ) { 
            last[0].scrollIntoView();
        }
        // Stop safari being dumb
        element[0].scrollTop = 0;
    };

    function getSelected() {

        var list = display[public.path.length-2],
        name     = list.dom.find(".selected").attr("name");

        for(var x = 0; x < list.object.children.length; x++ ) {
            if( list.object.children[x].name == name ) { 
                return {"parent":list, "index":x};
            }
        };
    };

    function clicked(e) {
        if( is_inside(e.target, element[0]) ) {
            if( e.target.nodeName == "LI" ) {
                var path = e.target.getAttribute("data-href").split("/");
                public.select(path);
            }
        } else {
            if( !is_inside(e.target, $("#pagebar")[0]) ) {
                public.deactivate();
            }
        }
    };

    function itemChosen() {
        if( opts.itemChosen ) {
            opts.itemChosen.apply(public, []);
        }
    }
    
    function keydown(e) {
        e.preventDefault();
        var k = e.keyCode;

        if      ( k == Keys.DOWN )                   { public.selectDown(); }
        else if ( k == Keys.UP )                     { public.selectUp(); }
        else if ( k == Keys.RIGHT || k == Keys.TAB ) { public.selectRight(); }
        else if ( k == Keys.ENTER )                  { itemChosen(); }
        else if ( k == Keys.LEFT || k == Keys.BACKSPACE ) { 
            public.selectLeft(); }
        // TODO 

    };

    function is_inside(obj, parent) {
        return ( obj == parent ) || 
            ( obj.parentNode != null && is_inside(obj.parentNode, parent) );
    };

    function findChild(list, name) {
        return $.grep(list, function(a) { return a.name == name; })[0];
    };

    function addToList(path, object, list) {
        display.push({"path": path, "object":object, "dom":list});
        $wrapper.width(display.length * listWidth).append(list);
    };

    function makeList(path, arr) {

        for( var html="<ul>", len=arr.length, x=0; x < len; x++ ) {

            var name = arr[x].name,
            data     = "data-href='"+path.join("/")+"/"+name+"'",
            arrow    = ( typeof arr[x].children != "undefined" ) 
                ? "class='haschild'" : "";
            
            html += "<li name='"+name+"' "+data+" "+arrow+">"+name+"</li>";
        }
        return $(html+"</ul>");
    };

    // Find the longest route that the old and new 
    // selection have in common
    function findStartOffset(path) {
        for( var x = 0; x < path.length; x++ ) {
            if( typeof display[x] == "undefined" 
                || display[x].object.name != path[x] ) {
                return x;
            }
        }
        return path.length;
    };
    
    // Delete everything from the previous selection that isnt
    // common with the new one
    function clearAfterOffset(offset) {
        for( var x = display.length; x > offset; x-- ) {
            display[x-1].dom.remove();
            display.length--;
        }
        $wrapper.width(display.length * listWidth);
        return true;
    };
    
    Keys = {};
    Keys.ENTER  = 13;
    Keys.DELETE = 46;
    Keys.UP     = 38;
    Keys.DOWN   = 40;
    Keys.LEFT   = 37;
    Keys.RIGHT  = 39;
    Keys.SHIFT  = 16;
    Keys.CTRL   = 17;
    Keys.TAB    = 9;
    Keys.ALT    = 18;
    Keys.BACKSPACE = 8;

    return public;
};


