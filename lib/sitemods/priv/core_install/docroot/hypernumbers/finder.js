Finder = function(element, json, opts)
{
    var public   = {};
    
    var display  = [];
    var $wrapper = element.find(".pane");
    
    $wrapper.append("<ul class='finderroot'><li name='/' data-href='/' "
                    +"class='haschild selected'>/</li></ul>");
    
    addToList(["home"], json, makeList(["home"], json.children));
    initEvents();

    var listWidth = parseInt($wrapper.find("ul:eq(1)").css("width"), 10);

    public.active = false;
    public.path   = document.location.pathname.split("/");

    public.addPath = function(path) {
        path = strToPath(path);
        var model = getObj(display[0].object, path);
        addChildren(model.obj, model.left);
    };

    public.updateBar = function() {
        
        $("#pagebar *:gt(0)").remove();

        var npath = public.path.splice(1);
        var path  = ["home"];

        $.each(npath, function() {
            if( this != "" ) {
                path.push(this);
                var url = path.join("/");
                $("#pagebar").append("<div class='crumb'></div>"
                                     +"<a data-href='"+url+"'>"+this+"</a>");
            }
        });
    };
    
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

        path = $.grep(path, function(a) { return a != ""; });

        if( path.length == 0 ) {
            path = ["home"];
        }

        public.path = path;

        var start   = findStartOffset(path);
        var current = findChild(display[start-1].object.children, path[start]);
        var npath   = path.slice(0, start);
        
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
        setTimeout(function() {
            scrollIntoView();
        }, 0);

        if( opts.itemSelected ) {
            opts.itemSelected.apply(public, []);
        }
    };

    public.chooseCurrent = function() {
        if( opts.itemChosen ) {
            opts.itemChosen.apply(public, []);
        }
    };

    // Most of this stuff is nasty, should rejig the tree a bit
    public.selectDown = function() {
        var selection = getSelected();
        if( !selection ) {
            return;
        }
        var children = selection.parent.object.children;
        if( selection.index+1 < children.length ) {
            var path = selection.parent.path.slice();
            path.push(children[selection.index+1].name);
            public.select(path);
        }
    };

    public.selectUp = function() {
        var selection = getSelected();
        if( !selection ) {
            return;
        }
        var children = selection.parent.object.children;
        if( selection.index > 0 ) {
            var path = selection.parent.path.slice();
            path.push(children[selection.index-1].name);
            public.select(path);
        }
    };

    public.selectRight = function() {
        var selection = getSelected();
        if( !selection ) {

            if( typeof display[0].object.children !== "undefined" ) {
                public.select(["home", display[0].object.children[0].name]);
            }

            return;
        }
        var children = selection.parent.object.children;
        if( typeof children[selection.index].children != "undefined" ) {
            var path = public.path.slice();
            path.push(children[selection.index].children[0].name);
            public.select(path);
        }
    };

    public.selectLeft = function() {
        if( public.path.length > 1 ) {
            public.select(public.path.slice(0, -1));
        }
    };

    public.asString = function() { 
        return ( public.path.length == 1 )
            ? "/" : "/"+public.path.slice(1).join("/")+"/";        
    };

    // should be all nice with animations, ok for now
    // TODO scrollIntoView is broken, fix
    function scrollIntoView() {
        var last = $wrapper.find(".selected:last");
        // if( last.length > 0 ) {
        //     last[0].scrollIntoView(true);
        // }
        // // Stop safari being dumb
        // element[0].scrollTop = 0;
    };

    function addChildren(rootObj, nPath) {
        
        if( nPath.length == 0 ) {
            return true;
        }
        var name     = nPath.shift();
        var childObj = {"name":name};
        
        if( typeof rootObj.children == "undefined" ) {
            rootObj.children = [];
        }
        
        rootObj.children.push(childObj);        
        return addChildren(childObj, nPath);
    }

    function getObj(root, path) {

        if( typeof root.children == "undefined" ) { 
            return {"obj":root, "left":path};
        }
 
        var child = path.shift();
        for( var i in root.children ) {
            if( root.children[i].name == child ) {
                return getObj(root.children[i], path);
            }
        }
        path.unshift(child);
        return {"obj":root, "left": path};

    };

    function getSelected() {

        var index = public.path.length < 2 ? 0 : public.path.length - 2;
        var list  = display[index];
        var name  = list.dom.find(".selected").attr("name");

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
            if( is_inside(e.target, $("#pagebar")[0]) ) {
                if( e.target.nodeName == "A" ) {
                    public.select(
                        e.target.getAttribute("data-href").split("/"));
                    
                    if( !public.active ) {
                        public.activate();
                    }
                }
            } else {
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

        if( document.activeElement.nodeName == "INPUT" ) {
            return;
        }

        var k = e.keyCode;

        if ( k == Keys.ESC )                         { public.deactivate(); }
        else if ( k == Keys.DOWN )                   { public.selectDown(); }
        else if ( k == Keys.UP )                     { public.selectUp(); }
        else if ( k == Keys.RIGHT || k == Keys.TAB ) { public.selectRight(); }
        else if ( k == Keys.ENTER )                  { itemChosen(); }
        else if ( k == Keys.LEFT || k == Keys.BACKSPACE ) {
            public.selectLeft(); }
        // TODO

    };

    function strToPath(str) {
        return $.grep(str.split("/"), function(a) { return a != ""; });
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
        $wrapper.width((display.length * listWidth + 120)).append(list);
    };

    function makeList(path, arr) {
        
        arr.sort(function(a,b) { 
            return a.name.toLowerCase() > b.name.toLowerCase(); 
        });
        
        for( var html="<ul>", len=arr.length, x=0; x < len; x++ ) {

            var name  = arr[x].name;
            var data  = "data-href='"+path.join("/")+"/"+name+"'";
            var arrow = ( typeof arr[x].children != "undefined" )
                ? "class='haschild'" : "";
            
            html += "<li name='"+name+"' "+data+" "+arrow+">"+name+"</li>";
        }
        return $(html+"</ul>");
    };
    
    
    function initEvents() {
        $(document).bind("mousedown", clicked);
    };
    
    // Find the longest route that the old and new
    // selection have in common
    function findStartOffset(path) {
        for( var x = 0; x < path.length; x++ ) {
            if( typeof display[x] == "undefined" || 
                display[x].object.name != path[x] ) {
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
        $wrapper.width((display.length * listWidth) + 120);
        return true;
    };
    
    Keys           = {};
    Keys.ENTER     = 13;
    Keys.DELETE    = 46;
    Keys.UP        = 38;
    Keys.DOWN      = 40;
    Keys.LEFT      = 37;
    Keys.RIGHT     = 39;
    Keys.SHIFT     = 16;
    Keys.CTRL      = 17;
    Keys.TAB       = 9;
    Keys.ALT       = 18;
    Keys.ESC       = 27;
    Keys.BACKSPACE = 8;

    public.updateBar();

    return public;
};


