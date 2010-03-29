Finder = function(element, json, opts)
{
    var public   = {};

    var active       = false;
    var currentPath  = null;
    var model        = json;
    var visibleLists = [];    
    var $wrapper     = element.find(".pane");

    var listWidth = 120;

    init();

    public.addPath = function(path) {

        var nPath   = HN.Util.clone(path);
        var tmpPath = [];

        console.log(path, nPath);

        for( var i=0; i<path.length; i++ ) {

            var segment = nPath.shift();

            tmpPath.push(segment);
            
            if( !getData(tmpPath, model) ) { 

                tmpPath.length--;
                var parent = getData(tmpPath, model) || model;
                if( typeof parent.children == "undefined" ) {
                    parent.children = [];
                }
                parent.children.push({"name":segment}); 
                tmpPath.push(segment);                
            }
        }
    };

    public.select = function(path) {

        if( currentPath &&
            HN.Util.listToPath(path) == HN.Util.listToPath(currentPath) ) {
            return;
        }

        currentPath = path;
        
        var start = findStartOffset(path);
        var npath = path.slice(0, start);
        clearAfterOffset(start);
        
        for( var x = start, len = path.length; x <= len; x++ ) {
            
            var obj = (npath.length == 0 && model) || getData(npath, model);

            if( obj.children ) {
                var ul = generateUl(npath, obj);
                appendUl(obj, ul);
            }

            if( typeof visibleLists[x] != "undefined" ) {
                visibleLists[x].dom.find("[name="+path[x]+"]")
                    .addClass("selected");                        
            }

            npath.push(path[x]);
        }

        if( opts.itemSelected ) {
            opts.itemSelected.apply(public, []);
        }
    };

    public.activate = function() {

        if( !active ) {

            if( opts.activate ) {
                opts.activate.apply(public, []);
            }
            
            $(document).bind("keydown.finder", keydown);
            active = true;
        }
    };

    public.deactivate = function() {

        if( active ) {
            
            if( opts.deactivate ) {
                opts.deactivate.apply(public, []);
            }

            $(document).unbind(".finder");
            active = false;
        }
    };

    public.chooseCurrent = function() {
        if( opts.itemChosen ) {
            opts.itemChosen.apply(public, []);
        }
    };

    public.asString = function() { 
        return HN.Util.listToPath(currentPath);
    };

    public.asList = function() { 
        return currentPath;
    };

    // Most of this stuff is nasty, should rejig the tree a bit
    public.selectDown = function() {        
        if( currentPath.length == 0 ) {
            return;
        }
        var path = visibleLists[currentPath.length-1]
            .dom.find(".selected").next().attr("data-href");
        if( path ) {    
            public.select(HN.Util.pathToList(path));
        }
    };

    public.selectUp = function() {
        if( currentPath.length == 0 ) {
            return;
        }
        var path = visibleLists[currentPath.length-1]
            .dom.find(".selected").prev().attr("data-href");
        if( path ) {    
            public.select(HN.Util.pathToList(path));
        }
    };

    public.selectRight = function() {
        if( visibleLists[currentPath.length] ) { 
            var path = visibleLists[currentPath.length].dom
                .find("li").attr("data-href");
            public.select(HN.Util.pathToList(path));
        }
    };

    public.selectLeft = function() {
        if( currentPath.length > 0 ) {
            public.select(currentPath.slice(0, -1));
        }
    };

    function init() {
        
        $wrapper.append("<ul class='finderroot'><li name='/' data-href='/' "
                        +"class='haschild selected'>/</li></ul>");
        
        $wrapper.bind("mousedown", clicked);
        $(document).bind("dblclick", doubleClicked);
    };

    function appendUl(object, list) {
        visibleLists.push({"object":object, "dom":list});
        $wrapper.width((visibleLists.length * listWidth + 120)).append(list);
    };


    function getData(path, model) { 

        var currentData = model;
        var found       = false;
        for( var x = 0, len = path.length; x < len; x++ ) {

            found = false;

            if( currentData.children ) {
                for( var i = 0, l = currentData.children.length; i < l; i++ ) {
                    if( currentData.children[i].name == path[x] ) {
                        currentData = currentData.children[i];
                        found = true;
                        break;
                    }
                }
            }
        }
        return found && currentData;  
    };

    function is_inside(obj, parent) {
        return ( obj == parent ) ||
            ( obj.parentNode != null && is_inside(obj.parentNode, parent) );
    };


    function clicked(e) {
        if( is_inside(e.target, $wrapper[0]) ) {
            if( e.target.nodeName == "LI" ) {
                var path = HN.Util.pathToList(
                    e.target.getAttribute("data-href"));
                public.select(path);
            }
        }
    };

    function doubleClicked(e) {
        if( is_inside(e.target, $wrapper[0]) ) {
            if( e.target.nodeName == "LI" ) {
                itemChosen();
            }
        }
    };

    function generateUl(path, data) { 
        
        data.children.sort(function(a,b) {
            return a.name.toLowerCase() > b.name.toLowerCase();
        });

        for( var html="<ul>", len=data.children.length, x=0; x < len; x++ ) {
            
            var name  = data.children[x].name;
            if( name ) {
                var href  = "data-href='"+path.join("/")+"/"+name+"'";
                var arrow = ( typeof data.children[x].children != "undefined" )
                    ? "class='haschild'" : "";
                html += "<li name='"+name+"' "+href+" "+arrow+">"+name+"</li>";
            }
        }
        return $(html+"</ul>");        
    };

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
    };

    // Find the longest route that the old and new
    // selection have in common
    function findStartOffset(path) {
        for( var x = 0; x < path.length; x++ ) {
            if( typeof visibleLists[x] == "undefined" || 
                visibleLists[x].object.name != path[x] ) {
                return x;
            }
        }
        return path.length;
    };

    // Delete everything from the previous selection that isnt
    // common with the new one
    function clearAfterOffset(offset) {
        for( var x = visibleLists.length; x > offset; x-- ) {
            visibleLists[x-1].dom.remove();
            visibleLists.length--;
        }
        $wrapper.width((visibleLists.length * 100) + 120);
        return true;
    };

    function itemChosen() {
        if( opts.itemChosen ) {
            opts.itemChosen.apply(public, []);
        }
    }

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
        
    return public;
};