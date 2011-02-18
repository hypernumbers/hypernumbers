Finder = function(element, json, opts)
{
    var public		= {};

    var active		= false;
    var currentPath	= null;
    var model		= json;
    var visibleLists	= [];    
    var $wrapper	= element.find(".pane");

    /* var listWidth = 120; */

    // pathStrToNodeId() is a function to translate arbitrary paths to
    // node ids. Since HTML spec is picky about what characters can be
    // in an id (and jQuery is even pickier), it's easiest to maintain
    // a hash table of path strings to unique node ids.

    // Since we want the same path to always have the same id, no
    // matter what order it's added, we use a cryptographic hash
    // function from https://code.google.com/p/crypto-js/ and memoise
    // the result.

    var nodeIds      = { }, // pathStr to nodeId
        nodePaths    = { }; // nodeId to path array

    function pathStrToNodeId(pathStr) {
        if (pathStr.substring(0,4) == "home") 
	    pathStr = pathStr.substring(4);    // change "home/" to "/"
	if (nodeIds[pathStr] === undefined) {
	    nodeIds[pathStr] = "node-" + Crypto.MD5(pathStr);
            nodePaths[nodeIds[pathStr]] = pathStr; 
	}
	return nodeIds[pathStr];
    }

    // Create the initial tree structure to be displayed by jsTree

    var root = "/";

    init();

/*
    function scrollIntoView($parent, $child) {

        var childTop     = $child.position().top - 26,
            scrollTop    = $parent[0].scrollTop,
            childHeight  = $child.height(),
            parentHeight = $parent.height(),
            newTop       = null;
        
        if (childTop < 0) {
            newTop = scrollTop + childTop;
        } else if (childTop + childHeight > parentHeight) {
            newTop = scrollTop + ((childTop + childHeight) - parentHeight);
        }

        if (newTop !== null) {
            if (Math.abs(newTop - scrollTop) > 200) { 
                $parent.animate({"scrollTop": newTop}, 'fast');
            } else {
                $parent[0].scrollTop = newTop;
            }
        }
    };
*/
    
    public.addPath = function(path) {
        var nPath   = HN.Util.clone(path);
        var tmpPath = [];

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

/*        
        var start = findStartOffset(path);
        var npath = path.slice(0, start);
        clearAfterOffset(start);
        
        for( var x = start, len = path.length; x <= len; x++ ) {
            
            var obj = (npath.length == 0 && model) || getData(npath, model);

            if( obj.children ) {

            }

            if( typeof visibleLists[x] != "undefined" ) {
                var tmp = visibleLists[x].dom.find("[name="+path[x]+"]")
                    .addClass("selected");

                if (tmp.length > 0) {
                    scrollIntoView(tmp.parent("ul"), tmp);
                }
            }

            npath.push(path[x]);
        }
*/        
        
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

/*
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

*/
    // Function to translate the model to jsTree data.

    function model2tree(models,pathStr) {
	if (models) {
	    return models.map(function(node) {
              var path = pathStr + node.name + "/";
              return {
	          "data"  : node.name,
		  "state" : node.children ? "open" : "closed",
		  "attr"  : { "id" : pathStrToNodeId(path) },
                  "children" : model2tree(node.children, path) 
	      };
            }); 
	    
	} else {
	    return [];
	}
    }

    function init() {

	var theme = "default",
            node  = root;

	// BUG sometimes currentPath is changed back after used
	// selects a different path

	if (currentPath) {
	    node = HN.Util.listToPath(currentPath);
	}

        var tree = [{ "data" : root, "attr" : { "id" : pathStrToNodeId(root) } }];
	if (model) {
	    tree = model2tree([ model ], "");
	    tree[0].data = root;              // display root "home" as "/"
	}

	$($wrapper).jstree({
          "core"    : { 
            "animation"      : 0,
            "strings"        : {
              "new_node"     : "New Sheet"
            }
          },
          "ui" : {
            "initially_select" : [ pathStrToNodeId(node) ],
            "select_limit" : 1
          },
          "json_data" : {
	     "data" : tree,
             "progressive_render" : true
	  }, 
          "themes" : { 
            "theme" : theme,
            "icons" : true,
            "dots"  : true,
            "url"   : "/jstree/themes/" + theme + "/style.css"
          },
          "plugins" : [ "themes", "json_data", "ui", "crrm" ]
         });
        
	$($wrapper).bind("select_node.jstree", function(e,d) {
          var pathStr = nodePaths[d.rslt.obj.attr("id")];
	  hn.ctrlPanel.pageSelected(HN.Util.pathToList(pathStr));
          public.select(HN.Util.pathToList(pathStr)); 
	  public.chooseCurrent();
        });
/*
        
        $($wrapper).html("<ul class='finderroot'><li name='/' data-href='/' "
                           +"class='haschild selected'>/</li></ul>");
        
        $wrapper.bind("mousedown", clicked);
        $(document).bind("dblclick", doubleClicked);

*/

    };

/*
    function appendUl(object, list) {
        visibleLists.push({"object":object, "dom":list});
	// TODO
        // $wrapper.width((visibleLists.length * listWidth + 120)).append(list);
    };
*/

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

/*
    function generateUl(path, data) { 
        data.children.sort(function(a, b) {
            return ((a.name.toLowerCase() > b.name.toLowerCase()) ? 1 : -1);
         });

        for( var html="<ul>", len=data.children.length, x=0; x < len; x++ ) {


            var pathStr = path.join("/")+"/",
                p_node = pathStrToNodeId(pathStr),
                name  = data.children[x].name + "/",
                node  = pathStrToNodeId(pathStr + name),
                data  = {
		  "data" : name,
                  "attr" : { "id" : node }  
		};

            if( name ) {


                var href  = "data-href='"+path.join("/")+"/"+name+"'";
                var arrow = ( typeof data.children[x].children != "undefined" )
                    ? "class='haschild'" : "";
                html += "<li name='"+name+"' "+href+" "+arrow+">"+name+"</li>";

            }
        }
        return $(html+"</ul>");        
  };
*/

    function keydown(e) {
                
        if( document.activeElement.nodeName == "INPUT" ) {
            return;
        }
        var k = e.keyCode;
        
        if ( k == Keys.ESC )                         { public.deactivate(); }
/*
        else if ( k == Keys.DOWN )                   { public.selectDown(); }
        else if ( k == Keys.UP )                     { public.selectUp(); }
        else if ( k == Keys.RIGHT || k == Keys.TAB ) { public.selectRight(); }
        else if ( k == Keys.ENTER )                  { itemChosen(); }
        else if ( k == Keys.LEFT || k == Keys.BACKSPACE ) {
            public.selectLeft(); }
*/
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