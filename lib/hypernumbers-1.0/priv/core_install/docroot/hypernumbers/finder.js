Finder = function(element, json, opts)
{
    var api		= {};

    var active		= false;
    var currentPath	= null;
    var model		= json;
    var visibleLists	= [];    
    var $wrapper	= element.find(".pane");
    
    var pathStrToNodeId = function(pathStr) {
	      var id = "node-" + pathStr;
	      return id;
    };
    
    // We use a function to translate ids to path strings rather than
    // accessing the array directly, as it appears that some functions
    // are using an outdated copy of the array.
    
    var nodeIdToPathStr = function(id) {
        var pathStr;
        pathStr = id.substring(9, id.length);
        return pathStr.replace(/X/g, "/");
    };

    var listToPath = function (list) { 
        return "X" +
            list.join("X") +
            ((list.length > 0) ? "X" : "");    
    };
    
    // Create the initial tree structure to be displayed by jsTree
    
    var root = "/ (frontpage)";

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
    
    api.addPath = function(path) {
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

    api.select = function(path) {

        if( currentPath &&
            HN.Util.listToPath(path) == HN.Util.listToPath(currentPath) ) {
            return;
        }
        currentPath = path;
        
        if( opts.itemSelected ) {
            opts.itemSelected.apply(api, []);
        }
    };

    api.activate = function() {
        if( !active ) {

            if( opts.activate ) {
                opts.activate.apply(api, []);
            }
            
            $(document).keypress(keydown);
            active = true;
        }
    };

    api.deactivate = function() {

        if( active ) {
            
            if( opts.deactivate ) {
                opts.deactivate.apply(api, []);
            }

            $(document).unbind("keypress", keydown);
            active = false;
        }
    };

    api.chooseCurrent = function() {
        if( opts.itemChosen ) {
            opts.itemChosen.apply(api, []);
        }
    };

    api.asString = function() { 
        return HN.Util.listToPath(currentPath);
    };

    api.asList = function() { 
        return currentPath;
    };

    // Function to translate the model to jsTree data.

    function model2tree(models,pathStr) {
	      if (models) {
	          return models.map(function(node) {
                    var path = pathStr + node.name + "X";
                    return {
	                      "data"     : node.name,
                        // "state" : node.children ? "open" : "closed",
		                    "attr"     : { "id" : pathStrToNodeId(path) },
                        "children" : model2tree(node.children, path) 
	                  };
            }); 
	          
	      } else {
	          return [];
	      }
    }
    
    api.openPageByPath  = function(pathStr) {
	      hn.ctrlPanel.pageSelected(HN.Util.pathToList(pathStr));
        api.select(HN.Util.pathToList(pathStr)); 
	      api.chooseCurrent();
        parent.location.hash = hn.hashUrl.setParam("path", pathStr);
    };
  
    api.openPageById = function(id) {
        var pathStr = nodeIdToPathStr(id);
 	      api.openPageByPath(pathStr);
    };
    
    var init = function() {
        
	      var theme = "default",
            node  = hn.hashUrl.getParam("path");
        
	      if (!node) {
            node = root;
        }

 	      if (!currentPath) {
            api.select(HN.Util.pathToList(node));
        }

        var root_id = pathStrToNodeId("homeX"),
            tree    = [{ "data" : root, "attr" : { "id" : root_id } }],
            opens   = [ root_id ];
	      if (model) {
	          tree = model2tree([ model ], "");
	          tree[0].data = root;// display root "home" as "/"
            
	          // The tree should be open to the current node. This
	          // requires the parent nodes also be open.
            
	          for (var i = 0; i < currentPath.length; i++) {
                var pathslice = listToPath(currentPath.slice(0,i+1));
		            opens.push(pathStrToNodeId("home" + pathslice));
	          }
	      }

	      $($wrapper).jstree({
            "core"    : { 
                "initially_open" : opens,
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
            "plugins" : [ "themes", "json_data", "crrm", "ui" ]
        });
        
	      $($wrapper).bind("select_node.jstree", function(e,d) {
            var pathStr = nodeIdToPathStr(d.rslt.obj.attr("id"));
	          hn.ctrlPanel.pageSelected(HN.Util.pathToList(pathStr));
            api.select(HN.Util.pathToList(pathStr)); 
        });
        
	// jsTree doesn't handle double-clicks well, so we use the
	// workaround below to bind to double-clicks.
        
	$($wrapper).delegate("li", "dblclick", function(e) {
                e.stopPropagation();
                var pathStr = nodeIdToPathStr(this.id);
	              hn.ctrlPanel.pageSelected(HN.Util.pathToList(pathStr));
                api.select(HN.Util.pathToList(pathStr)); 
                api.openPageByPath(pathStr);
  });
        
    };
    
    init();
    
    function getData(path, model) { 
        
        var currentData = model,
            found       = false;
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
            opts.itemChosen.apply(api, []);
        }
    }

/*
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
*/

    function keydown(e) {
/*                
        if( document.activeElement.nodeName == "INPUT" ) {
            return;
        }
*/
        if (e.keyCode == 27) {
	          hn.ctrlPanel.close();
	      }
    };
        
    return api;
};