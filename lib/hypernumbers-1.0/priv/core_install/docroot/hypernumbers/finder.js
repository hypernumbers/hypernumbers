/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: false, maxerr: 10000 */
/*global HN: false, hn: false, $: false, alert: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, updater: false, lang: false, toolbar: false  */
var Keys;
var Finder = function (element, json, opts)
{
    var api          = {},
        active       = false,
        currentPath  = null,
        model        = json,
        visibleLists = [],
        $wrapper     = element.find(".pane"),
        listWidth    = 120;

    // Set up the funs
    function itemChosen() {
        if( opts.itemChosen ) {
            opts.itemChosen.apply(api, []);
        }
    }

    function is_inside(obj, parent) {
        return ( obj === parent ) ||
            ( obj.parentNode !== null && is_inside(obj.parentNode, parent) );
    }
    
    function clicked(e) {
        if( is_inside(e.target, $wrapper[0]) ) {
            if( e.target.nodeName === "LI" ) {
                var path = HN.Util.pathToList(
                    e.target.getAttribute("data-href"));
                api.select(path);
            }
        }
    }

    function doubleClicked(e) {
        if( is_inside(e.target, $wrapper[0]) ) {
            if( e.target.nodeName === "LI" ) {
                itemChosen();
            }
        }
    }

    function init() {
        
        $($wrapper).html("<ul class='finderroot'><li name='/' data-href='/' " +
                           "class='haschild selected'>/</li></ul>");
        
        $wrapper.bind("mousedown", clicked);
        $(document).bind("dblclick", doubleClicked);
    }

    function appendUl(object, list) {
        visibleLists.push({"object": object, "dom": list});
        $wrapper.width((visibleLists.length * listWidth + 120)).append(list);
    }

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
    }

    function getData(path, model) { 

        var currentData = model,
            found       = false,
            x, len, i, l;
        
        for(x = 0, len = path.length; x < len; x = x + 1) {

            found = false;

            if( currentData.children ) {
                for(i = 0, l = currentData.children.length; i < l; i = i + 1) {
                    if( currentData.children[i].name === path[x] ) {
                        currentData = currentData.children[i];
                        found = true;
                        break;
                    }
                }
            }
        }
        return found && currentData;  
    }

    function generateUl(path, data) { 

        var html, len, x, name, href, arrow;
        
        data.children.sort(function (a, b) {
            return ((a.name.toLowerCase() > b.name.toLowerCase()) ? 1 : -1);
         });
        for (html = "<ul>", len = data.children.length, x = 0; x < len; x = x + 1) {
            
            name  = data.children[x].name;
            if (name) {
                href  = "data-href='" + path.join("/") + "/" + name + "'";
                arrow = (typeof data.children[x].children !== "undefined") ?
                    "class='haschild'" : "";
                html += "<li name='" + name + "' " + href + " " + arrow + ">" + name + "</li>";
            }
        }
        return $(html + "</ul>");        
    }
    
    function keydown(e) {

        var k;
        
        if( document.activeElement.nodeName === "INPUT" ) {
            return;
        }
        k = e.keyCode;
        
        if (k === Keys.ESC) {
            api.deactivate();
        }
        else if ( k === Keys.DOWN) {
            api.selectDown();
        }
        else if ( k === Keys.UP) {
            api.selectUp();
        }
        else if ( k === Keys.RIGHT || k === Keys.TAB) {
            api.selectRight();
        }
        else if ( k === Keys.ENTER) {
            itemChosen();
        }
        else if ( k === Keys.LEFT || k === Keys.BACKSPACE) {
            api.selectLeft();
        }
    }

    // Find the longest route that the old and new
    // selection have in common
    function findStartOffset(path) {
        var x;
        for (x = 0; x < path.length; x = x + 1) {
            if( typeof visibleLists[x] === "undefined" || 
                visibleLists[x].object.name !== path[x] ) {
                return x;
            }
        }
        return path.length;
    }

    // Delete everything from the previous selection that isnt
    // common with the new one
    function clearAfterOffset(offset) {
        var x;
        for (x = visibleLists.length; x > offset; x-- ) {
            visibleLists[x-1].dom.remove();
            visibleLists.length--;
        }
        $wrapper.width((visibleLists.length * 100) + 120);
        return true;
    }
    
    // Now initialise
    init();

    // Now set up the api
    api.addPath = function(path) {
        var nPath   = HN.Util.clone(path),
            tmpPath = [],
            i, segment, parent;

        for (i = 0; i < path.length; i = i + 1) {

            segment = nPath.shift();

            tmpPath.push(segment);
            
            if( !getData(tmpPath, model) ) { 

                tmpPath.length--;
                parent = getData(tmpPath, model) || model;
                if (typeof parent.children === "undefined") {
                    parent.children = [];
                }
                parent.children.push({"name": segment}); 
                tmpPath.push(segment);                
            }
        }
    };

    api.select = function (path) {

        var start, npath, x, len, obj, ul, tmp;
        
        if( currentPath &&
            HN.Util.listToPath(path) === HN.Util.listToPath(currentPath) ) {
            return;
        }

        currentPath = path;
        
        start = findStartOffset(path);
        npath = path.slice(0, start);
        clearAfterOffset(start);
        
        for (x = start, len = path.length; x <= len; x = x + 1) {
            
            obj = (npath.length === 0 && model) || getData(npath, model);

            if (obj.children) {
                ul = generateUl(npath, obj);
                appendUl(obj, ul);
            }

            if (typeof visibleLists[x] !== "undefined") {
                tmp = visibleLists[x].dom.find("[name=" + path[x] + "]")
                    .addClass("selected");

                if (tmp.length > 0) {
                    scrollIntoView(tmp.parent("ul"), tmp);
                }
            }

            npath.push(path[x]);
        }
        
        if( opts.itemSelected ) {
            opts.itemSelected.apply(api, []);
        }
    };

    api.activate = function () {

        if (!active) {

            if( opts.activate ) {
                opts.activate.apply(api, []);
            }
            
            $(document).bind("keydown.finder", keydown);
            active = true;
        }
    };

    api.deactivate = function () {

        if (active) {
            
            if (opts.deactivate) {
                opts.deactivate.apply(api, []);
            }

            $(document).unbind(".finder");
            active = false;
        }
    };

    api.chooseCurrent = function () {
        if (opts.itemChosen) {
            opts.itemChosen.apply(api, []);
        }
    };

    api.asString = function () { 
        return HN.Util.listToPath(currentPath);
    };

    api.asList = function () { 
        return currentPath;
    };

    // Most of this stuff is nasty, should rejig the tree a bit
    api.selectDown = function () {        
        if (currentPath.length === 0) {
            return;
        }
        var path = visibleLists[currentPath.length - 1]
            .dom.find(".selected").next().attr("data-href");
        if (path) {    
            api.select(HN.Util.pathToList(path));
        }
    };

    api.selectUp = function () {
        if (currentPath.length === 0) {
            return;
        }
        var path = visibleLists[currentPath.length - 1]
            .dom.find(".selected").prev().attr("data-href");
        if (path) {    
            api.select(HN.Util.pathToList(path));
        }
    };

    api.selectRight = function () {
        if (visibleLists[currentPath.length]) { 
            var path = visibleLists[currentPath.length].dom
                .find("li").attr("data-href");
            api.select(HN.Util.pathToList(path));
        }
    };

    api.selectLeft = function () {
        if (currentPath.length > 0) {
            api.select(currentPath.slice(0, -1));
        }
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
        
    return api;
};