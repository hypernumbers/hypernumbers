/**
 * @class HN.Util
 *
 */
HN.namespace("Util");

HN.Util.parseRef = function(ref) { 

    var list = ref.split("/"),
    range    = list.pop().toUpperCase(),
    addr     = HN.Util.parseAddr(range);
    
    // Should handle ranges / rows / cols
    return { "path" : HN.Util.arrayToPath(list),
             "type" : addr.type, 
             "obj"  : addr.obj };
}

HN.Util.parseAddr = function(str) {
    
    if( str.match(/[a-z]:[a-z]/i) ) {
        var tmp = str.split(":");
        return {"type":"colrange", "obj": {"x1": HN.Util.from_b26(tmp[0]), 
                                           "x2": HN.Util.from_b26(tmp[1])}};
    }
    else if( str.match(/[0-9]:[0-9]/i) ) {
        var tmp = str.split(":");
        return {"type":"rowrange", "obj": {"y1": parseInt(tmp[0]), 
                                           "y2": parseInt(tmp[1])}};
    }
    else if( str.match(/[A-Z]-?[0-9]:[A-Z]-?[0-9]/) ) {
        return {"type":"range", "obj": HN.Util.parse_range(str)};
    }
    else if( str.match(/-?[A-Z]-?[0-9]/i) ) {
        return {"type":"cell", "obj":HN.Util.parse_ref(str)};
    }
};

HN.Util.refToStr = function(ref) {
    if( ref.type == "colrange" ) {
        return HN.Util.to_b26(ref.obj.x1) + ":" + HN.Util.to_b26(ref.obj.x1);
    } else if( ref.type == "cell" ) {
        return HN.Util.to_b26(ref.obj.x) + ref.obj.y;
    }
};

HN.Util.arrayToPath = function(arr) {
    var empty = arr.length == 0,
    path      = document.location.pathname, // not sure this should resolve
    append    = (empty) ? "" : "/",
    prepend   = (empty || arr[0].length != 0) ? path : "";

    return prepend + arr.join("/") + append;
};

HN.Util.dispatch = function(fun, args)
{
    if( typeof fun !== "undefined" ) {
        fun.apply(this, args);
    }
};

if (typeof window.addEventListener === 'function') { 
    HN.Util.addEvent = function(el, type, fn) {
        el.addEventListener(type, fn, false);
    }; 
    HN.Util.removeEvent = function(el, type, fn) {
        el.removeEventListener(type, fn, false);
    }; 
} else if (typeof document.attachEvent === 'function') { // IE
    HN.Util.addEvent = function(el, type, fn) { 
        el.attachEvent('on' + type, fn);
    };
    HN.Util.removeEvent = function(el, type, fn) { 
        el.detachEvent('on' + type, fn);
    }; 
} else { // older browsers
    HN.Util.addEvent = function(el, type, fn) { 
        el['on' + type] = fn;
    }; 
    HN.Util.removeEvent = function(el, type, fn) {
        el['on' + type] = null;
    };
}

HN.Util.id = function(id)
{
    return document.getElementById(id);
};

HN.Util.is_visible = function(elem) {
    return ($(elem).is(':visible') && 
            $(elem).parents(':hidden').length == 0);
};

HN.Util.y_pos = function(obj)
{
    var top = 0;
    if (obj.offsetParent) {
        do {
	        top += obj.offsetTop;
        } while ((obj = obj.offsetParent));
    }
    return top;
};

HN.Util.x_pos = function(obj)
{
    var left = 0;
    if (obj.offsetParent) {
        do {
	        left += obj.offsetLeft;
        } while ((obj = obj.offsetParent));
    }
    return left;
};

HN.Util.to_b26 = function(cell)
{
    function f(num) {
        return (num <= 26)
            ? String.fromCharCode(num+64)
            : f(Math.floor(num / 26)) + f(num%26);
    };
    
    if( typeof cell == "number" ) {
        return f(cell);
    }

    throw "Not a number";
};

HN.Util.from_b26 = function(cell)
{
    for(var i = cell.length, pow = 0, sum = 0; i > 0; i--) {
        sum += Math.round((cell.charCodeAt(i-1)-64) * Math.pow(26, pow++));
    }
    return sum;
};

HN.Util.parse_relative = function(relref) {
    var bits;
    var start;
    var end;
    bits = relref.split(":");
    if (bits[1]) {
        start = HN.Util.resolve_rel_cell(bits[0]);
        end = HN.Util.resolve_rel_cell(bits[1]);
    } else {
        start = HN.Util.resolve_rel_cell(bits[0]);
        end = start;
    }
    return {x1: start.x,
            y1: start.y,
            x2: end.x,
            y2: end.y};
};

HN.Util.resolve_rel_cell = function(ref) {
    var x;
    var y;
    var xref = ref.match(/-?[a-z]+/i)[0];
    if (xref[0] === "-") {
        x = - HN.Util.from_b26(xref.slice(1, xref.length));
    } else {
        x = HN.Util.from_b26(xref);
    }
    return {x: x,
            y: parseInt(ref.match(/-?[0-9]+/)[0])};
};

HN.Util.parse_ref = function(ref)
{
    return {
        "x": HN.Util.from_b26((ref.match(/[a-z]+/i)[0])),
        "y": parseInt(ref.match(/-?[0-9]+/)[0])
    };
};

HN.Util.parse_cols = function(colrange)
{
    var cols = colrange.split(":");
    return {
        x1: HN.Util.from_b26(cols[0]),
        x2: HN.Util.from_b26(cols[1])
    };
};

HN.Util.parse_rows = function(rowrange)
{
    var rows = rowrange.split(":");
    return {
        y1: rows[0],
        y2: rows[1]
    };
};


HN.Util.parse_range = function(range)
{
    var cells = range.split(":"),
    beginning = HN.Util.parse_ref(cells[0]),
    end       = (cells.length > 1) 
        ? HN.Util.parse_ref(cells[1]) 
        : beginning;
    
    return {
        x1: beginning.x,
        y1: beginning.y,
        x2: end.x,
        y2: end.y
    };
};


HN.Util.coord_to_ref = function(cell)
{
    return HN.Util.to_b26(cell.x)+cell.y;
};

HN.Util.clone = function(obj)
{
    var newObj = (obj instanceof Array) ? [] : {};
    for (i in obj) {
        if (i == 'clone') {
            continue;
        }
        if (obj[i] && typeof obj[i] == "object") {
            newObj[i] = obj[i].clone();
        } else {
            newObj[i] = obj[i];
        }
    }
    return newObj;
};

HN.Util.range_to_str = function(range)
{
    return HN.Util.coord_to_ref({"x":range.x1, "y":range.y1})+":"
        + HN.Util.coord_to_ref({"x":range.x2, "y":range.y2});
};

HN.Util.range_to_str2 = function(range)
{
    if ((range.x1 == range.x2) && (range.y1 == range.y2)) {
        return HN.Util.coord_to_ref({"x":range.x1, "y":range.y1});
    } else {
        return HN.Util.coord_to_ref({"x":range.x1, "y":range.y1})+":"
            + HN.Util.coord_to_ref({"x":range.x2, "y":range.y2});
    }
};

HN.Util.postCell = function(path, cell, json)
{
    var url = path + HN.Util.coord_to_ref(cell);
    $.post(url, JSON.stringify(json), null, "json");
};

HN.Util.postPath = function(path, json, cb)
{
    $.post(path, JSON.stringify(json), cb, "json");
};

HN.Util.postColumn = function(path, start, end, json)
{
    var url = path + HN.Util.to_b26(start)
        + ":" + HN.Util.to_b26(end);
    $.post(url, JSON.stringify(json), null, "json");
};

HN.Util.postRow = function(path, start, end, json)
{
    var url = path + start + ":" + end;
    $.post(url, JSON.stringify(json), null, "json");
};


HN.Util.postRange = function(path, range, json)
{
    $.post(path+HN.Util.range_to_str(range), 
           JSON.stringify(json), null, "json");
};

HN.Util.readCookie = function(name)
{
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
	    var c = ca[i];
	    while (c.charAt(0)==' ') {
            c = c.substring(1,c.length);
        }
	    if( c.indexOf(nameEQ) == 0 ) {
            return c.substring(nameEQ.length+1,c.length-1);
        }
    }
    return null;
};

HN.Util.createCookie = function(name,value,days)
{
    if( days ) {
	    var date = new Date();
	    date.setTime(date.getTime()+(days*24*60*60*1000));
	    var expires = "; expires="+date.toGMTString();
    } else {
        var expires = "";
    }
    document.cookie = name+"=\""+value+"\""+expires+"; path=/";
};

HN.Util.eraseCookie = function(name) {
	HN.Util.createCookie(name,"",-1);
};
