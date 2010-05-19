/**
 * @class HN.Util
 *
 */
HN.namespace("Util");

HN.Util.parseRef = function(ref) {
    
    var list  = ref.split("/");
    var range = list.pop().toUpperCase();
    var addr  = HN.Util.parseAddr(range);
    
    list.push("");
    var path = HN.Util.relToAbsPath(document.location.pathname, list.join("/"));

    return (addr === undefined) 
        ? { "path":path, "type":"path" }
        : { "path":path, "type":addr.type, "obj":addr.obj };
};

HN.Util.equals = function(obj1, obj2) {
    for (var i in obj1) {
        if( obj1[i] != obj2[i] ) {
            return false;
        }
    }
    return true;
};

HN.Util.isCongruent = function(type1, ref1, type2, ref2) {

    if (type1 == "cell" || type1 === "row_from_last" 
        || type1 === "col_from_last") {
        
        return type2 === "cell"
            || type2 === "row_from_last"
            || type2 === "col_from_last";
        
    } else if (type1 === "range" || type1 === "row_range_from_last"
               || type1 === "col_range_from_last") {

        if (type2 === "range"
            || type2 === "row_range_from_last"
            || type2 === "col_range_from_last") {

            var r1 = HN.Util.parse_range(ref1.toUpperCase());
            var r2 = HN.Util.parse_range(ref2.toUpperCase());
            
            return (((r1.x2 - r1.x1) === (r2.x2 - r2.x1))
                    && ((r1.y2 - r1.y1) === (r2.y2 - r2.y1)));

        }
    }
    return false;
};

HN.Util.parseAddr = function(ref) {
    var cell                = /^[a-zA-Z]+[0-9]+$/;
    var range               = /^[a-zA-Z]+[0-9]+:[a-zA-Z]+[0-9]+$/;
    var col_range           = /^[a-zA-Z]+:[a-zA-Z]+$/;
    var col_from_last       = /^[a-zA-Z]+\-[0-9]+$/;
    var col_range_from_last = /^[a-zA-Z]+\-[0-9]+:[a-zA-Z]+\-[0-9]+$/;
    var row_range           = /^[0-9]+:[0-9]+$/;
    var row_from_last       = /^\-[a-zA-Z]+[0-9]+$/;
    var row_range_from_last = /^\-[a-zA-Z]+[0-9]+:\-[a-zA-Z]+[0-9]$/;
    if (cell.exec(ref) && (cell.exec(ref)[0] === ref)) {
        return {"type": "cell", "obj": HN.Util.parse_ref(ref)};
    } else if (range.exec(ref)   && (range.exec(ref)[0] === ref)) {
        return {"type": "range", "obj": HN.Util.parse_range(ref)};
    } else if (col_range.exec(ref) && (col_range.exec(ref)[0] === ref)) {
        var tmp = ref.split(":");
        return {"type":"col_range", "obj": {"x1": HN.Util.from_b26(tmp[0]),
                                            "x2": HN.Util.from_b26(tmp[1])}};
    } else if (col_from_last.exec(ref) 
               && (col_from_last.exec(ref)[0] === ref)) {
        var tmp = ref.split(":");
        return {"type":"col_from_last", 
                "obj": {"x1": HN.Util.from_b26(tmp[0]),
                        "x2": HN.Util.from_b26(tmp[1])}};
    } else if (col_range_from_last.exec(ref) 
               && (col_range_from_last.exec(ref)[0] === ref)) {
        return {"type":"col_range_from_last", "obj": HN.Util.parse_range(ref)};
    } else if (row_range.exec(ref) && (row_range.exec(ref)[0] === ref)) {
        var tmp = ref.split(":");
        return {"type":"row_range", "obj": {"y1": parseInt(tmp[0]),
                                            "y2": parseInt(tmp[1])}};
    } else if (row_from_last.exec(ref) 
               && (row_from_last.exec(ref)[0] === ref)) {
        var tmp = ref.split(":");
        return {"type":"row_from_last", "obj": {"y1": parseInt(tmp[0]),
                                                "y2": parseInt(tmp[1])}};
    } else if (row_range_from_last.exec(ref) 
               && (row_range_from_last.exec(ref)[0] === ref)) {
        var tmp = ref.split(":");
        return {"type":"row_range_from_last", "obj": HN.Util.parse_range(ref)};
    };
};

HN.Util.refToStr = function(ref) 
{
    switch (ref.type) {
    case "cell":
        return HN.Util.to_b26(ref.obj.x) + ref.obj.y;
    case "range":
        return HN.Util.to_b26(ref.obj.x1) + ref.obj.y1 + ":"
            + HN.Util.to_b26(ref.obj.x2) + ref.obj.y2;
    case "col_range":
        return HN.Util.to_b26(ref.obj.x1) + ":" + HN.Util.to_b26(ref.obj.x2);
    case "row_range":
        return ref.obj.y1 + ":" + ref.obj.y2;
    case "row_from_last":
        return "-" + HN.Util.to_b26(-ref.obj.x1) + ref.obj.y1 + ":"
            + "-" + HN.Util.to_b26(-ref.obj.x2) + ref.obj.y2;
    case "row_range_from_last":
        return "-" + HN.Util.to_b26(-ref.obj.x1) + ref.obj.y1 + ":"
            + "-" + HN.Util.to_b26(-ref.obj.x2) + ref.obj.y2;
    case "col_from_last":
        return HN.Util.to_b26(ref.obj.x1) + ref.obj.y1 + ":"
            + HN.Util.to_b26(ref.obj.x2) + ref.obj.y2;
    case "col_range_from_last":
        return HN.Util.to_b26(ref.obj.x1) + ref.obj.y1 + ":"
            + HN.Util.to_b26(ref.obj.x2) + ref.obj.y2;
    }
};

HN.Util.arrayToPath = function(arr) {
    var empty = arr.length == 0,
    append    = (empty) ? "" : "/";    
    return arr.join("/") + append;
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

HN.Util.id = function(id) {
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
        return (--num < 26)
            ? String.fromCharCode(num+65)
            : f(Math.floor(num / 26)) + f(num%26 + 1);
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

HN.Util.parse_ref = function(ref) {

    var xref = ref.match(/-?[a-z]+/i)[0];
    return {
        "x": (xref[0] === "-") 
            ? -HN.Util.from_b26(xref.slice(1, xref.length)) 
            : HN.Util.from_b26(xref),
        "y": parseInt(ref.match(/-?[0-9]+/)[0])
    };
};

HN.Util.parse_cols = function(col_range)
{
    var cols = col_range.split(":");
    return {
        "x1": HN.Util.from_b26(cols[0]),
        "x2": HN.Util.from_b26(cols[1])
    };
};

HN.Util.parse_rows = function(row_range)
{
    var rows = row_range.split(":");
    return {
        "y1": rows[0],
        "y2": rows[1]
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
        "x1": beginning.x,
        "y1": beginning.y,
        "x2": end.x,
        "y2": end.y
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

HN.Util.range_to_str2 = function(range, sheet)
{
    // Allow binding to column / row ranges
    if( sheet ) { 
        if( range.y1 == 1 && range.y2 == sheet.max.y ) {
            return HN.Util.to_b26(range.x1) + ":" + 
                HN.Util.to_b26(range.x2);
        }
        else if( range.x1 == 1 && range.x2 == sheet.max.x ) {
            return HN.Util.to_b26(range.y1) + ":" + 
                HN.Util.to_b26(range.y2);
        }

    }

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
    var url = path + HN.Util.to_b26(start) + ":" + HN.Util.to_b26(end);
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
    var nameEQ = name + "=",
    ca         = document.cookie.split(';');

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

HN.Util.refIterator = function(r) {

    var public = {};

    var cmpx = ((r.x2 >= r.x1)
                ? function(current, last) { return current > last; }
                : function(current, last) { return current < last; });
    
    var cmpy = ((r.y2 >= r.y1)
                ? function(current, last) { return current > last; }
                : function(current, last) { return current < last; });

    var incx = (r.x2 >= r.x1) ? 1 : -1;
    var incy = (r.y2 >= r.y1) ? 1 : -1;
    
    var startCol   = r.x1;
    var currentCol = r.x1;
    var currentRow = r.y1;

    var lastCol = r.x2;
    var lastRow = r.y2;
    
    public.nextRow = function() {
        currentCol = startCol;
        if( cmpy(currentRow, lastRow) ) {
            return false;
        } else {
            var tmp = currentRow;
            currentRow += incy;
            return tmp;
        }
    };

    public.nextCol = function() {
        if( cmpx(currentCol, lastCol) ) {
            return false;
        } else {
            var tmp = currentCol;
            currentCol += incx;
            return tmp;
        }
    };

    public.map = function(fun) {
        var y,x;
        while(y = public.nextRow()) {
            while(x = public.nextCol()) {
                fun(y, x);
            }
        }
    };

    return public;
};

HN.Util.is_inside = function(obj, parent) {
    return ( obj == parent ) ||
        ( obj.parentNode != null &&
          HN.Util.is_inside(obj.parentNode, parent) );
};

HN.Util.make_relative = function(currentpage, page) {

    var ret = "";
    if (currentpage === page) {
        return "";
    } else {
        // first break up the paths
        var c_bits = currentpage.split("/");
        c_bits = c_bits.slice(1, c_bits.length - 1);
        var p_bits = page.split("/");
        p_bits = p_bits.slice(1, p_bits.length - 1);
        // each array starts and ends with a ghost blank element
        // trim them off
        var ret_bits = p_bits;
        var c_len = c_bits.length;
        var p_len = p_bits.length;
        for (var i = 0; i <= p_len; i += 1) {
            if (!c_bits[i]) {
                return "./" + ret_bits.join("/") + "/";
            } else if (!p_bits[i]) {
                return HN.Util.repeat_string("../", c_len - p_len);
            } else if (c_bits[i] == p_bits[i]) {
                ret_bits = ret_bits.slice(1, p_bits.length); 
                // discard first element
            } else if (c_bits[i] !== p_bits[i]) {
                return HN.Util.repeat_string("../", c_len - i) + 
                    ret_bits.join("/") + "/";
            }
        }
        return "../" + ret_bits.join("/") + "/";
    }
};

HN.Util.repeat_string = function(s, n) {
  var ret = "";
  for (var i = 0; i < n; i++)
    ret += s;
  return ret;
};

HN.Util.parse_path_and_ref = function(value) {
    var bits = value.split("/");
    if (bits.length === 1) {
        return {'path': "", 'ref': bits[0]};
    } else {
        var path = bits.slice(0, bits.length - 1).join("/") +"/";
        var ref = bits[bits.length - 1];
        return {'path': path, 'ref': ref};
  }
};

HN.Util.is_char = function(key)
{
    return ((key >= 41) &&
            (key != 46) &&
            (key != 91) &&
            (111 >= key || key >= 124) &&
            (key != 224));
};

HN.Util.relToAbsPath = function(base, path) 
{
    if( path.charAt(0) == "/" ) {
        return path;
    }

    var blank = function(x) { return x !== ""; };
    
    path = $.grep(path.split("/"), blank);
    base = $.grep(base.split("/"), blank);
    
    for ( var i = 0; i < path.length; i++ ) {
        if( path[i] == ".." ) {
            if( base.length > 0 ) {
                base.length--;
            }
        } else if ( path[i] !== "." ) {
            base.push(path[i]);
        }
    }
    
    return "/"
        + base.join("/") 
        + ((base.length > 0) ? "/" : "");
};

HN.Util.listToPath = function(list) { 
    return "/"
        + list.join("/") 
        + ((list.length > 0) ? "/" : "");    
};

HN.Util.pathToList = function(path) { 
    return $.grep(path.split("/"), function(a) { return a != ""; });
};

HN.Util.correctPath = function(path) { 

    path        = path.split("/");
    var newPath = [];
    for( var i in path ) {
        var p = path[i].split(' ').join('_')
            .toLowerCase().replace(/[^a-zA-Z 0-9_-]+/g,'');
        if( p != "" ) {
            newPath.push(p);
        }
    }
    
    return HN.Util.listToPath( newPath );
};

HN.Util.logout = function() {
    var ret = escape(window.location.href);
    window.location.href = "/_logout/?return="+ret;
};

HN.Util.localStorage = function() {
    return ('localStorage' in window) && window['localStorage'] !== null;
};

HN.Util.previewMedia = function(val) {
    // Boiler place for previewing stuff that we cant embed in the
    // spreadsheet
    if (val.substr(0, 7) === "<iframe") {

        if (val.match("maps.google.com")) {
            var ll   = val.match(".*ll=([0-9,-\.]*).*")[1];
                var zoom = val.match(".*z=([0-9]*).*")[1];
            return "<img src='http://maps.google.com/maps/api/staticmap"
                +"?center=" + ll + "&zoom=" + zoom + "&size=512x512"
                +"&maptype=roadmap&sensor=false' />";
        } else {
            return "preview";
        }
    }
    return val;
};