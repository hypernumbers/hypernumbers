/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 100000 */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, layout: false */
/**
 * @class HN.Util
 *
 */
HN.namespace("Util");

HN.Util.bounded = function (x, min, max) {
   if (x < min) {
       return min;
   } else if (x > max) {
       return max;
   } else {
       return x;
   }
};

HN.Util.intersect = function (x, y, obj) {
    if ((x >= obj.x1) &&
        (x <= obj.x2) &&
        (y >= obj.y1) &&
        (y <= obj.y2)) {
        return true;
    } else {
        return false;
    };
}

HN.Util.snapWidth = function (val) {
    var noquarters;
    noquarters = Math.floor(val/20); // one quarter of a cell width
    if (noquarters === 0) {
        return 20;
    } else {
        return noquarters * 20;
    };
}

HN.Util.snapHeight = function (val) {
    var nohalfs;
    nohalfs = Math.floor(val/11); // one half of a cell heights
    if (nohalfs === 0) {
        return 11;
    } else {
        return nohalfs * 11;
    };
}

HN.Util.showDeadBox = function (msg) {
    
    $("body")
        .append("<div id='cover'> </div>")
        .append("<div id='dialog'>" + msg + "</div>");
};

HN.Util.showDialog = function (msg) {
        
    var close = $("<form id='closedialog'>" +
                  "<input value='ok' class='button' " +
                  "type='submit' />" +
                  "</form>")
        .bind("submit", function (e) {
            e.preventDefault();
            $("#cover,#dialog").remove();
        });
    
    $("body")
        .append("<div id='cover'> </div>")
        .append("<div id='dialog'>" + msg + "</div>");
    
    $("#dialog").append(close);
};

HN.Util.objToHTML = function (obj, type, index) {
    var html = "<div data-index='" + index + "' data-type='" + type + "'>",
    length = obj.length,
    x;
    for (x in obj) {
        if (x !== length) {
            html += x +
                " : " + obj[x] + "<br />";
        } else {
            html += x + " : " + obj[x];
        }
    }
    html += "</div>";
    return html;
};

HN.Util.htmlEncode = function (string)
{
    var el = document.createElement("div");
    el.innerText = el.textContent = string;
    string = el.innerHTML;
    return string;
};

HN.Util.setPassword = function (password) {
    HN.Callbacks.setPassword(password, function (data) {
                                 if ($("#hn_passwordval").val() === "") {
                                     return;
                                 }
                                 if (data.result === "success") { 
                                     $("#hn_pwd_feedback").empty()
                                         .append("<div class='success'>Password Set" +
                                                 "</div>");
                                 } else { 
                                     $("#hn_pwd_feedback").empty()
                                         .append("<div class='error'>Error: " + data.failure +
                                                 "</div>");
                                 }
                                 $("#hn_passwordval").val("");
                                 setTimeout(function () { 
                                                $("#hn_pwd_feedback").empty();
                                            }, 2000);
                             });
};


HN.Util.invertColor = function (R, G, B) {
    return 0.213 * R +
        0.715 * G +
        0.072 * B < 0.5 ? '#FFF' : '#000';
};  

HN.Util.makeAllowedViews = function () {
    var html  = "",
    sep   = "<hr color='#dddddd' background-color='#dddddd' />",
    v,
    ref = HN.Util.parseRef(document.location.pathname.toLowerCase()),
    path = ref.path,
    views = hn.data.readViews(path),
    hasSpreadsheet = false,
    hasWebpage = false,
    hasWikipage = false,
    permissions =  hn.data.readPermissions(path),
    thispage = $("body").attr("data-view");
    if (views.length === 0) {
        return sep + "You are not allowed to see any views of this page" + sep;
    } else {
        html += sep + "You can view this page as a:<ul>";
        for (v in views) {
            if (views.hasOwnProperty(v)) {
                html += "<li><a href='./?view=" + views[v] + "'>" +
                    views[v] + "</a></li>";
                if (views[v] === "spreadsheet") {
                    hasSpreadsheet = true;
                } else if (views[v] === "webpage") {
                    hasWebpage = true;
                } else if (views[v] === "wikipage") {
                    hasWikipage = true;
                }
            }
        }
        html += "</ul>" + sep;
        if (hasSpreadsheet && hasWebpage && hasWikipage) {
            html += "Preview: <a href='./?view=webpreview'>Web</a> " +
                "<a href='./?view=wikipreview'>Wiki</a>" + sep;
        } else if (hasSpreadsheet && hasWebpage) {
            html += "Preview: <a href='./?view=webpreview'>Web</a>" + sep;
        } else if (hasSpreadsheet && hasWikipage) {
            html += "Preview: <a href='./?view=wikipreview'>Wiki</a>" + sep;
        }
    }
    // now change the hypernumbers tab if the page is public
    if (permissions.views[thispage].everyone) {
        $(".ctrlbox").css("box-shadow", "2px 2px 4px #ffa500");
    }
    return html;
};

HN.Util.makeGroupMenu = function () {
    var groups = HN.Util.remove(hn.groups, "admin").sort(),
    g,
    html = "",
    bits;
    for (g in groups) {
        if (groups.hasOwnProperty(g)) {
            bits = groups[g].split("@");
            if (bits.length === 1) {
                html += "<div>" + groups[g] + "</div>";
            }
        }
    }
    return html;
};

HN.Util.makeAdvPerms = function (permissions) {
    var html   = "",
        view       = {},
        gAndVs     = [],
        v;
    html = "<table></tbody><tr>";
    for (v in permissions.views) {
        if (permissions.views.hasOwnProperty(v)) {
            html += "<td><p>" + HN.Util.capitalise(v) +
                "</p></td><td width='10%'><input type='checkbox' " +
                "`name='addgroupviews' value='" +
            v + "'/></td></tr>";
        }
    }
    html += "</tbody></table>";
    return html;
};

HN.Util.makeSelects = function (list, type, id) {
    var selects = [],
        html      = "",
        i;
    html = "<select id='" + id + "' name='" + type + "'>";
    for (i in list) {
        if (list.hasOwnProperty(i)) {
            html += "<option>" + list[i] + "</options>";
        }
    }
    html += "</select>";
    return html;
};

HN.Util.makeAdvUsers = function () {
    var html   = "",
    groups = [],
    g,
    bits;
    groups = HN.Util.remove(hn.groups, "admin").sort();
    if (groups.length === 0) {
        html = "<p id='groups' class='warning'>(There are no groups created yet)</p>";
    } else {
        html += "<select id='groups' name='groups' multiple>";
        for (g in groups) {
            if (groups.hasOwnProperty(g)) {
                bits = groups[g].split("@");
                if (bits.length === 1) {
                    html += "<option>" + groups[g] + "</option>";
                }
            }
        }
        html += "</select>";
    }
    return html;
};

HN.Util.makePermsTable = function (permissions, path) {
    var html      = "",
        gAndVs    = [],
        g,
        v,
        noOfViews = 0,
        headerrow = "",
        rows      = "",
        views     = {};
    html = "<table cellspacing='0'><tbody>";
    gAndVs = HN.Util.getGroupsAndViews(permissions);
    headerrow = "<tr><td></td><td><p>default</p></td>";
    for (g in gAndVs.groups) {
        if (gAndVs.groups[g] !== "admin") {
            headerrow += "<td>" + HN.Util.shorten(gAndVs.groups[g]) + "</td>";
        }
    }
    headerrow += "<td class='publicoption'>public</td>" +
        "<td></td></tr>";
    for (v in gAndVs.views) {
        if (gAndVs.views.hasOwnProperty(v)) {
            // if the groups length is 1 it means there is only the admin 
            // group in play so don't put the right border on
            if (gAndVs.views.hasOwnProperty(v)) {
                if (gAndVs.groups.length === 1) {
                    rows += "<tr><td class='viewlink'>" +
                        "<a href='http://" + document.location.host + path +
                        "?view=" + v + "'>" +
                        HN.Util.capitalise(v) +
                        "</a></td><td><input type='radio' value='" +
                        v + "' name='newdefaultpage' /></td>";
                } else {
                    rows += "<tr><td class='viewlink'>" +
                        "<a href='http://" + document.location.host + path +
                        "?view=" + v + "'>" +
                        HN.Util.capitalise(v) +
                        "</a></td><td class='defaultoption'>" +
                        "<input type='radio' value='" +
                        v + "' name='newdefaultpage' /></td>";
                }
            }
        }
        for (g in gAndVs.groups) {
            if (gAndVs.groups.hasOwnProperty(g)) {
                if (gAndVs.groups[g] !== "admin") {
                    rows += "<td><input data-perm='" + v + "_" + 
                        gAndVs.groups[g] +
                        "' class='groupinput' type='checkbox' /></td>";
                }
            }
        }
        rows += "<td class='publicoption'>" +
            "<input id='newpublic" + v + "' type='checkbox' data-value='" +
            v + "' /></td></tr>";
    }
    html += headerrow + rows + "</tbody></table>";
    html += "<span id='hn_previewlink'>Preview: </span>" +
        "<a href=http://" + document.location.host + path +
        "?view=webpreview>Web</a> " +
        "<a href=http://" + document.location.host + path +
        "?view=wikipreview>Wiki</a>";
    return html;
};

HN.Util.parseRef = function (ref) {
    
    var list  = ref.split("/"),
        range = list.pop().toUpperCase(),
        addr  = HN.Util.parseAddr(range),
        path;
    
    list.push("");
    path = HN.Util.relToAbsPath(document.location.pathname.toLowerCase(), list.join("/"));

    return (addr === undefined) ?
        { "path": path, "type": "path" } :
        { "path": path, "type": addr.type, "obj": addr.obj };
};

HN.Util.equals = function (obj1, obj2) {
    for (var i in obj1) {
        if (obj1[i] !== obj2[i]) {
            return false;
        }
    }
    return true;
};

HN.Util.isCongruent = function (type1, ref1, type2, ref2) {
    var r1, r2;
    
    if (type1 === "cell" || type1 === "row_from_last" ||
          type1 === "col_from_last") {
        
        return type2 === "cell" ||
            type2 === "row_from_last" ||
            type2 === "col_from_last";
        
    } else if (type1 === "range" || type1 === "row_range_from_last" ||
               type1 === "col_range_from_last") {

        if (type2 === "range" ||
            type2 === "row_range_from_last" ||
            type2 === "col_range_from_last") {

            r1 = HN.Util.parse_range(ref1.toUpperCase());
            r2 = HN.Util.parse_range(ref2.toUpperCase());
            
            return (((r1.x2 - r1.x1) === (r2.x2 - r2.x1)) &&
                     ((r1.y2 - r1.y1) === (r2.y2 - r2.y1)));

        }
    }
    return false;
};

HN.Util.parseAddr = function (ref) {
    var cell                = /^[a-zA-Z]+[0-9]+$/,
        range               = /^[a-zA-Z]+[0-9]+:[a-zA-Z]+[0-9]+$/,
        col_range           = /^[a-zA-Z]+:[a-zA-Z]+$/,
        col_from_last       = /^[a-zA-Z]+\-[0-9]+$/,
        col_range_from_last = /^[a-zA-Z]+\-[0-9]+:[a-zA-Z]+\-[0-9]+$/,
        row_range           = /^[0-9]+:[0-9]+$/,
        row_from_last       = /^\-[a-zA-Z]+[0-9]+$/,
        row_range_from_last = /^\-[a-zA-Z]+[0-9]+:\-[a-zA-Z]+[0-9]$/,
        tmp;
    if (cell.exec(ref) && (cell.exec(ref)[0] === ref)) {
        return {"type": "cell", "obj": HN.Util.parse_ref(ref)};
    } else if (range.exec(ref)   && (range.exec(ref)[0] === ref)) {
        return {"type": "range", "obj": HN.Util.parse_range(ref)};
    } else if (col_range.exec(ref) && (col_range.exec(ref)[0] === ref)) {
        tmp = ref.split(":");
        return {"type": "col_range", "obj": {"x1": HN.Util.from_b26(tmp[0]),
                                            "x2": HN.Util.from_b26(tmp[1])}};
    } else if (col_from_last.exec(ref) &&
               (col_from_last.exec(ref)[0] === ref)) {
        tmp = ref.split(":");
        return {"type": "col_from_last", 
                "obj": {"x1": HN.Util.from_b26(tmp[0]),
                        "x2": HN.Util.from_b26(tmp[1])}};
    } else if (col_range_from_last.exec(ref) &&
               (col_range_from_last.exec(ref)[0] === ref)) {
        return {"type": "col_range_from_last", "obj": HN.Util.parse_range(ref)};
    } else if (row_range.exec(ref) && (row_range.exec(ref)[0] === ref)) {
        tmp = ref.split(":");
        return {"type": "row_range", "obj": {"y1": parseInt(tmp[0], 10),
                                             "y2": parseInt(tmp[1], 10)}};
    } else if (row_from_last.exec(ref) &&
               (row_from_last.exec(ref)[0] === ref)) {
        tmp = ref.split(":");
        return {"type": "row_from_last", "obj": {"y1": parseInt(tmp[0], 10),
                                                 "y2": parseInt(tmp[1], 10)}};
    } else if (row_range_from_last.exec(ref) &&
               (row_range_from_last.exec(ref)[0] === ref)) {
        tmp = ref.split(":");
        return {"type": "row_range_from_last", "obj": HN.Util.parse_range(ref)};
    }
};

HN.Util.refToStr = function (ref) {
    switch (ref.type) {
    case "cell":
        return HN.Util.to_b26(ref.obj.x) + ref.obj.y;
    case "range":
        return HN.Util.to_b26(ref.obj.x1) + ref.obj.y1 + ":" +
            HN.Util.to_b26(ref.obj.x2) + ref.obj.y2;
    case "col_range":
        return HN.Util.to_b26(ref.obj.x1) + ":" + HN.Util.to_b26(ref.obj.x2);
    case "row_range":
        return ref.obj.y1 + ":" + ref.obj.y2;
    case "row_from_last":
        return "-" + HN.Util.to_b26(-ref.obj.x1) + ref.obj.y1 + ":" +
            "-" + HN.Util.to_b26(-ref.obj.x2) + ref.obj.y2;
    case "row_range_from_last":
        return "-" + HN.Util.to_b26(-ref.obj.x1) + ref.obj.y1 + ":" +
            "-" + HN.Util.to_b26(-ref.obj.x2) + ref.obj.y2;
    case "col_from_last":
        return HN.Util.to_b26(ref.obj.x1) + ref.obj.y1 + ":" +
            HN.Util.to_b26(ref.obj.x2) + ref.obj.y2;
    case "col_range_from_last":
        return HN.Util.to_b26(ref.obj.x1) + ref.obj.y1 + ":" +
            HN.Util.to_b26(ref.obj.x2) + ref.obj.y2;
    }
};

HN.Util.arrayToPath = function (arr) {
    var empty = arr.length === 0,
        append    = (empty) ? "" : "/";    
    return arr.join("/") + append;
};

HN.Util.dispatch = function (fun, args) {
    if (typeof fun !== "undefined") {
        fun.apply(this, args);
    }
};

HN.Util.addEvent = function (el, type, fn) {
    $(el).bind(type, fn);
};

HN.Util.removeEvent = function (el, type, fn) {
    $(el).unbind(type, fn);
};

HN.Util.id = function (id) {
    return document.getElementById(id);
};

HN.Util.is_visible = function (elem) {
    return ($(elem).is(':visible') &&
            $(elem).parents(':hidden').length === 0);
};

HN.Util.y_pos = function (obj) {
    var top = 0;
    if (obj.offsetParent) {
        do {
	        top += obj.offsetTop;
        } while ((obj = obj.offsetParent));
    }
    return top;
};

HN.Util.x_pos = function (obj) {
    var left = 0;
    if (obj.offsetParent) {
        do {
	        left += obj.offsetLeft;
        } while ((obj = obj.offsetParent));
    }
    return left;
};

HN.Util.to_b26 = function (cell) {
    function f(num) {
        return (--num < 26) ?
            String.fromCharCode(num + 65) :
            f(Math.floor(num / 26)) + f(num % 26 + 1);
    }

    if (typeof cell === "number") {
        return f(cell);
    }

    throw "Not a number";
};

HN.Util.from_b26 = function (cell) {
    for (var i = cell.length, pow = 0, sum = 0; i > 0; i--) {
        sum += Math.round((cell.charCodeAt(i - 1) -  64) * Math.pow(26, pow++));
    }
    return sum;
};

HN.Util.parse_ref = function (ref) {

    var xref = ref.match(/-?[a-z]+/i)[0];
    return {
        "x": (xref[0] === "-") ?
            -HN.Util.from_b26(xref.slice(1, xref.length)) :
            HN.Util.from_b26(xref),
        "y": parseInt(ref.match(/-?[0-9]+/)[0], 10)
    };
};

HN.Util.parse_cols = function (col_range) {
    var cols = col_range.split(":");
    return {
        "x1": HN.Util.from_b26(cols[0]),
        "x2": HN.Util.from_b26(cols[1])
    };
};

HN.Util.parse_rows = function (row_range) {
    var rows = row_range.split(":");
    return {
        "y1": rows[0],
        "y2": rows[1]
    };
};

HN.Util.parse_range = function (range) {
    var cells = range.split(":"),
    beginning = HN.Util.parse_ref(cells[0]),
    end       = (cells.length > 1) ?
         HN.Util.parse_ref(cells[1]) :
        beginning;

    return {
        "x1": beginning.x,
        "y1": beginning.y,
        "x2": end.x,
        "y2": end.y
    };
};

HN.Util.coord_to_ref = function (cell) {
    return HN.Util.to_b26(cell.x) + cell.y;
};

HN.Util.clone = function (obj) {
    var newObj = (obj instanceof Array) ? [] : {},
    i;
    for (i in obj) {
        if (obj.hasOwnProperty(i)) {
            if (i === 'clone') {
                continue;
            }
            if (obj[i] && typeof obj[i] === "object") {
                newObj[i] = obj[i].clone();
            } else {
                newObj[i] = obj[i];
            }
        }
    }
    return newObj;
};

HN.Util.range_to_str = function (range) {
    return HN.Util.coord_to_ref({"x": range.x1, "y": range.y1}) + ":" +
        HN.Util.coord_to_ref({"x": range.x2, "y": range.y2});
};

HN.Util.range_to_str2 = function (range, sheet) {
    // Allow binding to column / row ranges
    if (sheet) { 
        if (range.y1 === 1 && range.y2 === sheet.max.y) {
            return HN.Util.to_b26(range.x1) + ":" + 
                HN.Util.to_b26(range.x2);
        }
        else if (range.x1 === 1 && range.x2 === sheet.max.x) {
            return HN.Util.to_b26(range.y1) + ":" + 
                HN.Util.to_b26(range.y2);
        }

    }

    if ((range.x1 === range.x2) && (range.y1 === range.y2)) {
        return HN.Util.coord_to_ref({"x": range.x1, "y": range.y1});
    } else {
        return HN.Util.coord_to_ref({"x": range.x1, "y": range.y1}) + ":" +
            HN.Util.coord_to_ref({"x": range.x2, "y": range.y2});
    }
};

HN.Util.postCell = function (path, cell, json) {
    var url = path + HN.Util.coord_to_ref(cell);
    $.post(url, JSON.stringify(json), null, "json");
};

HN.Util.postPath = function (path, json, cb) {
    $.post(path, JSON.stringify(json), cb, "json");
};

HN.Util.postColumn = function (path, start, end, json) {
    var url = path + HN.Util.to_b26(start) + ":" + HN.Util.to_b26(end);
    $.post(url, JSON.stringify(json), null, "json");
};

HN.Util.postRow = function (path, start, end, json) {
    var url = path + start + ":" + end;
    $.post(url, JSON.stringify(json), null, "json");
};

HN.Util.postRange = function (path, range, json) {
    $.post(path + HN.Util.range_to_str(range),
           JSON.stringify(json), null, "json");
};

HN.Util.readCookie = function (name) {
    var nameEQ = name + "=",
        ca     = document.cookie.split(';'),
        i, c;

    for (i = 0; i < ca.length; i = i + 1) {
	    c = ca[i];
	    while (c.charAt(0) === ' ') {
            c = c.substring(1, c.length);
        }
	    if (c.indexOf(nameEQ) === 0) {
            return c.substring(nameEQ.length + 1, c.length - 1);
        }
    }
    return null;
};

HN.Util.createCookie = function (name, value, days) {
    var date,
        expires;
    
    if (days) {
	    date = new Date();
	    date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
	    expires = "; expires=" + date.toGMTString();
    } else {
        expires = "";
    }
    document.cookie = name + "=" + value + expires + "; path=/";
};

HN.Util.eraseCookie = function (name) {
	  HN.Util.createCookie(name, "", -1);
};

HN.Util.refIterator = function (r) {

    var api = {},

    cmpx = ((r.x2 >= r.x1) ?
            function (current, last) {
                return current > last;
            } :
            function (current, last) {
                return current < last;
            }),
    
    cmpy = ((r.y2 >= r.y1) ?
            function (current, last) {
                return current > last;
            } :
            function (current, last) {
                return current < last;
            }),
    
    incx = (r.x2 >= r.x1) ? 1 : -1,
    incy = (r.y2 >= r.y1) ? 1 : -1,
    
    startCol   = r.x1,
    currentCol = r.x1,
    currentRow = r.y1,

    lastCol = r.x2,
    lastRow = r.y2;
    
    api.nextRow = function () {
        currentCol = startCol;
        if (cmpy(currentRow, lastRow)) {
            return false;
        } else {
            var tmp = currentRow;
            currentRow += incy;
            return tmp;
        }
    };

    api.nextCol = function () {
        if (cmpx(currentCol, lastCol)) {
            return false;
        } else {
            var tmp = currentCol;
            currentCol += incx;
            return tmp;
        }
    };

    // Dunno what this function does but it appears not be used
    // Gordon Guthrie 25/11/2010
    // jslint borks at the assignments and so do I
    /*api.map = function (fun) {
        var y,x;
        while(y = api.nextRow()) {
            while(x = api.nextCol()) {
                fun(y, x);
            }
        }
    };*/

    return api;
};

HN.Util.is_inside = function (obj, parent) {
    return (obj === parent) ||
        (obj.parentNode !== null &&
          HN.Util.is_inside(obj.parentNode, parent));
};

HN.Util.make_relative = function (currentpage, page) {

    var ret = "",
    c_bits, p_bits, ret_bits, c_len, p_len, i;
    if (currentpage === page) {
        return "";
    } else {
        // first break up the paths
        c_bits = currentpage.split("/");
        c_bits = c_bits.slice(1, c_bits.length - 1);
        p_bits = page.split("/");
        p_bits = p_bits.slice(1, p_bits.length - 1);
        // each array starts and ends with a ghost blank element
        // trim them off
        ret_bits = p_bits;
        c_len = c_bits.length;
        p_len = p_bits.length;
        for (i = 0; i <= p_len; i += 1) {
            if (!c_bits[i]) {
                return "./" + ret_bits.join("/") + "/";
            } else if (!p_bits[i]) {
                return HN.Util.repeat_string("../", c_len - p_len);
            } else if (c_bits[i] === p_bits[i]) {
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

HN.Util.repeat_string = function (s, n) {
    var ret = "",
        i;
    
    for (i = 0; i < n; i = i + 1) {
        ret += s;
    }
    return ret;
};

HN.Util.parse_path_and_ref = function (value) {
    var bits = value.split("/"),
    path = "",
    ref = "";
    if (bits.length === 1) {
        return {'path': "", 'ref': bits[0]};
    } else {
        
        path = bits.slice(0, bits.length - 1).join("/") + "/";
        ref = bits[bits.length - 1];
        return {'path': path, 'ref': ref};
    }
};

HN.Util.is_char = function (key)
{
    return ((key >=  41) &&
            (key !== 46) &&
            (key !== 91) &&
            (111 >=  key || key >= 124) &&
            (key !== 224));
};

HN.Util.relToAbsPath = function (base, path) 
{
    var blank, i;
    if (path.charAt(0) === "/") {
        return path;
    }

    blank = function (x) {
        return x !== "";
    };
    
    path = $.grep(path.split("/"), blank);
    base = $.grep(base.split("/"), blank);
    
    for (i = 0; i < path.length; i = i + 1) {
        if (path[i] === "..") {
            if (base.length > 0) {
                base.length = base.lenght - 1;
            }
        } else if (path[i] !== ".") {
            base.push(path[i]);
        }
    }
    
    return "/" +
        base.join("/") +
        ((base.length > 0) ? "/" : "");
};

HN.Util.listToPath = function (list) { 
    return "/" +
        list.join("/") +
        ((list.length > 0) ? "/" : "");    
};

HN.Util.pathToList = function (path) { 
    return $.grep(path.split("/"), function (a) {
        return a !== "";
    });
};

HN.Util.correctPath = function (path) { 
    var newPath, i, p;
    path        = path.split("/");
    newPath = [];
    for (i = 0; i < path.length; i = i + 1) {
        p = path[i].split(' ').join('_')
            .toLowerCase().replace(/[^a-zA-Z 0-9_\-]+/g, '');
        if (p !== "") {
            newPath.push(p);
        }
    }
    
    return HN.Util.listToPath(newPath);
};

HN.Util.logout = function () {
    var ret = escape(window.location.href);
    window.location.href = "/_logout/?return=" + ret;
};

HN.Util.localStorage = function () {
    return ('localStorage' in window) && window.localStorage !== null;
};

HN.Util.makeWikiPreview = function (value) {
    var val;
    if (value) {
        val = value;
    } else {
        val = "&nbsp;";
    }
    return "<span class='inlineform'></span>" +
        "<div class='hn-wikiselect-preview'><div class='hn-wiki-img'>" + 
        val + "</div></div>";
};

HN.Util.makeWikiSelect = function (input, selection, ref, uniqid) {
    var opts = "", 
    start, x, list;
    if (input.select) {
        list = input.select;
    } else if (input.values) {
        list = input.values;
    }
    for (x in list) {
        if (list.hasOwnProperty(x)) {
            if (list[x].toString() === selection) {
                start = "<option selected>";
            } else {
                start = "<option>";
            }
        }
        opts += start + list[x].toString() + "</option>";
    }
    return "<div><select class='hn_inlineselect hn_inlineselect_" + uniqid +
        "' data-ref='" + ref + "'>" +
        opts + "</select></div>";
};

HN.Util.getPreview = function (cell) {
    if (cell.preview) {
        return "<div class='hn_html_preview hn-preview-wd-" + cell.preview.w +
            " hn-preview-ht-" + cell.preview.h +
            "'>" + cell.preview.txt + "</div>";
    } else if (cell.value) {
        return cell.value;
    } else {
        return "";
    }
};

HN.Util.renderPreview = function (str) {

    var width = str.match("width=\"([0-9]*)\""),
    div, wd, hg;

    if (width) {
    
        wd  = str.match("width=\"([0-9]*)\"")[1];
        hg  = str.match("height=\"([0-9]*)\"")[1];
        div = "<div style='height:" + hg + "px;width:" + wd + "px;" +
            "background:#EEE;'><span style='padding:5px;display:block;" +
            "line-height:10px;'>" +
            "<img src='/img/film.png' style='position:relative; top:4px'/>" +
            "&nbsp;preview</span></div>";
    } else {
        div = "<div style='background:#EEE;height:100%;'><span style='padding:5px;" +
            "display:block;line-height:10px;'>" +
            "<img src='/img/film.png' style='position:relative; top:4px'/>" +
            "&nbsp;preview</span></div>";
    }
    return div;
};

HN.Util.initReset = function () {
    var params = HN.Util.getQueryParams(),
        resetPassword,
        doReset;
    if (params.reset) {
        $(".newpwd").css("display", "");
        $("#resettext").html("Please set your new password.");
        resetPassword = function (e) {
            e.preventDefault();
            var email = $("#requestingemail").val(),
                newpwd = $("#newpassword").val(),
                data = {"email"  : email,
                       "newpwd" : newpwd,
                       "hash"   : params.reset};        
            $.ajax({
                "type"     : "POST",
                "url"      : "/_forgotten_password/",
                "data"     : JSON.stringify(data),
                "dataType" : "json",
                "success"  : function (data) {
                    if (data.status === "success") {
                        $("#resetmsg").html(data.response).removeClass("error");
                    } else {
                        $("#resetmsg").html(data.response).addClass("error");
                    }
                },
                "error"    : function (data) {
                    var msg = "<p>An error has occured. " +
                        "It has been logged. Please try again.</p>";
                    $("#resetmsg").html(msg).addClass("error");
                }
            });
        };
        $("#reset").submit(resetPassword);
    } else {
        $("#resettext").html("Please enter your e-mail address.");
        doReset = function (e) {
            e.preventDefault();
            var email = $("#requestingemail").val(),
                site = HN.Util.getQueryParams().site,
                data = {"email": email, "site": site};
            $.ajax({
                "type"     : "POST",
                "url"      : "/_forgotten_password/",
                "data"     : JSON.stringify(data),
                "dataType" : "json",
                "success"  : function (data) {
                    if (data.status === "success") {
                        $("#resetmsg").html(data.response).removeClass("error");
                    } else {
                        $("#resetmsg").html(data.response).addClass("error");
                    }
                },
                "error"    : function (data) {
                    var msg = "<p>An error has occured. " +
                        "It has been logged. Please try again.</p>";
                    $("#resetmsg").html(msg).addClass("error");
                }
            });
        };
        $("#reset").submit(doReset);
    }
};

HN.Util.initLogin = function (type) {
    var cookie = HN.Util.readCookie("auth"),
        user   = HN.Util.parseEmail(cookie),
        site   = document.location,
        doLogin;
    $("#forgotten_pwd").html("<a href=\"http://hypernumbers.com/_forgotten_password/?site=" +
                             site + "\">forgotten password?</a>");
    
    doLogin = function (e) {

        e.preventDefault();
        
        var email = $("#email").val(),
            pass  = $("#pass").val(),
            rem   = true;
        
        $("#loginfeedback").html("");
        $("#submit").attr("disabled", "disabled").val("Loading");
        
        HN.Util.login(email, pass, rem, function (result) {
                  
            if (result.error) {
                $("#submit").removeAttr("disabled").val("Log In");
                $("#loginfeedback").html("<strong>error:</strong> " + result.error);
            } else {
                window.location.reload(true);
            }
        });
    };    

    setTimeout(function () {
        if (user !== "anonymous" && type !== "spreadsheet") {
            // fugly for 401/404 pages
            if (typeof(hn) !== "undefined") {
                var html = HN.Util.makeAllowedViews();
                $("#allowedviews").html(HN.Util.makeAllowedViews());
            }
            $("#uname").text(user);
            $("#editloggedin").show();
            $("#hn_reset_pwd").click(function (e) {
                $("#hn_passwordform").css("display", "block");
            });
            $("#hn_passwordform").bind("submit", function (e) {
                e.preventDefault();
                HN.Util.setPassword($("#hn_passwordval").val());
            });
            $("#logout").attr("href", "/_logout/?return=" +
                              escape(window.location.href));
            
        } else {
            $("#editanon").show();
            $("#login").submit(doLogin);
        }
        $("#powered").bind("mouseover", function () {
            // fugly bodge - layout only exists on the spreadsheet
            if (typeof(layout) !== "undefined") {
                layout.grabFocus();
            }
            $("#editspreadsheet").addClass("active");
            $("#editingmenu").show();
            $("#email").focus();
            var f = function (e) {
                if (!HN.Util.is_inside(e.target,
                                       $("#editspreadsheet")[0])) {
                    $(document).unbind("mousedown", f);
                    $("#editingmenu").hide();
                    $("#editspreadsheet").removeClass("active");
                }
            };
            $(document).bind("mousedown", f);
        });
        
        $("#editspreadsheet").fadeIn("slow");
    }, 1000);
    
};


HN.Util.readCookie = function (name) {
    var nameEQ = name + "=",
    ca = document.cookie.split(';'),
    i,
    c;
    
    for (i = 0; i < ca.length; i = i + 1) {
	      c = ca[i];
	      while (c.charAt(0) === ' ') {
            c = c.substring(1, c.length);
	      }
        if (c.indexOf(nameEQ) === 0) {
            return c.substring(nameEQ.length, c.length);
        }
    }
    return null;
};

HN.Util.parseEmail = function (cookie) {
    var email = cookie.split("|")[0];
    email = email.replace("!", "@");
    email = email.replace("#", "+");
    return email;
};

HN.Util.login = function (email, pass, remember, cb) {
    
    var data = {"email": email, "pass": pass, "remember": remember};
    
    if (pass !== "" && email !== "") {
        
        $.ajax({
            "type"     : "POST", 
            "url"      : "/_login/?return=" + escape(window.location.href),
            "data"     : JSON.stringify(data), 
            "dataType" : "json",
            "success"  : function (data) {
                if (data.response === "error") {
                    cb({"error" : "Sorry, those credentials were not found"});
                } else {
                    cb({"success" : true});
                }
            },
            "error"    : function (data) {
                cb({"error" : "Sorry, there was an error processing your " +
                    "login, please try again" });
            }
        });
        
    } else {
        cb({"error" : "Please enter full details"});
    }
};

HN.Util.getCrumbTrail = function () {
    var path = HN.Util.pathToList(document.location.pathname.toLowerCase()),
        bits = document.location.host.split(":"),
        host = bits[0],
        port = bits[1],
        acc = "",
        ret = "<a href=\"http://" + document.location.host + "\">" + host + "</a>",
        i;
    for (i = 0; i < path.length; i = i + 1) {
        acc = acc + path[i]  + "/";
        ret += " -> <a href=\"http://" + document.location.host + "/" +
            acc + "\">" + path[i] + "</a>";
    }
    return ret;
};

HN.Util.getQueryParams = function () {
    var queries = {};
 
    // Use the String::replace method to iterate over each
    // name-value pair in the query string. Location.search
    // gives us the query string (if it exists).
    window.location.search.replace(
        new RegExp("([^?=&]+)(=([^&]*))?", "g"),
  
        // For each matched query string pair, add that
        // pair to the URL struct using the pre-equals
        // value as the key.
        function ($0, $1, $2, $3) {
            queries[$1] = $3;
        }
    );
    return queries;
};

HN.Util.capitalise = function (string) {
    var pattern = /(\w)(\w*)/, // first letter
        parts = string.match(pattern),
        firstLetter = parts[1].toUpperCase(),
        restOfWord = parts[2].toLowerCase();
    
    return firstLetter + restOfWord; 

};

HN.Util.dedup = function (array) {
    var ret = [],
    i;
    array = array.sort();
    ret[0] = array[0];
    for (i = 1; i < array.length; ++i) {
        if (ret[ret.length - 1]  !== array[i]) {
            ret.push(array[i]);
        }
    }
    return ret;
};

HN.Util.remove = function (array, item) {
    var ret = [],
        i;
    for (i = 0; i < array.length; i = i + 1) {
        if (array[i] !== item) {
            ret.push(array[i]);
        }
    }
    return ret;
};

HN.Util.getGroupsAndViews = function (permissions) {
    var data = {},
        groups = [],
        views  = [],
        v;
    for (v in permissions.views) {
        if (permissions.views.hasOwnProperty(v)) {
            views[v] = permissions.views[v].groups;
            groups = $.merge(groups, permissions.views[v].groups);
        }
    }
    data.views = views;
    data.groups = HN.Util.dedup(groups);
    return data;
};

HN.Util.getParams = function () {
    var vars = [],
        hash,
        i,
        hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
    for (i = 0; i < hashes.length; i = i + 1)
    {
        hash = hashes[i].split('=');
        vars.push(hash[0]);
        vars[hash[0]] = hash[1];
    }
    return vars;
};

HN.Util.shorten = function (name) {
    var bits = name.split("@");
    switch (bits.length) {
    case 1: 
        return "<p>" + name + "</p>";
    case 2 : 
        return "<p title='" + name + "'><i>" + bits[0] + "&nbsp;(u)</i></p>";
    }
};

HN.Util.trimarray = function (array) {
    var ret = [],
    x;
    for (x in array) {
        if (array.hasOwnProperty(x)) {
            ret[x] = HN.Util.trim(array[x]);
        }
    }
    return ret;
};

HN.Util.trim = function (str) {
    return str.replace(/^\s\s*/, '').replace(/\s\s*$/, '');
};

HN.Util.setupInlineSelect = function (displaytype, uniqid) {
    // set up the inline select boxes
    var incs = $(".hn_inlineselect_" + uniqid),
    fun, cell;
    fun = function () {
        cell = HN.Util.parseRef($(this).attr("data-ref"));
        if (displaytype === "spreadsheet") {
            HN.Callbacks.set_cell(cell.path, cell.obj, $(this).val());
        } else if (displaytype === "wikipage") {
            HN.Callbacks.inline_input(cell.path, cell.obj, $(this).val());
        }
    };
    incs.bind("change", fun);
};

HN.Util.get_uniq_id = function () {
    var newDate = new Date();
    return newDate.getTime().toString();
};

HN.Util.showDeveloper = function() {
    HN.Util.show("showdeveloper", ".hn_developer");
}

HN.Util.showAdvanced = function () {
    HN.Util.show("showadvanced", ".hn_advanced");
};

HN.Util.showHidden = function () {
    HN.Util.show("showhidden", ".hn_hidden");
};

HN.Util.show = function (cookiename, selector) {
    var cookie = HN.Util.readCookie(cookiename);
    if (!(cookie && cookie === "true")) {
        $(selector).css("display", "none");
    } else {
        $(selector).css("display", "");
    }
};

//	Blog Entry:
//	Ask Ben: Parsing CSV Strings With Javascript Exec() Regular Expression Command
//	Author:
//	Ben Nadel / Kinky Solutions
//	
//	Link:
//	http://www.bennadel.com/index.cfm?event=blog.view&id=1504	
//
//	Date Posted:
//	Feb 19, 2009 at 10:03 AM
	
	// This will parse a delimited string into an array of
	// arrays. The default delimiter is the comma, but this
	// can be overriden in the second argument.
HN.Util.CVSToArray = function (strData, strDelimiter) {
		// Check to see if the delimiter is defined. If not,
		// then default to comma.
		strDelimiter = (strDelimiter || ",");
 
		// Create a regular expression to parse the CSV values.
		var objPattern = new RegExp(
			(
				// Delimiters.
				"(\\" + strDelimiter + "|\\r?\\n|\\r|^)" +
 
				// Quoted fields.
				"(?:\"([^\"]*(?:\"\"[^\"]*)*)\"|" +
 
				// Standard fields.
				"([^\"\\" + strDelimiter + "\\r\\n]*))"
			),
			"gi"
			);
 
 
		// Create an array to hold our data. Give the array
		// a default empty first row.
		var arrData = [[]];
 
		// Create an array to hold our individual pattern
		// matching groups.
		var arrMatches = null;
 
 
		// Keep looping over the regular expression matches
		// until we can no longer find a match.
		while (arrMatches = objPattern.exec( strData )){
 
			// Get the delimiter that was found.
			var strMatchedDelimiter = arrMatches[ 1 ];
 
			// Check to see if the given delimiter has a length
			// (is not the start of string) and if it matches
			// field delimiter. If id does not, then we know
			// that this delimiter is a row delimiter.
			if (
				strMatchedDelimiter.length &&
				(strMatchedDelimiter != strDelimiter)
				){
 
				// Since we have reached a new row of data,
				// add an empty row to our data array.
				arrData.push( [] );
 
			}
 
 
			// Now that we have our delimiter out of the way,
			// let's check to see which kind of value we
			// captured (quoted or unquoted).
			if (arrMatches[ 2 ]){
 
				// We found a quoted value. When we capture
				// this value, unescape any double quotes.
				var strMatchedValue = arrMatches[ 2 ].replace(
					new RegExp( "\"\"", "g" ),
					"\""
					);
 
			} else {
 
				// We found a non-quoted value.
				var strMatchedValue = arrMatches[ 3 ];
 
			}
 
			// Now that we have our value string, let's add
			// it to the data array.
			arrData[ arrData.length - 1 ].push( strMatchedValue );
		}
 
		// Return the parsed data.
		return( arrData );
	};