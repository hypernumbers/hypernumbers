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

HN.Util.addEvent = function(el, type, fn) {
    $(el).bind(type, fn);
};
HN.Util.removeEvent = function(el, type, fn) {
    $(el).unbind(type, fn);
};

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
    
    // Boiler plate for previewing stuff that we cant embed in the
    // spreadsheet
    if (val.substr(0, 7) === "<iframe") {

        if (val.match("maps.google.com")) {
            var ll   = val.match(".*ll=([0-9,-\.]*).*")[1];
            var zoom = val.match(".*z=([0-9]*).*")[1];
            return "<img src='http://maps.google.com/maps/api/staticmap"
                +"?center=" + ll + "&zoom=" + zoom + "&size=512x512"
                +"&maptype=roadmap&sensor=false' />";
        } else if (val.match("facebook.com")) {
            return "<div><img src='/img/media.png' style='position:relative; "
                +"top:4px'/>&nbsp;facebook preview</span></div>";
;
        } else {
            return HN.Util.renderPreview(val);
        }
    } else if (val.substr(0, 4) === "<obj") {
        return HN.Util.renderPreview(val);
    }
    
    return val;
};

HN.Util.renderPreview = function(str) {

    var width = str.match("width=\"([0-9]*)\"");

    if (width) {
    
        var wd  = str.match("width=\"([0-9]*)\"")[1],
            hg  = str.match("height=\"([0-9]*)\"")[1],
            div = "<div style='height:" + hg + "px;width:" + wd + "px;"
            + "background:#EEE;'><span style='padding:5px;display:block;"
            + "line-height:10px;'>"
            + "<img src='/img/film.png' style='position:relative; top:4px'/>"
            + "&nbsp;preview</span></div>";
    } else {
        var div = "<div style='background:#EEE;height:100%;'><span style='padding:5px;"
            + "display:block;line-height:10px;'>"
            + "<img src='/img/film.png' style='position:relative; top:4px'/>"
            + "&nbsp;preview</span></div>";
    }
    return div;
};

HN.Util.initReset = function () {
    var params = HN.Util.getQueryParams();
    if (params.reset) {
        $(".newpwd").css("display", "");
        $("#resettext").html("Please set your new password.");
        var resetPassword = function(e) {
            e.preventDefault();
            var email = $("#requestingemail").val();
            var newpwd = $("#newpassword").val();
            var data ={"email"  : email,
                       "newpwd" : newpwd,
                       "hash"   : params.reset};        
            $.ajax({
                       "type"     : "POST",
                       "url"      : "/_forgotten_password/",
                       "data"     : JSON.stringify(data),
                       "dataType" : "json",
                       "success"  : function (data) {
                           console.log(data);
                           if (data.status === "success") {
                               $("#resetmsg").html(data.response).removeClass("error");
                           } else {
                               $("#resetmsg").html(data.response).addClass("error");
                           };
                       },
                       "error"    : function (data) {
                           var msg = "<p>An error has occured. " 
                               + "It has been logged. Please try again.</p>";
                           $("#resetmsg").html(msg).addClass("error");
                       }
                   });
        };
        $("#reset").submit(resetPassword);
    } else {
        $("#resettext").html("Please enter your e-mail address.");
        var doReset = function(e) {
            e.preventDefault();
            var email = $("#requestingemail").val();
            var site = HN.Util.getQueryParams().site;
            var data = {"email": email, "site": site};
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
                           };
                       },
                       "error"    : function (data) {
                           var msg = "<p>An error has occured. " 
                               + "It has been logged. Please try again.</p>";
                           $("#resetmsg").html(msg).addClass("error");
                       }
                   });
        };
        $("#reset").submit(doReset);
    };
};

HN.Util.initLogin = function () {

    var cookie = HN.Util.readCookie("auth"),
        user   = HN.Util.parseEmail(cookie),
        site   = document.location;
    $("#forgotten_pwd").html("<a href=\"http://hypernumbers.com/_forgotten_password/?site=" + site + "\">forgotten password?</a>");

    var doLogin = function(e) {

        e.preventDefault();
        
        var email = $("#email").val(),
            pass  = $("#pass").val(),
            rem   = true;
        
        $("#loginfeedback").html("");
        $("#submit").attr("disabled", "disabled").val("Loading");
        
        HN.Util.login(email, pass, rem, function(result) {
                  
                  if (result.error) {
                      $("#submit").removeAttr("disabled").val("Log In");
                      $("#loginfeedback").html("<strong>error:</strong> "+result.error);
                  } else {
                      window.location.reload(true);
                  }
              });
        
    };    


    setTimeout(function () {
                   
                   if (user !== "anonymous") {

                       $("#uname").text(user);
                       $("#editloggedin").show();
                       $("#logout").attr("href", "/_logout/?return=" +
                                         escape(window.location.href));
                       
                   } else {
                       $("#editanon").show();
                       $("#login").submit(doLogin);
                   }
                    $("#powered").bind("mouseover", function () {
                        $("#editspreadsheet").addClass("active");
                        $("#editmenu").show();
                        var f = function (e) {
                            if (!HN.Util.is_inside(e.target,
                                                   $("#editspreadsheet")[0])){
                                $(document).unbind("mousedown", f);
                                $("#editmenu").hide();
                                $("#editspreadsheet").removeClass("active");
                            }
                        };
                        $(document).bind("mousedown", f);
                    });
                    
                    $("#editspreadsheet").fadeIn("slow");
               }, 1000);
    
};


HN.Util.readCookie = function(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
	    var c = ca[i];
	    while (c.charAt(0)==' ') c = c.substring(1,c.length);
	    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
};

HN.Util.parseEmail = function(cookie) {
    var email = cookie.split("|")[0];
    email = email.replace("!", "@");
    email = email.replace("#", "+");
    return email;
};

HN.Util.login = function(email, pass, remember, cb) {
    
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
                cb({"error" : "Sorry, there was an error processing your "
                    + "login, please try again" });
            }
        });
        
    } else {
        cb({"error" : "Please enter full details"});
    }
};

HN.Util.getCrumbTrail = function() {
    var path = HN.Util.pathToList(document.location.pathname);
    var bits = document.location.host.split(":");
    var host = bits[0];
    var port = bits[1];
    var acc = "";
    var ret = "<a href=\"http://" + document.location.host + "\">" 
        + host + "</a>";
    for (var i = 0; i < path.length; i++) {
        acc = acc + path[i]  + "/";
        ret += " -> <a href=\"http://" + document.location.host + "/" 
            + acc + "\">" + path[i] + "</a>";
        };
    return ret;
};

HN.Util.getQueryParams = function() {
    var queries = new Object();
 
    // Use the String::replace method to iterate over each
    // name-value pair in the query string. Location.search
    // gives us the query string (if it exists).
    window.location.search.replace(
        new RegExp( "([^?=&]+)(=([^&]*))?", "g" ),
  
        // For each matched query string pair, add that
        // pair to the URL struct using the pre-equals
        // value as the key.
        function( $0, $1, $2, $3 ){
            queries[ $1 ] = $3;
        }
    );
    return queries;
};