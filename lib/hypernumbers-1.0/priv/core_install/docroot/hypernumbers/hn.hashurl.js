/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */
var HashUrl = function (callback) {

    var current = "",
        vars    = {};

    function parseHash(url) {
        
        var params = {}, param, tmp,
            arr = url.replace("#", "").split(",");
        
        for (param in arr) {
            if (arr.hasOwnProperty(param)) {
                tmp = arr[param].split("=");
                if (tmp.length === 1) {
                    params[tmp[0]] = true;
                } else {
                    params[tmp[0]] = tmp[1];
                }
            }
        }
        return params;
    }

    function poll() {

        if (window.location.hash === current) {
            return;
        }
        
        current = window.location.hash;
        vars    = parseHash(current);
        
        if (typeof callback.changed === "function") {
            callback.changed();
        }
    }

    function toUrl() {
        var nvars = [], param;
        for (param in vars) {
            if (param !== "") { 
                nvars.push(param  + "=" + vars[param]);
            }
        }
        return "#" + nvars.join(",");
    }

    
    setInterval(poll, 500);
    
    function getParam(name) {
        return vars[name] || false;
    }
    
    function setParam(key, value) {
        vars[key] = value;
        return toUrl();
    }
    
    function deleteParam(key) {
        delete vars[key];
        return toUrl();
    }
       
    return {
        "vars"        : vars,
        "getParam"    : getParam,
        "setParam"    : setParam,
        "deleteParam" : deleteParam,
        "toUrl"       : toUrl
    };
};