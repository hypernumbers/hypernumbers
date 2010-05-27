HashUrl = function(callback) {

    var current = "",
        vars    = {};
    
    setInterval(poll, 500);
    
    function getParam(name) {
        return vars[name] || false;
    };
    
    function setParam(key, value) {
        vars[key] = value;
        return toUrl();
    };
    
    function deleteParam(key) {
        delete vars[key];
        return toUrl();
    };

    function toUrl() {
        var nvars = [], param;
        for( param in vars ) {
            if( param != "" ) { 
                nvars.push(param +"="+vars[param]);
            }
        }
        return "#" + nvars.join(",");
    };
    
    function parseHash(url) {
        
        var params = {}, param, tmp,
            arr = url.replace("#","").split(",");
        
        for( param in arr ) {
            tmp = arr[param].split("=");
            if( tmp.length == 1 ) {
                params[tmp[0]] = true;
            } else {
                params[tmp[0]] = tmp[1];
            }
        };
        return params;
    };
    
    function poll() {

        if( window.location.hash == current ) {
            return;
        }
        
        current = window.location.hash;
        vars    = parseHash(current);
        
        if( typeof callback.changed == "function" ) {
            callback.changed();
        }
    };
   
    return {
        "vars"        : vars,
        "getParam"    : getParam,
        "setParam"    : setParam,
        "deleteParam" : deleteParam,
        "toUrl"       : toUrl
    };
};