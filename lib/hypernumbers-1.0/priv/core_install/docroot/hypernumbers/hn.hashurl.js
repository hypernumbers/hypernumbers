HashUrl = function(callback) {

    var public  = {};
    var current = "";
    var vars    = {};
    public.vars = vars;
    
    setInterval(poll, 500);
    
    public.getParam = function(name) {
        return vars[name] || false;
    };

    public.setParam = function(key, value) {
        vars[key] = value;
        return public.toUrl();
    };
    
    public.deleteParam = function(key) {
        delete vars[key];
        return public.toUrl();
    };

    public.toUrl = function() {
        var nvars = [];
        for( var param in vars ) {
            if( param != "" ) { 
                nvars.push(param +"="+vars[param]);
            }
        }
        return "#" + nvars.join(",");
    };
    
    function parseHash(url) {
        var params = {}, param, arr = url.replace("#","").split(",");
        for( var param in arr ) {
            var tmp = arr[param].split("=");
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
   
    return public;
};