HN.namespace("Logger");
HN.Logger = function() 
{
    var public = {};
    var log    = [];
    
    public.start = function() {
        HN.Util.addEvent(document, "mousedown", function(e) {
            log.push(e);
        });
    };
    
    public.read = function() {
        return log;
    };
    
    public.clear = function() {
        log = [];
    };

    return public;
};