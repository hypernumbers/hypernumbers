/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false */
HN.namespace("Logger");
HN.Logger = function () 
{
    var api = {},
        log = [];
    
    api.start = function () {
        HN.Util.addEvent(document, "mousedown", function (e) {
            log.push(e);
        });
    };
    
    api.read = function () {
        return log;
    };
    
    api.clear = function () {
        log = [];
    };

    return api;
};