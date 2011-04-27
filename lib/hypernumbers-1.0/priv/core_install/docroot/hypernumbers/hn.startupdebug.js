/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global $: false, window: false  */
HN.StartUpDebug = function () {
    var api = {},
        filelog = {},
        msglog  = {},
        timelog = [],
        index   = 1;

    api.log = function(jsfile, item) {
        console.log("should log " + item + " for " + jsfile);
        var len,
        log = {"item": item, "index" : index};
        if (!filelog.hasOwnProperty(jsfile)) {
            filelog[jsfile] = [];
            filelog[jsfile][0] = log;
            } else {
               len = filelog[jsfile].length();
                console.log(len);
                filelog[jsfile][len] = log;
            }
        index += 1;
    };
    
    api.dump = function () {
        console.log(filelog);
    };
    return api;
}

HN.startupdebug = new HN.StartUpDebug();