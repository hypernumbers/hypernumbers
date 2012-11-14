/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global jQuery: false, console: false, prompt: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false  */

var Err = {};

Err.Remoterr = {};

Err.Remoterr.onerror = function (msg, errorfileurl, lineno) {

    var jsonstring, response, pageurl, cookies, ourfiles, ours, errorpath,
    retval, site, escStrFn;
    
    escStrFn = function (str) {
        return str
            .replace(/[\\]/g, '\\\\')
            .replace(/[\"]/g, '\\\"')
            .replace(/[\/]/g, '\\/')
            .replace(/[\b]/g, '\\b')
            .replace(/[\f]/g, '\\f')
            .replace(/[\n]/g, '\\n')
            .replace(/[\r]/g, '\\r')
            .replace(/[\t]/g, '\\t')
            .replace(/[{]/g,  '\\{')
            .replace(/[}]/g,  '\\}');
    };

    site = window.location.protocol + "//" + window.location.host + "/";
    errorpath = errorfileurl.replace(site, "");

    pageurl = window.location.href;
    cookies = document.cookie;

    // Make the json message we are going to post
    // Could use JSON.stringify() here if you are sure that
    // JSON will have run when the error occurs
    // http://www.JSON.org/js.html
    jsonstring = "{\"set\": {\"jserr\": " +
        "{\"msg\": \""         + escStrFn(msg) + "\", " + 
        "\"errorfileurl\": \"" + errorfileurl + "\", " +
        "\"pageurl\": \""      + pageurl + "\", " +
        "\"cookies\": \""      + cookies + "\", " +
        "\"lineno\": \""       + lineno + "\"}}}";

    // Use the jquery cross-browser post
    // http://api.jquery.com/jQuery.post/
    // this assumes that no errors happen before jquery has initialised
    $.post("?jserr", jsonstring, null, "json");

    return retval;
};

Err.ConsoleErr = {};

Err.ConsoleErr.initialise = function () {
    if (typeof console === "undefined" || typeof console.log === "undefined") {
        // we want this to be a global variable
        console = {};
        console.log = function (msg) {
            // do nothing
        };
    } 
};
// Add Ajax errors as well
//$(document).ajaxError(function(e, xhr, settings) {
//  logError(settings.url + ':' + xhr.status + '\n\n' + xhr.responseText);
//});
window.onerror = Err.Remoterr.onerror;
Err.ConsoleErr.initialise();
