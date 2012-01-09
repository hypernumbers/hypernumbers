/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global Err: false, console: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

Err = {};

Err.Remoterr = {};

Err.Remoterr.onerror = function (msg, errorfileurl, lineno) {

    var jsonstring, response, pageurl, cookies;

    // Get some user input
    //response = prompt("There has been an error. " +
    //                  "It has been logged and will be investigated.", 
    //                  "Put in comments (and e-mail or phone number for" + 
    //                  " response.)");

    response = "Commmented out";

    // get some context of where and how the error occured
    // to make debugging easier
    pageurl = window.location.href;
    cookies = document.cookie;

    // Make the json message we are going to post
    // Could use JSON.stringify() here if you are sure that
    // JSON will have run when the error occurs
    // http://www.JSON.org/js.html
    jsonstring = "{\"set\": {\"jserr\": " +
        "{\"msg\": \""         + msg + "\", " + 
        "\"errorfileurl\": \"" + errorfileurl + "\", " +
        "\"pageurl\": \""      + pageurl + "\", " +
        "\"cookies\": \""      + cookies + "\", " +
        "\"lineno\": \""       + lineno + "\", " +
        "\"response\": \""     + response + "\"}}}";

    // Use the jquery cross-browser post
    // http://api.jquery.com/jQuery.post/
    // this assumes that no errors happen before jquery has initialised
    $.post("?jserr", jsonstring, null, "json");

    // I don't want the page to 'pretend' to work 
    // so I am going to return 'false' here
    // Returning 'true' will clear the error in the browser
    return false;
};

Err.ConsoleErr = {};

Err.ConsoleErr.initialise = function () {
    if (typeof console === "undefined" || typeof console.log === "undefined") {
        console = {};
        console.log = function (msg) {
            // do nothing
        };
    } 
};

window.onerror = Err.Remoterr.onerror;
Err.ConsoleErr.initialise();
