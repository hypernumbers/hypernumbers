/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, Twilio: false, console: false, google: false */

HN.postMessage = {};

HN.postMessage = function () {
    // Get the parent page URL as it was passed in,
    // for browsers that don't support
    // window.postMessage
    var hash = document.location.hash.replace(/^#/, ''),
    bits = hash.split("!"),
    parent_url = decodeURIComponent(bits[0]),
    name = bits[1],
    api = {};

    // The first param is serialized using $.param (if not a string)
    // and passed to the parent window. If window.postMessage exists,
    // the param is passed using that, otherwise it is passed in the
    // location hash (that's why parent_url is required).
    // The second param is the targetOrigin.
    api.setHeight = function () {
        var height = $("#outer").css("height"),
        width = $("#outer").css("width"),
        msg = {'height' : height, 'width' : width, 'name' : name};

        $.postMessage(msg, parent_url, parent);
    };
    return api;
};

// Only run this if it is inside a frame
if (top.frames.length !== 0) {
    var pm = new HN.postMessage();

    // need to override the css overflow for body
    $("body").css("overflow", "hidden");

    pm.setHeight();
}