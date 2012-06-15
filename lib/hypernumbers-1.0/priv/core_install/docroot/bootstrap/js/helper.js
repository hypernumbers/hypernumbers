/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

/* some helper javascript to make bootstrap work */

HN.BootstrapHelper = {};

HN.BootstrapHelper.reload = function() {

    var divs = $(".dropdown-toggle"), 
    inners = $(".hn_inner"),
    i, parent;
    
    // if they are in button group find the grandparent
    // otherwise find the parent
    for (i = 0; i < divs.length; i++) {
        parent = $(divs[i]).parent();
        if ($(parent).hasClass("btn-group")) {
            $(parent).parent().css("overflow", "visible");
       } else {
            $(parent).css("overflow", "visible");
        }
    }
    // if the menu is in a include need to make the parent of
    // the include also overflow
    $(".hn_inner").parent().css("overflow", "visible");

};