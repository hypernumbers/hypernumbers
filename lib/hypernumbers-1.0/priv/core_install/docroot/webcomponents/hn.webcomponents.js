/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

// initialises web components
HN.WebComponents = {};

HN.WebComponents.reload = function () {
    // Set up menus
    $(".potato-menu").ptMenu();
    $(".potato-menu").css("display", "block");
    // Need to make the parent of the menu allow overflow...
    // The reason for the multiple selection is that the fn '=include'
    // brings in a complete webpage fragment and you need to overflow that
    // fragment and the cell that '=include' is in...
    $(".potato-menu").parents("[data-ref]").css("overflow", "visible");
};
