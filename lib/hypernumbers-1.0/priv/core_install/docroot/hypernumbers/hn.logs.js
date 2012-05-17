/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 100000, white: false */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, layout: false */
/**
 * @class HN.logs
 *
 */
HN = {};

HN.logs = {};

HN.logs.init = function () {
    
    var i, b, buttons, successFn, onClickFn;

    successFn = function () {
        location.reload(true);  
    };

    onClickFn = function (e) {
        var json = {
           "revert_to": e.currentTarget.dataset.reversion 
        };
        $.ajax({
                   "type"     : "POST",
                   "url"      : document.location.pathname,
                   "dataType" : "json",
                   "data"     : JSON.stringify(json),
                   "success"  : successFn});
    };
    
    buttons = $(".button");
    for (i = 0; i < buttons.length; i++) {
        $(buttons[i]).bind("click", onClickFn);
    }
};

HN.logs.init();