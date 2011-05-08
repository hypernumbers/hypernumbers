/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, Finder: false */
HN.Map = function () {

    var api = {};

    api.loadMap = function (SuccessFun, name) {
        $.ajax({
            "url"      :"/_site/?map=" + name,
            "dataType" : "json",
            "success"  : SuccessFun
            });
        };

    return api;
    };