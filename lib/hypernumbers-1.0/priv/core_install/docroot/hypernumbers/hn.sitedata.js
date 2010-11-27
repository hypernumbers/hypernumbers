/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true maxerr: 10000 */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, Finder: false */
HN.SiteData = function () {

    var api = {};
    
    api.loadSiteData = function (SuccessFun) {
        $.ajax({
            "url"      : "/_site/",
            "dataType" : "json",
            "success"  : SuccessFun
        });
    };
    return api;
};