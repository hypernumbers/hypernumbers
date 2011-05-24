/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.MapSheet = {};

HN.MapSheet.reload = function () {

    var uploadComplete, divs, i, ajaxUpload = [],
        map, type;
    
    uploadComplete = function(file, response) {
        if (response.error) {
            HN.Util.showDialog("There has been an error: " +
                               response.error);
        };
    };

    divs = $("input[type=submit].hn-mapsheet");
    
    for (i = 0; i < divs.length; i++) {
        map = $(divs[i]).attr("data-map");
        type = $(divs[i]).attr("data-map-type"),
        page = $(divs[i]).attr("data-map-page");
        ajaxUpload = new AjaxUpload(divs[i], {
            "responseType" : "json",
            "name"         : "Mapdata",
            "data"         : {"map"  : map,
                              "type" : type,
                              "page" : page},
            "action"       : document.location.pathname,
            "onComplete"   : uploadComplete
        });
    }
};