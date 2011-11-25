/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.MapCustom = {};

HN.MapCustom.reload = function () {

    var uploadComplete, onSubmit, divs, i, ajaxUpload = [],
        map, type, obj, klass = 'hn_ajax_map_custom';
    
    // first clean up
    $("." + klass).remove();

    uploadComplete = function (file, response) {
        if (response.error) {
            HN.Util.showDialog("There has been an error: " +
                               response.error);
        } else {
            HN.Util.showDialog("Upload completed");
        };
        document.body.style.cursor = "default";
    };

    onSubmit = function () {
        document.body.style.cursor = "wait";
    };

    divs = $("input[type=submit].hn-mapcustom");
    
    for (i = 0; i < divs.length; i++) {
        map = $(divs[i]).attr("data-map");
        type = $(divs[i]).attr("data-map-type");
        obj = {
            "responseType" : "json",
            "name"         : "Mapdata",
            "data"         : {"map"  : map,
                              "type" : type},
            "action"       : document.location.pathname,
            "onComplete"   : uploadComplete,
            "onSubmit"     : onSubmit,
            "class"        : klass
        };
        ajaxUpload = new AjaxUpload(divs[i], obj);
    }
};