/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.MapRows = {};

HN.MapRows.reload = function () {

    var uploadComplete, onSubmit, divs, i, ajaxUpload = [],
        map, type, klass = 'hn_ajax_map_rows';

    // first clean up
    $("." + klass).remove();

    uploadComplete = function(file, response) {
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

    divs = $("input[type=submit].hn-maprows");
    
    for (i = 0; i < divs.length; i++) {
        map = $(divs[i]).attr("data-map");
        type = $(divs[i]).attr("data-map-type");
        ajaxUpload = new AjaxUpload(divs[i], {
            "responseType" : "json",
            "name"         : "Mapdata",
            "data"         : {"map"  : map,
                              "type" : type},
            "action"       : document.location.pathname,
            "onComplete"   : uploadComplete,
            "onSubmit"     : onSubmit,
            "class"        : klass
        });
    }
};