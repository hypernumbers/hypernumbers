/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.MapSheet = {};

HN.MapSheet.reload = function () {

    var uploadComplete, onSubmit, page, divs, i, ajaxUpload = [],
        map, type, ref, url, klass = "hn_ajax_map_sheet";

    // first clean up
    $("." + klass).remove();
    
    onSubmit = function () {
        document.body.style.cursor = "wait";
    };

    uploadComplete = function(file, response) {
        if (response.error) {
            HN.Util.showDialog("There has been an error: " +
                               response.error);
        } else {
            HN.Util.showDialog("Upload completed");
        };
        document.body.style.cursor = "default";
    };

    divs = $("input[type=submit].hn-mapsheet");
    
    for (i = 0; i < divs.length; i++) {
        map = $(divs[i]).attr("data-map");
        type = $(divs[i]).attr("data-map-type"),
        page = $(divs[i]).attr("data-map-page");
        ref = HN.Util.parseRef(document.location.pathname);
        url = ref.path + $($(divs[i]).parent()).attr("data-ref");
        ajaxUpload = new AjaxUpload(divs[i], {
            "responseType" : "json",
            "name"         : "Mapdata",
            "data"         : {"map"  : map,
                              "type" : type,
                              "page" : page},
            "action"       : url,
            "onComplete"   : uploadComplete,
            "onSubmit"     : onSubmit,
            "class"        : klass
        });
    }
};