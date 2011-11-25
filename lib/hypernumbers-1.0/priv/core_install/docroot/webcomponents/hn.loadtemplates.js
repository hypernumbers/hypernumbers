/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.LoadTemplates = {};

HN.LoadTemplates.reload = function () {

    var uploadComplete, onSubmit, divs, i, ajaxUpload = [],
        template, obj;
    
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

    divs = $("input[type=submit].hn-loadtemplate");
    
    for (i = 0; i < divs.length; i++) {
        template = $(divs[i]).attr("data-template");
        obj = {
            "responseType" : "json",
            "name"         : "Templatedata",
            "data"         : {"load_templates"  : template},
            "action"       : document.location.pathname,
            "onComplete"   : uploadComplete,
            "onSubmit"     : onSubmit
        };
        ajaxUpload = new AjaxUpload(divs[i], obj);
    }
};