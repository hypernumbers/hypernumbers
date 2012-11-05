HN.SiteAdmin = {};

HN.SiteAdmin.reload = function () {

    var onSubmit, scrolls, s, i, height, ajaxUpload, uploadCompleteFn;
    scrolls = $(".hn_scroll");
    if (scrolls !== []) {
        for (i = 0; i < scrolls.length; i++) {
            height = $(scrolls[i]).parent().parent().height();
            $(scrolls[i]).css("height", height - 142);
            $(scrolls[i]).parent().css("display", "block");
        }
    }
    // now set up upload
    uploadCompleteFn = function(file, response) {
        console.log(response);
        console.log(file);
        console.log("upload complete...");
    };
    // don't forget to fix the upload for sub-pages
    ajaxUpload = new AjaxUpload($(".hn_file_upload"), {
                                    "responseType" : "json",
                                    "name"         : "Filedata",
                                    "onComplete"   : uploadCompleteFn
                                });       
};

