HN.SiteData = function () {

    var api = {};
    
    api.loadSiteData = function(SuccessFun) {
        $.ajax({
            "url"      : "/_site/",
            "dataType" : "json",
            "success"  : SuccessFun
        });
    };
    return api;
};