HN.Factory = {};

HN.Factory.reload = function () {

    var onSubmit;

    onSubmit = function () {
        var siteType, email, fields, i, ref, cell, path, json, locs = {};
        document.body.style.cursor = "wait";
        siteType = $(".hn_factory input[type=submit]").attr("data-type");
        email = $(".hn_factory_email").val();
        ref = $(".hn_factory input[type=submit]").attr("data-ref");
        cell = HN.Util.parseAddr(ref),
        fields = $(".hn_factory_input");
        for (i = 0; i < fields.length; i++) {
            locs[$(fields[i]).attr("data-location")] = $(fields[i]).val();
        }
        path = document.location.pathname,
        json =  {"signup": {"sitetype" : siteType, 
                            "email"    : email, 
                            "data"     : locs}};
        HN.Util.postCell(path, cell.obj, json);
    };
    $(".hn_factory input[type=submit]").click(onSubmit);
     
};

