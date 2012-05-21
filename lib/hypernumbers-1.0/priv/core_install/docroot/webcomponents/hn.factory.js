HN.Factory = {};

HN.Factory.reload = function () {

    var onSubmit;

    onSubmit = function () {

        var callback, siteType, email, fields, resp, gotourl, 
        i, ref, cell, path, url, json, id, locs = {};

        resp = $(this).attr("data-response");
        gotourl = $(this).attr("data-goto");

        callback = function (data) {

            if (data === "success") {

                alert(resp);
                document.location.href = gotourl;

            } else if (data.reason === "invalid_email") {

                alert("That email is invalid.");

            }
        };

        document.body.style.cursor = "wait";
        siteType = $(this).attr("data-type");
        email = $("input[data-input=" + $(this).attr("id") + "]").val();
        url = document.location.pathname + 
            $($(this).parent().parent().parent()).attr("data-ref");
        fields = $(".hn_factory_input");

        for (i = 0; i < fields.length; i++) {
            locs[$(fields[i]).attr("data-location")] = $(fields[i]).val();
        }
        
        json =  {"signup": {"sitetype" : siteType, 
                            "email"    : email, 
                            "data"     : locs}};

        $.post(url, JSON.stringify(json), callback, "json");

    };
    $(".hn_factory input[type=submit]").click(onSubmit);
     
};

