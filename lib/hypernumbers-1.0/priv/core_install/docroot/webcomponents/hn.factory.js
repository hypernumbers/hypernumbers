HN.Factory = {};

HN.Factory.reload = function () {

    var onSubmit;

    onSubmit = function () {

        var callback, siteType, email, fields, resp, gotourl, 
        i, ref, cell, path, url, json, locs = {};

        callback = function (data) {

            if (data === "success") {

                resp = $(".hn_factory input[type=submit]")
                    .attr("data-response");
                gotourl = $(".hn_factory input[type=submit]")
                    .attr("data-goto");
                alert(resp);
                document.location.href = gotourl;

            } else if (data.reason === "invalid_email") {

                alert("That email is invalid.");

            }
        };

        document.body.style.cursor = "wait";
        siteType = $(".hn_factory input[type=submit]").attr("data-type");
        email = $(".hn_factory_email").val();
        ref = $(".hn_factory input[type=submit]").attr("data-ref");
        path = document.location.pathname,
        url = path + ref;

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

