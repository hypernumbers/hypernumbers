HN.Factory = {};

HN.Factory.reload = function () {

    var onSubmit;

    onSubmit = function () {

        var div = $(".hn_factory input[type=submit]"),
        callback, siteType, email, fields, resp, gotourl, 
        i, ref, cell, path, url, json, locs = {};

        callback = function (data) {

            if (data === "success") {

                resp = $(div).attr("data-response");
                gotourl = $(div).attr("data-goto");
                console.log(resp);
                alert(resp);
                document.location.href = gotourl;

            } else if (data.reason === "invalid_email") {

                alert("That email is invalid.");

            }
        };

        document.body.style.cursor = "wait";
        siteType = $(div).attr("data-type");
        email = $(".hn_factory_email").val();
        url = document.location.pathname + $(div.parent().parent().parent())
            .attr("data-ref");

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

