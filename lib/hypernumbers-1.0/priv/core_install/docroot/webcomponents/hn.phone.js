HN.Phone = {};

HN.Phone.phone_outbound_reload = function() {
    var bindfn, element, ref, path;
    element = $("#hn-outboundphone");
    ref = $(element).parent().attr("data-ref");
    path = document.location.pathname.toLowerCase();
    var SuccessFun = function(Response) {
        var clickfn;
        Twilio.Device.setup(Response.phonetoken);

        Twilio.Device.ready(function (device) {
                                $(element).removeAttr("disabled");
                            });

        Twilio.Device.error(function (error) {
                                console.log("error");
                                console.log(error);
                            });

        Twilio.Device.connect(function (conn) {
                                  $("#hn-outboundphone > div")
                                      .css("background-image",
                                           "url(/img/telephone_delete.png)");
                              });

        Twilio.Device.disconnect(function (conn) {
                                     $("#hn-outboundphone > div")
                                         .css("background-image",
                                              "url(/img/telephone.png)");
                                 });

        clickfn = function () {
            var element = $("#hn-outboundphone");
            switch (Twilio.Device.status()) {
            case "ready":
                Twilio.Device.connect();
                break;
            case "busy":
                Twilio.Device.disconnectAll();
                break;
            default:
                console.log("default handling: " + Twilio.Device.status());
            };
        };
        $(element).bind("click", clickfn);
    };
    $.ajax({
               "url"      : "/_services/phone" + path +
                   ref + "?service=phone",
               "dataType" : "json",
               "success"  : SuccessFun
           });
};
