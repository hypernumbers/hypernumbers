HN.Phone = {};

HN.Phone.Out = {};

HN.Phone.Out.initialize = function (Response) {
    var bindfn, path, clickfn;
    path = document.location.pathname.toLowerCase();
    $("#hn-phone-img").html(Response.button_txt);
    Twilio.Device.setup(Response.phonetoken);
    
    Twilio.Device.ready(function (device) {
                            $("#hn-phone").removeAttr("disabled");
                        });
    
    Twilio.Device.error(function (error) {
                        });
    
    Twilio.Device.incoming(function (conn) {
                               conn.accept();
                           });
    
    Twilio.Device.connect(function (conn) {
                              $("#hn-phone > div")
                                  .css("background-image",
                                       "url(/img/telephone_delete.png)");
                          });
    
    Twilio.Device.disconnect(function (conn) {
                                 $("#hn-phone > div")
                                     .css("background-image",
                                          "url(/img/telephone.png)");
                             });
    clickfn = function () {
        var element = $("#hn-phone");
        switch (Twilio.Device.status()) {
        case "ready":
            params = {"hypertag": Response.hypertag};
            Twilio.Device.connect(params);
            break;
        case "busy":
            Twilio.Device.disconnectAll();
            break;
        default:
            ok;
        };
    };
    
    $("#hn-phone").bind("click", clickfn);
    Twilio.Device.connect(params);
    
};
};

HN.Phone.initialise = function() {
    var params, SuccessFun;
    // now start the call
    SuccessFun = function (Response) {
        HN.Phone.Out.initialize(Response);
    };  
    $.ajax({
               "url"      :  path + "?view=phone",
               "dataType" : "json",
               "success"  : SuccessFun
           });
    
};

HN.Phone.initialize();

