HN = {};

HN.Phone = {};

HN.SMS = {};

HN.Phone.Out = {};

HN.SMS.Out = {};

HN.Phone.Out.initialize = function (Response) {
    var bindfn, clickfn, params;
    $("#hn-phone-headline").html(Response.headline_txt);
    $("#hn-phone-img").html(Response.button_txt);
    params = {"hypertag": Response.hypertag};
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

HN.SMS.Out.initialize = function (Response) {
  var bindfn, clickfn, params, path, phoneno, data;
    path = document.location.pathname.toLowerCase();
    phoneno = Response.phoneno;
    $("#hn-phone-headline").html(Response.headline_txt);
    $("#hn-phone-img").html(Response.button_txt);
    $("#hn-phone").removeAttr("disabled");
    clickfn = function () {
      var element = $("#hn-phone"), successFun;
      successFun = function (Response) {
        if (Response === "success") {
          $("#hn-phone-response").html("Message sent to " + phoneno);
        } else {
          $("#hn-phone-response").html("Message sending failed");
        }
      };
      data = JSON.stringify({"manual_sms": {"phoneno" : phoneno,
                                            "msg"     : "yay!"}}),
      $.ajax({
               "url"      : path + "?view=phone",
               "type"     : "POST",
               "data"     : data,
               "dataType" : "json",
               "success"  : successFun
             });
    };

    $("#hn-phone").bind("click", clickfn);

};

HN.Phone.initialize = function() {
    var params, successFun, path;
    path = document.location.pathname.toLowerCase();
    // now start the call
    successFun = function (Response) {
        if (Response.softphone_type === "outbound call") {
          HN.Phone.Out.initialize(Response);
        } else if (Response.softphone_type === "manual sms") {
          HN.SMS.Out.initialize(Response);
        }
        $("#hn-phone-frame").toggle();
    };
      $.ajax({
               "url"      :  path + "?view=phone",
               "dataType" : "json",
               "success"  : successFun
           });

};

HN.Phone.initialize();

