/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, Twilio: false, console: false */

var HN = {};

HN.Phone = {};

HN.Phone.Out = {};
HN.Phone.In = {};

HN.Phone.Out.initialize = function (Response) {
    var clickfn, params, readyFn, errorFn, incomingFn, 
    connectFn, disconnectFn;

    $("#hn-phone-headline").html(Response.headline_txt);
    $(".hn-phone-img").html(Response.button_txt);
    params = {"hypertag" : Response.hypertag, 
              "site"     : Response.site};
    Twilio.Device.setup(Response.phonetoken);

    readyFn = function (device) {
        $("#hn-phone").removeAttr("disabled");
    };

    Twilio.Device.ready(readyFn);

    errorFn = function (error) {
    };

    Twilio.Device.error(errorFn);

    incomingFn = function (conn) {
        conn.accept();
    };

    Twilio.Device.incoming(incomingFn);

    connectFn = function (conn) {
        $("#hn-phone > div").css("background-image",
                                 "url(/img/telephone_delete.png)");
    };

    Twilio.Device.connect(connectFn);

    disconnectFn = function (conn) {
        $("#hn-phone > div").css("background-image",
                                 "url(/img/telephone.png)");
    };

    Twilio.Device.disconnect(disconnectFn);
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
            break;
        }
    };

    $("#hn-phone").bind("click", clickfn);
};

HN.Phone.In.initialize = function (Response) {

    console.log(Response);

    var clickfn, params, readyFn, errorFn, incomingFn, 
    connectFn, disconnectFn;

    $("#hn-phone-headline").html(Response.headline_txt);
    $(".hn-phone-img").html(Response.button_txt);
    params = {"hypertag" : Response.hypertag, 
              "site"     : Response.site};

    Twilio.Device.setup(Response.phonetoken);

    readyFn = function (device) {
        $("#hn-phone").removeAttr("disabled");
    };

    Twilio.Device.ready(readyFn);

    errorFn = function (error) {
    };

    Twilio.Device.error(errorFn);

    incomingFn = function (conn) {
        conn.accept();
    };

    Twilio.Device.incoming(incomingFn);

    connectFn = function (conn) {
        $("#hn-phone > div").css("background-image",
                                 "url(/img/telephone_delete.png)");
    };

    Twilio.Device.connect(connectFn);

    disconnectFn = function (conn) {
        $("#hn-phone > div").css("background-image",
                                 "url(/img/telephone.png)");
    };

    Twilio.Device.disconnect(disconnectFn);

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
            break;
        }
    };

    $("#hn-phone").bind("click", clickfn);
};

HN.SMS = {};

HN.SMS.Out = {};

HN.SMS.Out.initialize = function (Response) {

    var clickfn, path, phoneno, msg;

    path = document.location.pathname.toLowerCase();
    phoneno = Response.phoneno;
    msg = Response.msg;
    $("#hn-phone-headline").html(Response.headline_txt);
    $(".hn-phone-img").html(Response.button_txt);
    $("#hn-phone-msg").html(msg);
    $("#hn-phone").removeAttr("disabled");
    clickfn = function () {
        var element = $("#hn-phone"), successFun, data, msg;
        successFun = function (Response) {
            if (Response === "success") {
                $("#hn-phone-response").html("Message sent to " + phoneno);
            } else {
                $("#hn-phone-response").html("Message sending failed");
            }
        };
        msg = {"manual_sms": {"phoneno" : phoneno, "msg" : msg}};
        data = JSON.stringify(msg);
        $.ajax({"url"      : path + "?view=phone",
                "type"     : "POST",
                "data"     : data,
                "dataType" : "json",
                "success"  : successFun}
               );
    };
    
    $("#hn-phone").bind("click", clickfn);

};

HN.Email = {};

HN.Email.Out = {};

HN.Email.Out.initialize = function (Response) {
    
    var clickfn, path, to, cc, status;

    path = document.location.pathname.toLowerCase();
    to = Response.to;
    cc = Response.cc;
    $("#hn-phone-headline").html(Response.headline_txt);
    $(".hn-phone-img").html(Response.button_txt);
    $("#hn-phone-to").html("<strong>To:</strong> " + Response.to);
    $("#hn-phone-from").html("<strong>From:</strong> " + Response.reply_to);
    $("#hn-phone-cc").html("<strong>cc:</strong> " + Response.cc);
    $("#hn-phone-subject").html("<strong>Subject:</strong> " + Response.subject);
    $("#hn-phone-contents").html(Response.contents);
    $(".hn-phone-img").removeClass("hn-phone-img");
    $("#hn-phone").removeAttr("disabled");
    clickfn = function () {

        var element = $("#hn-phone"), successFun, data, msg;

        successFun = function (Response) {
            if (Response === "success") {
                if (cc !== "") {
                    status = "Message sent to " + to + " and " + cc;
                } else {
                    status = "Message sent to " + to;
                }
                $("#hn-phone-response").html(status);
            } else {
                $("#hn-phone-response").html("Message sending failed");
            }
        };
        msg = {"manual_email": {"to": Response.to,
                                "reply_to" : Response.reply_to,
                                "cc" : Response.cc,
                                "subject" : Response.subject,
                                "contents" : Response.contents}
              };
        data = JSON.stringify(msg);
        $.ajax({"url"      : path + "?view=phone",
                "type"     : "POST",
                "data"     : data,
                "dataType" : "json",
                "success"  : successFun}
             );
    };

    $("#hn-phone").bind("click", clickfn);

};

HN.Phone.initialize = function () {
    var params, successFun, path;
    path = document.location.pathname.toLowerCase();
    // now start the call
    successFun = function (Response) {
        if (Response.softphone_type === "outbound call") {
            HN.Phone.Out.initialize(Response);
        } else if (Response.softphone_type === "inbound call") {
            HN.Phone.In.initialize(Response);
        } else if (Response.softphone_type === "manual sms") {
            HN.SMS.Out.initialize(Response);
        } else if (Response.softphone_type === "manual email") {
            HN.Email.Out.initialize(Response);
        }
        $("#hn-phone-frame").toggle();
    };
    $.ajax({"url"      :  path + "?view=phone",
            "dataType" : "json",
            "success"  : successFun}
           );
};

HN.Phone.initialize();

