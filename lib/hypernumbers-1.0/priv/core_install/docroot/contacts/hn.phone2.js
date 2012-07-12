HN = {};

//HN.Phone_debug = function (phone) {
//    var selects, onChangeFn;

//    onChangeFn = function (e) {
//        var id = $(e.currentTarget).attr("id"),
//        val = $("#" + id + " :selected").val();
//        switch (id) {
//        case "hn_capabilities_debug":
//            if (val === "none") {
//                phone.twilio_capabilities = {};
//            } else {
//                phone.twilio_capabilities[val] =  true;
//            }
//            phone.setup();
//            break;
//        case "hn_phone_out_debug":
//            phone.phone_out_permissions = val;
//            phone.setup();
//            break;
//        case "hn_sms_out_debug":
//            phone.sms_out_permissions = val;
//            phone.setup();
//            break;
//        case "hn_email_debug":
//            phone.email_permissions = val;
//            phone.setup();
//            break;
//        }
//    };
//
//    // String is too long
//    selects = "#hn_capabilities_debug, #hn_phone_out_debug, " +
//        "#hn_sms_out_debug, #hn_email_debug";
//
//    // Bind to various debug selects
//    $(selects).change(onChangeFn);
//};

HN.Phone = function () {
    var api = {},
    init, get_config,
    post,
    setup_twilio_fn, setup_phone_fn, setup_email_fn,
    phone_dial_click_fn, phone_hangup_click_fn, 
    phone_away_click_fn, phone_send_sms_click_fn,
    dialpad_click_fn, dialpad_keydown_fn,
    email_send_click_fn,
    format_groups_fn,
    show_fn, enable_fn;

    // first setup the permissions
    api.twilio_capabilities = {"phone_in": "false", "phone_out": "false"};
    api.phone_out_permissions="none";
    api.sms_out_permissions="none";
    api.email_permissions="none";
    
    // now get the user details
    api.username = "gordon@vixo.com";    // FIX UP later
    api.xtn = "1234";                    // FIX UP later
    api.groups = ["Sales", "Marketing"]; // FIX UP later

    // setup phone details
    api.hypertag = "";
    api.site = "";
    api.phonetoken = "";

    // Now get the get_config and set up the phone
    get_config = function () {
        var params, successFun, path;
        path = document.location.pathname.toLowerCase();
        // now start the call
        successFun = function (R) {
            api.phone_out_permissions = R.permissions.phone_out_permissions;
            api.sms_out_permissions   = R.permissions.sms_out_permissions;
            api.email_permissions     = R.permissions.email_permissions;
            api.twilio_capabilities   = R.capabilities;
            api.hypertag              = R.hypertag;
            api.site                  = R.site;
            api.phonetoken            = R.phonetoken;
            setup_twilio_fn();
            api.setup();
    };
      $.ajax({
               "url"      :  path + "?view=phone",
               "dataType" : "json",
               "success"  : successFun
           });                
    };

    // we want to get on with it so fire it now
    get_config();

    // POST
    post = function(url, json, callbackfn) {
        var path = document.location.pathname.toLowerCase();
        $.post({
                   "url"      : url,
                   "dataType" : "json",
                   "success"  : callbackfn
               });
    };

    phone_dial_click_fn = function() {
        console.log("dial clicked");
        var params = {"hypertag" : api.hypertag, 
                      "site"     : api.site};
        console.log(params);
        if (Twilio.Device.status() === "ready") {
            $("#hn_phone_status").text("On Call....");
            $("#hn_phone_status").toggleClass("hn_flash");
            $("#hn_phone_dialpad").toggleClass("hn_flash_shadow");
            Twilio.Device.connect(params);
            enable_fn(false, "#hn_phone_dial");
            enable_fn(true, "#hn_phone_hangup");
        };
    };

    phone_hangup_click_fn = function() {
        console.log("hangup clicked");        
        if (Twilio.Device.status() === "busy") {
            $("#hn_phone_status").text("Ready");
            $("#hn_phone_status").toggleClass("hn_flash");
            $("#hn_phone_dialpad").toggleClass("hn_flash_shadow");
            Twilio.Device.disconnectAll();
            enable_fn(true, "#hn_phone_dial");
            enable_fn(false, "#hn_phone_hangup");
        };
    };

    phone_away_click_fn = function() {
        if ($("#hn_phone_away").attr("data-away") === "false") {
            $("#hn_phone_status").text("Away");
            $("#hn_phone_status").toggleClass("hn_flash");
            $("#hn_phone_away").attr("data-away", "true");
            $("#hn_phone_away").html("Back");
        } else {
            $("#hn_phone_status").text("Ready");
            $("#hn_phone_status").toggleClass("hn_flash");
            $("#hn_phone_away").attr("data-away", "false");            
            $("#hn_phone_away").html("Away");
        }
        console.log("away clicked");        
    };

    phone_send_sms_click_fn = function() {
        console.log("sms clicked");        
    };

    dialpad_click_fn = function(e) {
        var key = $(e.currentTarget).attr("data-button"),
        number;
        switch (key) {
            case "plus":
            number = $("#hn_phone_number").val();
            $("#hn_phone_number").val(number + "+");
            break;
        case "comma":
            number = $("#hn_phone_number").val();
            $("#hn_phone_number").val(number + ",");
            break;
        default:
            number = $("#hn_phone_number").val();
            $("#hn_phone_number").val(number + key);
            break;
        }
    };    

    dialpad_keydown_fn = function(e) {
        // ALLOW: backspace, delete, tab, escape, and enter
        if ( e.keyCode == 46 || e.keyCode == 8 || 
             e.keyCode == 9 || e.keyCode == 27 || e.keyCode == 13 || 
             // ALLOW: Ctrl+A
            (e.keyCode == 65 && e.ctrlKey === true) || 
             // ALLOW: home, end, left, right
            (e.keyCode >= 35 && e.keyCode <= 39)) {
                 // let it happen, don't do anything
                 return;
        }
        else {
            // if it is not a number, a comma (188), a space (23) 
            //or a plus (SHIFT 61) stop it
            if ((e.shiftKey || ((e.keyCode < 48 || e.keyCode > 57) && 
                 (e.keyCode < 96 || e.keyCode > 105)) && 
                 !(e.keyCode === 32 || e.keyCode === 188)) &&
                !(e.shiftKey && e.keyCode === 61)) {
                e.preventDefault(); 
            }   
        }
    };

    email_send_click_fn = function() {
        console.log("send email button clicked");
    };

    show_fn = function (boolean, jquery) {
        if (boolean) {
            $(jquery).css("display", "block");
        } else {
            $(jquery).css("display", "none");
        }
    };
    
    enable_fn = function (boolean, jquery) {
        if (boolean) {
            $(jquery).removeAttr("disabled");
        } else {
            $(jquery).attr("disabled", "disabled");
        }
    }

    // Utility fns
    format_groups_fn = function(groups) {
        var g, html = "<div class='hn_label'>Groups</div>";
        if (groups.length === 0) {
            return "";
        }
        for (g = 0; g < groups.length - 1; g++) {
            html = html + groups[g] + ", ";
        }
        html = html + groups[groups.length - 1];
        return html;
    };

    // Various setup fns
    setup_twilio_fn = function () {
        Twilio.Device.setup(api.phonetoken);
        
        Twilio.Device.ready(function (device) {
                                console.log("ready...");
                            });
        
        Twilio.Device.error(function (error) {
                                console.log("error...");
                            });
        
        Twilio.Device.incoming(function (conn) {
                                   console.log("incoming...");
                                   conn.accept();
                               });
        
        Twilio.Device.connect(function (conn) {
                                  console.log("connect...");
                              });
        
        Twilio.Device.disconnect(function (conn) {
                                     console.log("disconnect...");
                                 });
    };
    
    setup_phone_fn = function () {

        var left, xtn;

        // Username
        $("#hn_phone_user_name").html("<div class='hn_label'>User</div>" + 
                                      api.username);

        // GENERAL Phone
        // make the phone appear
        show_fn((api.twilio_capabilities.phone_out || 
                 api.twilio_capabilities.phone_in), 
                "#hn_phone_dialpad, #hn_phone_signed_in");
        
        // GENERAL SMS
        // make the text box appear
        show_fn((api.sms_out_permissions !== "none"), 
                "#hn_phone_text_container");

        // SPECIFIC phone out
        // Now enable phone elements
        // the dialpad and the number box 
       enable_fn((api.twilio_capabilities.phone_out && 
                   (api.phone_out_permissions === "free dial" ||
                    api.sms_out_permissions === "free all")), 
                  ".hn_phone, #hn_phone_number");
        // now the controls stuff
        enable_fn((api.twilio_capabilities.phone_out && 
                   api.phone_out_permissions === "free dial"), 
                  "#hn_phone_dial");

        // SPECIFIC phone in
        xtn = "<div class='hn_label'>Extension</div>";
        show_fn(api.twilio_capabilities.phone_in, 
                "#hn_phone_user_groups, #hn_phone_user_xtn");        
        if (api.twilio_capabilities.phone_in) {
            $("#hn_phone_user_xtn").html(xtn + api.xtn);
            $("#hn_phone_user_groups").html(format_groups_fn(api.groups));
        };
        enable_fn(api.twilio_capabilities.phone_in, "#hn_phone_away");

        // SPECIFIC SMS
        enable_fn((api.twilio_capabilities.phone_out && 
                   (api.sms_out_permissions === "free all" ||
                   api.sms_out_permissions === "free message")), 
                  "#hn_phone_text");
        enable_fn((api.twilio_capabilities.phone_out && 
                   (api.sms_out_permissions === "free all" ||
                    api.sms_out_permissions === "free message") ||
                   api.sms_out_permissions === "fixed all"), 
                  "#hn_phone_text, #hn_phone_send_sms");

        left = 140 - $("#hn_phone_text").val().length;
        $("#hn_phone_text_msg").text(left + " characters left");

    };

    setup_email_fn = function () {
        var show = (api.email_permissions === "fixed all" ||
                 api.email_permissions === "free all"  ||
                 api.email_permissions === "free body"),
        panels = "#hn_phone_dialpad, #hn_phone_signed_in";

        show_fn(show, "#hn_phone_email");

        if (show) {
            $(panels).css("margin-left", "55px");
        } else {
            $(panels).css("margin-left", "0px");        
        }
        enable_fn((api.email_permissions === "free all"), 
                  ".hn_phone_email_input");
        enable_fn((api.email_permissions === "free body" ||
                   api.email_permissions === "free all"), 
                  ".hn_phone_email_fields");
    };

    init = function() {
        $("#hn_phone_dial").bind("click",     phone_dial_click_fn);
        $("#hn_phone_hangup").bind("click",   phone_hangup_click_fn);
        $("#hn_phone_away").bind("click",     phone_away_click_fn);
        $("#hn_phone_send_sms").bind("click", phone_send_sms_click_fn);
        $(".hn_phone").bind("click",          dialpad_click_fn);
        $("#hn_email_send").bind("click",     email_send_click_fn);
        $("#hn_phone_number").bind("keydown", dialpad_keydown_fn);
    };

    // now create the initialisation fn
    api.setup = function() {
        setup_phone_fn();
        setup_email_fn();
    };

    // finally initialise and return
    init();
    api.setup();
    return api;
};

HN.SoftPhone = new HN.Phone();

//HN.Phone_debug(HN.SoftPhone);