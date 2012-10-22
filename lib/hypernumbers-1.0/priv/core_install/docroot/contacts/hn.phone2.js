var HN = {};

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

// Define the object with an API 'cos the debugger needs it
HN.Phone = function () {
    var api = {},
    init, get_config, register_phone,
    post, split_numbers, format_numbers,
    setup_twilio_fn, setup_phone_fn, setup_email_fn,
    phone_dial_click_fn, phone_hangup_click_fn, 
    phone_away_click_fn, phone_sms_click_fn,
    dialpad_click_fn, dialpad_keydown_fn,
    email_send_click_fn, text_entry_fn,
    format_groups_fn,
    showif_fn, enableif_fn, clearif_fn;

    // first setup the permissions
    api.twilio_capabilities = {"phone_in": false, "phone_out": false};
    api.phone_out_permissions="none";
    api.sms_out_permissions="none";
    api.email_permissions="none";
    api.default_dialling_code = false;
    
    // now get the user details
    api.username = "";
    api.phone_already_registered = false;
    api.xtn = "";
    api.groups = [];

    // setup phone details
    api.hypertag = "";
    api.site = "";
    api.phonetoken = "";

    // the default values (if any)
    api.default_phone_no = "";
    api.default_sms_text = "";
    api.default_email_to = "";
    api.default_email_cc = "";
    api.default_email_from = "";
    api.default_email_subject = "";
    api.default_email_body = "";

    // finally store the ajax updater query so as we can abort it
    api.xhr = "";

    // Register the phone with the server
    // This is a long poll fn so it calls itself
    register_phone = function (status) {
        var register;
        if (status.phoneid) {           
            register = function () {
                var json, path, id;
                id = status.phoneid;
                path = document.location.pathname.toLowerCase();
                json = {"postwebcontrols": {"phone": {"register" : id,
                                                      "groups"   : api.groups}
                                           }
                       };
                post(path, json, register_phone);
            };
            setTimeout(register, 0);
        
        } else if (status.error) {
            api.phone_already_registered = true;
        }
    };

    // Now get the config and set up the phone
    get_config = function () {
        var params, successFun, path;
        path = document.location.pathname.toLowerCase();
        // now start the call
        successFun = function (R) {

            var convert = function(c) {
                var obj, pin, pout;
                if (c.phone_in === "true") {
                    pin = true;
                } else if (c.phone_in === "false") {
                    pin = false;
                };
                if (c.phone_out === "true") {
                    pout = true;
                } else if (c.phone_out === "false") {
                    pout = false;
                };
                obj = {phone_in:  pin,
                       phone_out: pout};
                return obj;
            };
            // permissions            
            api.phone_out_permissions    = R.config.phone_out_permissions;
            api.sms_out_permissions      = R.config.sms_out_permissions;
            api.email_permissions        = R.config.email_permissions;
            api.default_dialling_code    = R.config.default_dialling_code;
            // user details
            api.username                 = R.user.name;
            api.phone_already_registered = R.user.already_registered;
            api.xtn                      = R.config.extension;
            api.groups                   = R.config.groups;
            // phone details
            api.twilio_capabilities      = convert(R.capabilities);
            api.hypertag                 = R.hypertag;
            api.site                     = R.site;
            api.phonetoken               = R.phonetoken;
            // defaults
            api.default_phone_no         = R.config.phone_no;
            api.default_sms_text         = R.config.sms_msg;
            api.default_email_to         = R.config.email_to;
            api.default_email_cc         = R.config.email_cc;
            api.default_email_from       = R.config.email_from;
            api.default_email_subject    = R.config.email_subject;
            api.default_email_body       = R.config.email_body;
            console.log(api);
            // now run setup for twilio
            setup_twilio_fn();
            // now run the setup for the softphone
            api.setup();
            // so just register it already so it can take incoming calls
            register_phone({"phoneid": Math.floor(Math.random() * 1000000)});

    };
      $.ajax({
               "url"      :  path + "?view=phone",
               "dataType" : "json",
               "success"  : successFun
           });                
    };

    // we want to get on with it so fire it now
    get_config();

    // Some Utility Fns
    
    // POST
    post = function(url, json, callbackfn) {
        $.post(url, JSON.stringify(json), callbackfn, "json");
    };

    format_numbers = function (array) {
        var i, newarray = [], prefix, doubleprefix, fixednum, status, len,
        def = api.default_dialling_code;
        for (i = 0; i < array.length; i++) {
            if (array[i] !== "") {
                prefix = array[i].slice(0,1);
                switch (prefix) {
                case "+":
                    newarray.push(array[i]);
                    break;
                case "0":
                    doubleprefix = array[i].slice(1,2);
                    len = array[i].length;
                    switch (doubleprefix) {
                    case "0":
                        newarray.push("+" + array[i].slice(2, len));
                        break;
                    default:
                        if (def) {
                            newarray.push(def + array[i].slice(1, len));
                        } else {
                            newarray.push(array[i]);
                        }
                        break;
                    }
                    break;
                default:
                    if (def) {
                        newarray.push(def + array[i].slice(1, len));
                    } else {
                        newarray.push(array[i]);
                    }
                    break;
                }
            }
        };
        return newarray;
    };

    split_numbers = function (string) {
        var strings, regexp;
        regexp = /\+/g;
        string = string.replace(regexp, ",+");
        strings = string.split(",");
        return format_numbers(strings);
    };        

    clearif_fn = function (bool, jquery) {
        if (bool) {
            $(jquery).val("");
        };
    };

    showif_fn = function (bool, jquery) {
        if (bool) {
            $(jquery).css("display", "block");
        } else {
            $(jquery).css("display", "none");
        }
    };
    
    enableif_fn = function (bool, jquery) {
        if (bool) {
            $(jquery).removeAttr("disabled");
        } else {
            $(jquery).attr("disabled", "disabled");
        }
    };

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

    // Now the bindable functions

    phone_dial_click_fn = function() {
        var params = {"hypertag" : api.hypertag, 
                      "site"     : api.site},
        dial_fn, numbers, json, path, newnums;

        dial_fn = function (e) {
            //Twilio.Device.connect(params);            
        };

        newnums = split_numbers($("#hn_phone_number").val());

        // update the display and stop it being editable
        $("#hn_phone_number").val(newnums.join());
        enableif_fn(false, "#hn_phone_number");

        json = {"postwebcontrols": {"phone": {"dial": {"numbers" : newnums}}}};
        path = document.location.pathname.toLowerCase();
  
        if (true) {
        //if (Twilio.Device.status() === "ready") {
            $("#hn_phone_status").text("On Call....");
            $("#hn_phone_status").toggleClass("hn_flash");
            $("#hn_phone_dialpad").toggleClass("hn_flash_shadow");
            post(path, json, dial_fn);
            enableif_fn(false, "#hn_phone_dial");
            enableif_fn(true, "#hn_phone_hangup");
        };
    };

    phone_hangup_click_fn = function() {
        var json, path, hangup_fn;
        if (true) {
        //if (Twilio.Device.status() === "busy") {
            $("#hn_phone_status").text("Ready");
            $("#hn_phone_status").toggleClass("hn_flash");
            $("#hn_phone_dialpad").toggleClass("hn_flash_shadow");
            //Twilio.Device.disconnectAll();
            hangup_fn = function () {
                //console.log("hung up...");
            };
            json = {"postwebcontrols": {"phone": "hangup"}};
            path = document.location.pathname.toLowerCase();
            post(path, json, hangup_fn);
            enableif_fn(true, "#hn_phone_dial");
            enableif_fn(false, "#hn_phone_hangup");
            // the phone number will have been locked on dialling
            // it might need to be re-enables
            enableif_fn((api.twilio_capabilities.phone_out && 
                         (api.phone_out_permissions === "free dial" ||
                          api.sms_out_permissions === "free all")), 
                        "#hn_phone_number");
        };
    };

    phone_away_click_fn = function() {
        var json, path, away_fn, back_fn;
        if ($("#hn_phone_away").attr("data-away") === "false") {
            $("#hn_phone_status").text("Away");
            $("#hn_phone_status").toggleClass("hn_flash");
            $("#hn_phone_away").attr("data-away", "true");
            $("#hn_phone_away").html("Back");
            away_fn = function () {
                console.log("phone away...");
            };
            json = {"postwebcontrols": {"phone": "away"}};
            path = document.location.pathname.toLowerCase();
            post(path, json, away_fn);
        } else {
            $("#hn_phone_status").text("Ready");
            $("#hn_phone_status").toggleClass("hn_flash");
            $("#hn_phone_away").attr("data-away", "false");            
            $("#hn_phone_away").html("Away");
            back_fn = function () {
                console.log("phone back...");
            };
            json = {"postwebcontrols": {"phone": "back"}};
            path = document.location.pathname.toLowerCase();
            post(path, json, back_fn);
        }
    };

    phone_sms_click_fn = function() {

        var json, path, success_fn, phone_no, sms_msg;

        path = document.location.pathname.toLowerCase();

        // for when the sms message is posted
        success_fn = function (Response) {
            var phone_no = $("#hn_phone_number").val();
            if (Response === "success") {
                // console.log("no status bar on phone");
                $("#hn-phone-response").html("Message sent to " + phone_no);
            } else {
                // console.log("no status bar on phone");
                $("#hn-phone-response").html("Message sending failed");
            }
        };
        
        if ($("#hn_phone_sms").attr("data-sms-mode") === "false") {
            $("#hn_phone_sms").text("Send");
            $("#hn_phone_sms").attr("data-sms-mode", "true");
            $("#hn_phone_sms").toggleClass("btn-success");
            showif_fn(true, "#hn_phone_text_container");
            $("#hn_phone_text").focus();
        } else {
            phone_no = $("#hn_phone_number").val();
            sms_msg = $("#hn_phone_text").val();
            json = {"postwebcontrols" : {"send_sms": 
                                         {"phone_no" : phone_no,
                                          "sms_msg"  : sms_msg}
                                        }
                   },
            post(path, json, success_fn);
            // now tidy up the GUI
            $("#hn_phone_sms").text("Text");
            $("#hn_phone_sms").attr("data-sms-mode", "false");
            $("#hn_phone_sms").toggleClass("btn-success");
            showif_fn(false, "#hn_phone_text_container");
            clearif_fn((api.phone_out_permissions === "fixed dial" || 
                        api.sms_out_permissions === "fixed dial" ||
                        api.sms_out_permissions === "free message"),
                       "#hn_phone_text");
        }
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

        var json, path, success_fn, email_to, email_cc, email_from, 
        email_subject, email_body;

        path = document.location.pathname.toLowerCase();

        // for when the email message is posted
        success_fn = function (Response) {
            var phoneno = $("#hn_phone_number").val();
            if (Response === "success") {
                console.log("no status bar on phone");
                $("#hn-phone-email-response").html("Message sent to " + 
                                                   email_to);
            } else {
                console.log("no status bar on phone");
                $("#hn-phone-email-response").html("Message sending failed");
            }
        };

        email_to      = $("#hn_phone_to").val(); 
        email_cc      = $("#hn_phone_cc").val();
        email_from    = api.default_email_from; // never put in an editable field
        email_subject = $("#hn_phone_subject").val();
        email_body    = $("#hn_phone_body").val();

        json = {"postwebcontrols" : {"send_email": 
                                     {"email_to"      : email_to,
                                      "email_cc"      : email_cc,
                                      "email_from"    : email_from,
                                      "email_subject" : email_subject,
                                      "email_body"    : email_body}
                                    }
               },
        post(path, json, success_fn);
    };

    // Various setup fns
    setup_twilio_fn = function () {
        /*Twilio.Device.setup(api.phonetoken);
        
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
                                 });*/
    };
    
    setup_phone_fn = function () {

        var xtn;

        // Username
        $("#hn_phone_user_name").html("<div class='hn_label'>User</div>" + 
                                      api.username);

        // GENERAL Phone
        // make the phone appear
        showif_fn((api.twilio_capabilities.phone_out || 
                   api.twilio_capabilities.phone_in  || 
                   api.sms_out_permissions !== "none"), 
                  "#hn_phone_dialpad, #hn_phone_signed_in");

        // GENERAL SMS
        // SPECIFIC phone out
        // Now enable phone elements
        // the dialpad and the number box 
        enableif_fn(((api.twilio_capabilities.phone_out && 
                      api.phone_out_permissions === "free dial") ||
                     api.sms_out_permissions === "free all"), 
                    ".hn_phone, #hn_phone_number");
        // now the controls stuff
        enableif_fn((api.twilio_capabilities.phone_out && 
                     (api.phone_out_permissions === "free dial" ||
                      api.phone_out_permissions === "fixed dial")), 
                    "#hn_phone_dial");
        
        // SPECIFIC phone in
        xtn = "<div class='hn_label'>Extension</div>";
        showif_fn(api.twilio_capabilities.phone_in, 
                  "#hn_phone_user_groups, #hn_phone_user_xtn");        
        if (api.twilio_capabilities.phone_in) {
            if (api.xtn !== false) {
                $("#hn_phone_user_xtn").html(xtn + api.xtn);
            }
            $("#hn_phone_user_groups").html(format_groups_fn(api.groups));
        };
        enableif_fn(api.twilio_capabilities.phone_in, "#hn_phone_away");

        // SPECIFIC SMS
        enableif_fn((api.sms_out_permissions === "free all" ||
                     api.sms_out_permissions === "free message"), 
                    "#hn_phone_text");
        enableif_fn((api.sms_out_permissions === "free all"     ||
                     api.sms_out_permissions === "free message" ||
                     api.sms_out_permissions === "fixed all"), 
                    "#hn_phone_sms");
        
        // DEFAULTS
        $("#hn_phone_number").val(api.default_phone_no);
        $("#hn_phone_text").val(api.default_sms_text);
        $("#hn_phone_to").val(api.default_email_to);
        $("#hn_phone_cc").val(api.default_email_cc);
        // There is no email_from field
        $("#hn_phone_subject").val(api.default_email_subject);
        $("#hn_phone_body").val(api.default_email_body);

    };

    text_entry_fn = function () {
        var left = 140 - $("#hn_phone_text").val().length;
        $("#hn_phone_text_msg").text(left + " characters left");
        
    };

    setup_email_fn = function () {
        var showEmail = (api.email_permissions === "fixed all" ||
                         api.email_permissions === "free all"  ||
                         api.email_permissions === "free body"),
        showDialPad = (api.twilio_capabilities.phone_out|| 
                       api.twilio_capabilities.phone_in),
        panels = "#hn_phone_dialpad, #hn_phone_signed_in";

        showif_fn(showEmail, "#hn_phone_email");
        if (showEmail && showDialPad) {
            $(panels).css("margin-left", "55px");
        } else if (showEmail) {
            $("#hn_phone_signed_in").css("margin-left", "55px");  
        } else {
            $(panels).css("margin-left", "0px");        
        }
        enableif_fn((api.email_permissions === "free all"), 
                    ".hn_phone_email_input");
        enableif_fn((api.email_permissions === "free body" ||
                     api.email_permissions === "free all"), 
                    ".hn_phone_email_fields");
    };

    // now create the initialisation fn
    init = function() {
        $("#hn_phone_dial").bind("click",     phone_dial_click_fn);
        $("#hn_phone_hangup").bind("click",   phone_hangup_click_fn);
        $("#hn_phone_away").bind("click",     phone_away_click_fn);
        $("#hn_phone_sms").bind("click",      phone_sms_click_fn);
        $(".hn_phone").bind("click",          dialpad_click_fn);
        $("#hn_email_send").bind("click",     email_send_click_fn);
        $("#hn_phone_number").bind("keydown", dialpad_keydown_fn);
        $("#hn_phone_text").bind("keydown",   text_entry_fn);
    };

    // now setup the phone
    api.setup = function() {
        var msg;
        if (api.phone_already_registered) {
            msg = api.username + " already has a softphone open" +
                " (try again in a while)";
            $("#hn_phone_already_registered").text(msg).show();
            return;
        };
        setup_phone_fn();
        setup_email_fn();
        text_entry_fn();
        showif_fn(true, "#hn_phone_container");
    };

    // finally initialise and return
    init();
    return api;
};

HN.SoftPhone = new HN.Phone();

//HN.Phone_debug(HN.SoftPhone);