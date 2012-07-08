HN = {};

HN.Phone_debug = function (phone) {
    var selects, onChangeFn;

    onChangeFn = function (e) {
        var id = $(e.currentTarget).attr("id"),
        val = $("#" + id + " :selected").val();
        console.log(val);
        switch (id) {
        case "hn_capabilities_debug":
            if (val === "none") {
                phone.twilio_capabilities = [];
            } else {
                phone.twilio_capabilities[val] =  true;
            }
            phone.setup();
            break;
        case "hn_phone_out_debug":
            phone.phone_out_permissions = val;
            phone.setup();
            break;
        case "hn_sms_out_debug":
            phone.sms_out_permissions = val;
            phone.setup();
            break;
        case "hn_email_debug":
            phone.email_permissions = val;
            phone.setup();
            break;
        }
    };

    // String is too long
    selects = "#hn_capabilities_debug, #hn_phone_out_debug, " +
        "#hn_sms_out_debug, #hn_email_debug";

    // Bind to various debug selects
    $(selects).change(onChangeFn);
};

HN.Phone = function () {
    var api = {},
    init,
    setup_twilio_fn, setup_phone_fn, setup_sms_fn, setup_email_fn,
    phone_dial_click_fn, phone_hangup_click_fn, 
    phone_away_click_fn, phone_sms_click_fn,
    dialpad_click_fn, dialpad_keydown_fn,
    is_phone_fn, show_fn, enable_fn;

    // first setup the permissions
    api.twilio_capabilities=[];
    api.phone_out_permissions="none";
    api.sms_out_permissions="none";
    api.email_permissions="none";

    phone_dial_click_fn = function() {
        console.log("dial clicked");        
    };

    phone_hangup_click_fn = function() {
        console.log("hangup clicked");        
    };

    phone_away_click_fn = function() {
        console.log("away clicked");        
    };

    phone_sms_click_fn = function() {
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
        // Allow: backspace, delete, tab, escape, and enter
        if ( e.keyCode == 46 || e.keyCode == 8 || 
             e.keyCode == 9 || e.keyCode == 27 || e.keyCode == 13 || 
             // Allow: Ctrl+A
            (e.keyCode == 65 && e.ctrlKey === true) || 
             // Allow: home, end, left, right
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

    is_phone_fn = function (capability) {
        if (api.twilio_capabilities[capability]) {
            return true;
        } else {
            return false;
        }
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

    // Various setup fns
    setup_twilio_fn = function () {
        console.log("setup twilio");
    };

    setup_phone_fn = function () {

        // make the phone appear
        show_fn(is_phone_fn("phone out") ||
                  is_phone_fn("phone in"), "#hn_phone_dialpad");

        // make the text box appear
        show_fn((api.sms_out_permissions !== "none"), "#hn_phone_text");

        // Now enable phone elements
        // the dialpad and the number box 
       enable_fn((is_phone_fn("phone out") && 
                   (api.phone_out_permissions === "free dial" ||
                    api.sms_out_permissions === "free all")), 
                  ".hn_phone, #hn_phone_number");
          
        // now the phone specific stuff
        enable_fn((is_phone_fn("phone out") && 
                   api.phone_out_permissions === "free dial"), 
                  "#hn_phone_dial");

        // now the SMS specific stuff
        enable_fn((is_phone_fn("phone out") && 
                   api.sms_out_permissions === "free all"), 
                  "#hn_phone_text, #hn_phone_sms");

        if (is_phone_fn("phone out") && 
                   api.sms_out_permissions === "free all") {
            
            } else {

            };
    };

    setup_sms_fn = function () {
        console.log("setup sms");
    };

    setup_email_fn = function () {
        console.log("setup email");
    };

    init = function() {
        $("#hn_phone_dial").bind("click",     phone_dial_click_fn);
        $("#hn_phone_hangup").bind("click",   phone_hangup_click_fn);
        $("#hn_phone_away").bind("click",     phone_away_click_fn);
        $("#hn_phone_sms").bind("click",      phone_sms_click_fn);
        $(".hn_phone").bind("click",          dialpad_click_fn);
        $("#hn_phone_number").bind("keydown", dialpad_keydown_fn);
    };

    // now create the initialisation fn
    api.setup = function() {
        setup_twilio_fn();
        setup_phone_fn();
        setup_sms_fn();
        setup_email_fn();
    };

    // finally initialise and return
    init();
    api.setup();
    return api;
};

HN.SoftPhone = new HN.Phone();

HN.Phone_debug(HN.SoftPhone);