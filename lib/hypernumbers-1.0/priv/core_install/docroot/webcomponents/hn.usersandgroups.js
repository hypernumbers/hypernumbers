HN.UsersAndGroups = {};

HN.UsersAndGroups.invite = function(e) {
    var id, email, gparray, msg, i, groups = [], 
    json, url, cell, ref, gotopage;
    cell = $(e.currentTarget).parent().parent();
    ref = $(cell).attr("data-ref"), 
    id = $(e.currentTarget).attr("data-id");
    email = $("#" + id + " > .hn_emailforgroup").val();
            if (email === "") {
                return;
            } else if (email === "workmate@example.com") {
                $("#" + id + " > .hn_newuserfeedback")
                    .html("please enter a real email");
                return;
            }
    gparray = $("#" + id + " > div > div > .hn_sel_groups :selected");
    if (gparray.length === 0) {
        $("#" + id + " > .hn_newuserfeedback")
            .html("you must select at least one group");
        return;
    }
    for (i = 0; i < gparray.length; i = i + 1) {
        groups.push($(gparray[i]).val());
    }
    $("#" + id + " > .hn_newuserfeedback")
        .html("user " + email + " added");
    msg = $("#" + id + " > .hn_newusermsg").val();
    gotopage = $("#" + id + " > .hn_invite_path").val();
    if (!gotopage) {
        gotopage = "/";
    };
    json = {"postwebcontrols": 
            {
                "invite_user" : {
                    "user"    : email,
                    "groups"  : groups,
                    "msg"     : msg,
                    "path"    : gotopage
                }
            }
           };
    url = document.location.pathname + ref;
    $.ajax({
               "type"     : "POST",
               "url"      : url,
               "data"     : JSON.stringify(json),
               "dataType" : "json"
           });
};

HN.UsersAndGroups.toggle = function(e) {
    var user = $(e.currentTarget).attr("data-user"),
    group = $(e.currentTarget).attr("data-group"),
    checked = $(e.currentTarget).attr("checked"),
    cell = $(e.currentTarget).parent().parent().parent().parent()
        .parent().parent().parent(),
    ref = $(cell).attr("data-ref"), 
    json, url;
    if (checked && checked === "checked") {
        json = {"postwebcontrols": 
                {
                    "add_user" : {
                        "user"  : user,
                        "group" : group
                    }
                }
               };
    } else {
        json = {"postwebcontrols": 
                {
                    "remove_user": {
                        "user"  : user,
                        "group" : group
                    }
                }
               };
      }
    url = document.location.pathname + ref;
    $.ajax({
               "type"     : "POST",
               "url"      : url,
               "data"     : JSON.stringify(json),
               "dataType" : "json"
           });
};

HN.UsersAndGroups.reload_u_and_g = function () {
    var checkboxes = $(".hn_user_admin input[type=checkbox]"), 
    boxes = $(".hn_user_admin"), 
    content = $(".hn_user_admin > .hn_overflow2"),
    i;
    $(checkboxes).bind("click", HN.UsersAndGroups.toggle);
    for (i = 0; i < boxes.length; i++) {
        var height = parseInt($(boxes[i]).parent().css("height")) - 14;
        $(boxes[i]).css("height", height);
    };
    for (i = 0; i < content.length; i++) {
        var height = parseInt($(content[i]).parent().css("height")) - 42;
        $(content[i]).css("height", height);
    }    
}

HN.UsersAndGroups.reload_invite_user = function() {
    var buttons = $(".hn_invite_admin input[type=submit]");
    $(buttons).bind("click", HN.UsersAndGroups.invite);
};