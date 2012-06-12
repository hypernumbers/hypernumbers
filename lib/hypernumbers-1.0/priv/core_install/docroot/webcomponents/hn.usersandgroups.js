HN.UsersAndGroups = {};


HN.UsersAndGroups.toggle = function(e) {
    var user = $(e.currentTarget).attr("data-user"),
    group = $(e.currentTarget).attr("data-group"),
    checked = $(e.currentTarget).attr("checked"),
    cell = $(e.currentTarget).parent().parent().parent().parent()
        .parent().parent().parent().parent(),
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

HN.UsersAndGroups.reload = function() {
    var checkboxes = $(".hn_user_admin input[type=checkbox]"), 
    boxes = $(".hn_user_admin"), content = $(".hn_user_admin > .hn_overflow2"),
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
};