HN.Toggle = {};

HN.Toggle.wikimsg = "End Editing";
HN.Toggle.webmsg  = "Edit";

HN.Toggle.setup = function () {
    var currentView= $("body").attr("data-view");
    if (currentView === "webpage") {
        $(".hn-toggleviews").attr("value", HN.Toggle.webmsg);
    } else if (currentView === "wikipage") {
        $(".hn-toggleviews").attr("value", HN.Toggle.wikimsg);
    }
};    

HN.Toggle.reload = function () {
    var currentView, path, views, clickfn, setupfn;
    currentView = $("body").attr("data-view");
    path = window.location.pathname.toLowerCase();
    views = hn.data.readViews(path);
    clickfn = function () { 
        if (currentView === "webpage") {
            $("body").attr("data-view", "wikipage");
            HN.data.refreshView(path);
            $(".hn-toggleviews").attr("value", HN.Toggle.webmsg);
        } else if (currentView === "wikipage") {
            $("body").attr("data-view", "webpage");
            HN.data.refreshView(path);
            $(".hn-toggleviews").attr("value", HN.Toggle.wikimsg);
        }
        setTimeout("HN.Toggle.call_wrapper()",100);
    };
    if (hn.data.readViews(path)) {
        // only show the toggle button if the user has both views
        if (($.inArray("webpage", views) > 0 && 
             $.inArray("wikipage", views) > 0)) {
            $(".hn-toggleviews").css("display", "block");
            HN.Toggle.setup();
        } else {
            $(".hn-toggleviews").css("display", "none");
        }
    }
    $(".hn-toggleviews").bind("click", clickfn);
};

HN.Toggle.call_wrapper = function () {
    HN.Toggle.reload();
    HN.Toggle.setup();
}

HN.Toggle.setup();
