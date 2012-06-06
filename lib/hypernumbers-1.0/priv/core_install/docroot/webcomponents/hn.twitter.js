HN.Twitter = {};

HN.Twitter.reload = function () {
    var d = document, 
    s = "script", 
    id = "twitter-wjs",
    fjs = d.getElementsByTagName(s)[0],
    elem, js, head;
    js = d.createElement(s);
    js.id = id;
    js.src = "//platform.twitter.com/widgets.js";
    if ($("body").attr("data-twttr-rendered") === "true") {
        $("body").attr("data-twttr-rendered", "");
    }
    if (!d.getElementById(id)) {
        fjs.parentNode.insertBefore(js,fjs);
    } else {
        elem = d.getElementById(id);
        elem.parentNode.removeChild(elem);
        head = $("head");
        head.insertBefore(js, head);     
    }
};
