HN = {};

HN.wordpress = {};

HN.wordpress.postMessage = function () {
    var src = "http://gordon.dev:9000",
    baseurl = encodeURIComponent(document.location.href),
    iframe, receiveFun, height, width, style;

    style = "overflow:hidden;height:1000px;width:700px;border:1;";

    iframe = $("<iframe src = '" + src + "#" + baseurl + 
               "' style='" + style + "'>" +
               "</iframe>").appendTo('#iframe');

    receiveFun = function (e) {
        var h = e.data.replace(/.*height=(\d+)(.*$)/, '$1'),
        w = e.data.replace(/.*width=(\d+)(.*$)/, '$1'),
        recursiveFn;
        if (h !== height) {
            $(iframe).css("height", h);
            height = h;
        }
        if (w !== width) {
            $(iframe).css("width", w);
            width = w;
        }
        recursiveFn = function () {
            $.receiveMessage(receiveFun);
        };
        setTimeout(recursiveFn, 1, "");
    };

    $.receiveMessage(receiveFun);
};

var frame = new HN.wordpress.postMessage();