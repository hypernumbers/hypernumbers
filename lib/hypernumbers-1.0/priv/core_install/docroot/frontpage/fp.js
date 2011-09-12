var VIDEO = {};

VIDEO.run = function() {
    $("#hn_text").click(VIDEO.onclick);
}

VIDEO.onclick = function() {
    $("#hn_text").html("<iframe src='http://player.vimeo.com/video/26333002?title=0&amp;byline=0&amp;color=ffffff&amp;autoplay=1'</iframe>");
    $("#hn_text").css("border", "none");
    $("#hn_text").css("box-shadow", "none");
}