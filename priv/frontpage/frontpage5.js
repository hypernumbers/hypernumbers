var Frontpage = function() {

    var onFocusFun = function(e) {
        $("#signupemail").val("");
        $("#siognupemail").unbind(onFocusFun);
    };

    $("#signupemail").focus(onFocusFun);

};

var HN_Frontpage = new Frontpage();

