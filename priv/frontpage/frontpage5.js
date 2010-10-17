var Frontpage = function() {

    var onFocusFun = function(e) {
        $("#signupemail").val("");
        $("#signupemail").unbind();
    };

    $("#signupemail").focus(onFocusFun);

};

var HN_Frontpage = new Frontpage();

