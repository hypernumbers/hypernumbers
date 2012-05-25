HN.Google = {};

HN.Google.reload = function () {

    var clickFn;

    clickFn = function (e) {

        var item = $(e.currentTarget).attr("data-item"),
        mark = {"google_buynow": item};

        HN.Callbacks.setMark(mark);
  };

    $(".hn_googlebuy").click(clickFn);
}