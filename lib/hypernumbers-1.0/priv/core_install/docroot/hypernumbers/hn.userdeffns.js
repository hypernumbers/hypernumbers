/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 100000, white: false */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, layout: false */
/**
 * @class HN.UserDefFns
 *
 */

HN.namespace("UserDefFns");

HN.UserDefFns.configure = function () {
    var div = $("#hn_parameters");
    div.innerHTML = "";
    HN.UserDefFns.add_parameter(1);
};

HN.UserDefFns.add_parameter = function (n) {
    var html = "<div class='hn_uf_subhead'>Parameter <span>" + n +
        "<span><img class='hn_uf_addparam'' alt='Add'' src='img/add.png'>" +
        "</div><div class='clear'>" +
        "<div class='hn_uf_insettxt'>Name:</div>" +
        "<input type='text' class='hn_uf_inp hn_uf_fnpname' />" +
        "<div class='hn_uf_insettxt'>Description:</div>" +
        "<input type='text' class='hn_uf_inp hn_uf_fndesc' />" +
        "<div class='hn_uf_insettxt'>Value:</div>" +
        "<input type='text' class='hn_uf_inp hn_uf_fncell' />" +
        "</div>",
    div = $("#hn_parameters");
    console.log(html);
    console.log(div.html());
    console.log(div);
    div.html(div.html() + html);
};
