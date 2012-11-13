/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, Twilio: false, console: false, google: false */

HN.Graphs = {};

HN.Graphs.setup = function () {
    google.load("visualization", "1", {packages : ["corechart"]});
};    

HN.Graphs.draw = function (div) {
    var array = eval($(div).attr("data-array")),
    options = eval($(div).attr("data-options")),
    opts2 = {}, i, chart, data;

    for (i = 0; i < options.length / 2; i = i + 1) {
        opts2[options[i * 2]] = options[i * 2 + 1];
    }

    chart = new google.visualization.LineChart(div);
    data = google.visualization.arrayToDataTable(array);
    chart.draw(data, opts2);    
};

HN.Graphs.reload = function () {
    var i, graphs = $(".hn_graphs");
    for (i = 0; i < graphs.length; i = i + 1) {
        HN.Graphs.draw(graphs[i]);   
    }
};

HN.Graphs.setup();
