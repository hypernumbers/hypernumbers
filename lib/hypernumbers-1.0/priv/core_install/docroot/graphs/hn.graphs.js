HN.Graphs = {};

HN.Graphs.setup = function () {
    google.load("visualization", "1", {packages:["corechart"]});
};    

HN.Graphs.draw = function (div) {
    var array = eval($(div).attr("data-array")),
    options = eval($(div).attr("data-options")),
    opts2 = {}, i, chart, data;

    for (i = 0; i < options.length/2; i ++) {
        opts2[options[i * 2]] = options[i * 2 + 1];
    }

    chart = new google.visualization.LineChart(div),
    data = google.visualization.arrayToDataTable(array);
    chart.draw(data, opts2);    
}

HN.Graphs.reload = function () {
    var i, graphs = $(".hn_graphs");
    for (i = 0; i < graphs.length; i ++) {
     HN.Graphs.draw(graphs[i]);   
    }
};

HN.Graphs.setup();
