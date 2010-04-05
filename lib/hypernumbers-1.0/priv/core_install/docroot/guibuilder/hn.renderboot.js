HN.RenderBoot = function()
{
    var options = {
        "stopUpdate"   : true,
        "dataLoaded"   : function() { htmlRender.render(document); },
        "dataReloaded" : function(data) {  },
        "update"       : function() { htmlRender.render(document); }
    };

    var data   = new HN.Data(options),
    htmlRender = new HN.HTMLRender(data);

    htmlRender.render(document);
}();