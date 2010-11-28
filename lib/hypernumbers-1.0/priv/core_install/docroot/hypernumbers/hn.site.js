/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: false */
/*global HN: false, hn: false, $: false, alert: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false  HashUrl: false, sheetDataLoader: false */
// TODO: Get rid of globals, expose one for use through at the shell
HN.Site = function () {
    
    var api = {};
    
    api.hashUrl   = null;
    api.finder    = null;
    api.logger    = new HN.Logger();
    api.ctrlPanel = new HN.UI.CtrlPanel();
    api.data      = null;
    api.groups    = {};
    api.templates = {};
    api.is_admin  = false;
    api.lang      = "";
    api.max_row_height = 0;
    api.max_col_width = 0;

    // define some functions
        function init() {
        // calculate the max size of rows and cols
        $(window).resize(function() {
            api.resetRowColMax();
        });
        api.logger.start();
    }
    
    function urlChanged() {
        
        var view  = api.hashUrl.getParam("view"),
            path  = api.hashUrl.getParam("path");
        
        if (api.hashUrl.getParam("path")) { 
            sheetDataLoader(path); 
        }
        
        if (api.hashUrl.getParam("tour")) { 
            hn.ctrlPanel.selectTab("sitetour");
            hn.ctrlPanel.open();
        } else if (api.hashUrl.getParam("site")) {
            hn.ctrlPanel.selectTab("sitemypages");
            hn.ctrlPanel.open();
        } else if (api.hashUrl.getParam("settings")) {
            hn.ctrlPanel.selectTab("sitesettings");
            hn.ctrlPanel.open();
        }
    }

    // Now initialise
    init();

    // define the api
    api.resetRowColMax = function() {
        //console.log("resizing rows and cols");
        api.max_row_height = Math.floor($("#spreadsheet").height() * 0.95);
        api.max_col_width = Math.floor($("#spreadsheet").width() * 0.95);
        //console.log("max row height  is " + api.max_row_height + " max col width is " + api.max_col_width);
    };
    
    api.currentPath = function () {
        return api.hashUrl.getParam("path") || document.location.pathname;
    };
    
    api.startPoll = function () {
        api.hashUrl = new HashUrl({"changed": urlChanged});
    };
    
    return api ;
};