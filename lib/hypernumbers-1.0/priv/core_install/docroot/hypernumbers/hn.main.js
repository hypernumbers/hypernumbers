/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, $: false, alert: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false  */
// TODO: Get rid of globals, expose one for use through at the shell
var hn      = new HN.Site(),
    sheets  = [],
    layout  = null,
    toolbar = null,
    ref     = HN.Util.parseRef(document.location.pathname.toLowerCase()),
    path    = ref.path,
    initialLoad, makeTabs, updateTabs, sheetDataLoader, hasLoaded,
    showDeveloper, showAdvanced, showHidden, show, options;

document.heightCalced = 0;


initialLoad = function (initPath) {
    var page;
    layout  = new HN.Layout(sheets[initPath]);
    toolbar = new HN.ToolBar(layout);

    HN.UI.init(layout);
    
    page = hn.data.getPage(initPath);    
    toolbar.userNavigation(page.user);

};

makeTabs = function () {

    $("#tabs").empty();
    
    $.each(hn.data.getLoadedUrls(), function () {

        var name = (this === "/") ? document.location.host + "/" : this;

        $("<a href='" + this + "'>" + name + "</a>").click(function (e) {
                window.location.hash = 
                hn.hashUrl.setParam("path", $(this).attr("href"));
            return false;
        }).appendTo($("#tabs"));
    });
};

updateTabs = function () {
    makeTabs();
    $("#tabs a").removeClass("current");
    $("#tabs a[href='" + hn.currentPath() + "']").addClass("current");
};

sheetDataLoader = function (sheetPath) {

    var perms;

    if (typeof sheets[sheetPath] === "undefined") {

        if (hn.finder) {
            hn.finder.addPath(HN.Util.pathToList(sheetPath));
        }

        return hn.data.addPage(sheetPath);
    }

    updateTabs();
    
    if (sheetPath === hn.currentPath(HN.url)) {
        
        path = sheetPath;
        document.title = path;

        $("#currentViewPath").text(path);
        
        sheets[path].processLoaded();
        layout.switchTab(sheets[path]);
        
        HN.Util.id("copied").style.display = "none"; 
            
        if (hn.finder) {
            hn.finder.select(HN.Util.pathToList(path));
        }
    }
};

hasLoaded = false;
options = {

    "authError": function (path) {
        var ref = HN.Util.parseRef(document.location.pathname.toLowerCase()),
        urlpath = ref.path;
        window.location.hash = 
            hn.hashUrl.setParam("path", urlpath);
        alert("Sorry, you do not have permission to access: " + path);
    },

    "dataLoaded": function (path) {
        sheets[path] = new HN.Sheet(path, hn.data, true);

        if (!hasLoaded) {
            hn.startPoll();
            initialLoad(path);
            // set up the hn.hasURL
            hn.hashUrl.setParam("path", path);
            hasLoaded = true;
        }
        sheetDataLoader(path);
        toolbar.loadFunctions();
        // now calculate column and row maxes
        hn.resetRowColMax();
    },
        
    "dataReloaded": function (data) {
        data.path = path;
        sheets[path].reload_data(data);
        layout.dataUpdated();
        sheetDataLoader(path);
    },

    "cellChanged": function (path, y, x) {
        sheets[path].cellChanged(y, x);
    },
    
    "update": function () {
        sheets[path].processLoaded();
        layout.dataUpdated();
    }
};


if ($.browser.msie || $.browser.opera) {
    $("#loading").html(
        "Sorry, your browser is " +
            "not supported yet.<br />Please try <a href='http://firefox.com'>" +
            "Firefox(3.6+)</a>, <a href='http://www.apple.com/safari/'>Safari</a>" +
            " or <a href='http://www.google.com/chrome'>Chrome</a><br />" +
            "<br />If you have just signed up, please check your email<br />" +
            "for instructions on how to log back in.");
} else {
    // initialise hn (ugly)
    hn.functions = [];
    hn.pages = [];
    hn.groups = [];
    hn.is_admin = false;
    hn.lang = "en_gb";
    var siteSuccessFun = function (data) {
        //hn.pages = data.pages;
        hn.groups = data.groups;
        hn.is_admin = data.is_admin;
        hn.lang = data.lang;
        hn.templates = data.templates,
        hn.ctrlPanel.loadTemplates(data.templates);
        hn.ctrlPanel.loadMaps(data.maps);
        hn.ctrlPanel.initCtrlPanel();
        hn.pagesPanel.initPagesPanel();
        hn.pagesPanel.loadTemplates(data.templates);
        hn.data = new HN.Data(options);
        hn.data.addPage(path);
        $("#loading, #cover").fadeOut("slow");
    };
    
    hn.sitedata = new HN.SiteData();
    hn.map = new HN.Map();
    hn.sitedata.loadSiteData(siteSuccessFun);
    // need to refresh the pages view
    // ie we haven't loaded it
    hn.refresh_pages = true;
    HN.Util.showAdvanced();
    HN.Util.showHidden();
    HN.Util.showDeveloper();
   // now setup login
    HN.Util.initLogin("spreadsheet");

}