/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: false */
/*global HN: false, $: false, alert: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false  */
// TODO: Get rid of globals, expose one for use through at the shel
var hn      = new HN.Site(),
    sheets  = [],
    layout  = null,
    toolbar = null,
    path    = document.location.pathname,
    initialLoad, makeTabs, updateTabs, sheetDataLoader, hasLoaded, options;

document.heightCalced = 0;

initialLoad = function (initPath) {

    layout  = new HN.Layout(sheets[initPath]);
    toolbar = new HN.ToolBar(layout);

    HN.UI.init(layout);
    
    var page = hn.data.getPage(initPath);    
    toolbar.userNavigation(page.user);

    $("#loading, #cover").fadeOut("slow");
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
    $("#tabs a[href=" + hn.currentPath(HN.url) + "]").addClass("current");
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
        document.title = path + " - hypernumbers";

        $("#currentViewPath").text(path);
        
        sheets[path].processLoaded();
        layout.switchTab(sheets[path]);

        perms = hn.data.key(path, "permissions");
                
        // New permission Stuff
        toolbar.loadViews();
       
        HN.Util.id("copied").style.display = "none"; 
            
        if (hn.finder) {
            hn.finder.select(HN.Util.pathToList(path));
        }
    }
};

hasLoaded = false;
options = {

    "authError": function (path) {
        window.location.hash = 
            hn.hashUrl.setParam("path", document.location.pathname);
        alert("Sorry, you do not have permission to access: " + path);
    },

    "dataLoaded": function (path) {
        sheets[path] = new HN.Sheet(path, hn.data, true);

        if (!hasLoaded) {
            hn.startPoll();
            initialLoad(path);
            hasLoaded = true;
        }
        sheetDataLoader(path);
        toolbar.loadSite();
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


if ( $.browser.msie || $.browser.opera ) {
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
    var successFun = function(data) {
        hn.functions = data.functions;
        hn.pages = data.pages;
        hn.groups = data.groups;
        hn.is_admin = data.is_admin;
        hn.lang = data.lang;
        hn.ctrlPanel.loadTemplates(data.templates);
        hn.data = new HN.Data(options);
        hn.data.addPage(path);
        };
    hn.sitedata = new HN.SiteData();
    hn.sitedata.loadSiteData(successFun);
    // now setup login
    HN.Util.initLogin("spreadsheet");

}