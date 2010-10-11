// TODO: Get rid of globals, expose one for use through at the shell
HN.UI.CtrlPanel = function () {
    
    var api        = {},
    currentTab = null,
    ajaxUpload = null;
    currenttourpage = 1,
    tourpages = $("#tourwrapper").children().length;
    
    api.show_tour_page = function(x, firemark) {
        currenttourpage = x;
        HN.Util.id("tourpager").innerHTML = "Page "+currenttourpage
            +" of "+tourpages+" Pages";
        $("#tourwrapper").children().hide();
        $("#tourwrapper").children().eq(x-1).show();
        if (firemark) {
            HN.Callbacks.setMark("viewing page "+x+" of the tour");
        }
    };

    api.loadTemplates = function(templates) {
        var html = "";
        hn.templates = templates;
        html = HN.Util.makeTemplates();
        $("#templates").html(html);
    };
    
    api.open = function () {
        if (currentTab) {
            api.selectTab(currentTab);
        } else {
            api.selectTab($("#ctrltabs li a").attr("data-tab"));
        }
            
        $("#cover, #ctrlpanel").show();
        hn.finder.activate();
    
        switch (currentTab) {
        case "sitemypages": 
            $("#newpage").focus(); 
            $("#templatename").focus(); 
            break;
        case "sitesettings": 
            $("#passwordval").focus(); 
            break;
        default :
        };
    };
    
    api.close = function () { 
        $("#cover, #ctrlpanel").hide();
        hn.finder.deactivate();
        setTimeout(layout.resumeSelection, 0);
    };

    api.selectTab = function (tab) { 
        currentTab = tab;
        
        $("#ctrltabs li a.selected").removeClass("selected");
        $("#ctrltabs li a[data-tab=" + tab + "]").addClass("selected");
        $("#ctrltabdivs > div").hide();
        $("#ctrltabdivs div#" + tab).show();
        layout.grabFocus();
    };

    api.pageSelected = function (path) { 
        var pathStr = HN.Util.listToPath(path);
        $(".currentpageselection").val(pathStr).text(pathStr);
        $("#newpage").attr("value", pathStr);
        $("#visitlink").attr("href", pathStr);        
    };

    function makePath(str) { 
        var path = HN.Util.correctPath( str );
        hn.finder.addPath(HN.Util.pathToList(path));
        hn.finder.select(HN.Util.pathToList(path));
        return path;
    };
    
    function newPage() { 
        var path = makePath( $("#newpage").val() ),
        template = $("#templateslist :selected").val();
        if (template !== "blank") {
            HN.Callbacks.loadTemplate(path, template);
        };
        window.location.hash = hn.hashUrl.setParam("path", path);
        $("#newpage").val("");
        api.close();        
    };

    function uploadStart(file, extension) { 
        $("#doupload").attr("disabled", "disabled").val("Uploading ...");
        ajaxUpload.setAttribute("action", hn.finder.asString());
    };

    function uploadComplete(file, responseTypee) {
        document.location.href = response.location +
            "?view=spreadsheet";
    };

    function setPassword(password) {
        HN.Callbacks.setPassword(password, function(data) { 
            if( data.result === "success" ) { 
                $("#settingsfeedback").empty()
                    .append("<div class='success'>Password Set" +
                            "</div>");
            } else { 
                $("#settingsfeedback").empty()
                    .append("<div class='error'>Error: " + data.failure +
                            "</div>");
            }
            $("#passwordval").val("");
            setTimeout(function () { 
                $("#settingsfeedback").empty();
            }, 2000);
        });
    };

    function initEvents () {
        
        // Now set up the tour
        $("#tournext").click( function() {
                                  var id = ( currenttourpage == tourpages )
                                      ? 1 : currenttourpage+1;
                                      api.show_tour_page(id, true);
                              });
        
        $("#tourback").click( function() {
                                  var id = ( currenttourpage == 1 )
                                      ? tourpages : currenttourpage-1;
                                      api.show_tour_page(id, true);
                              });
        
        api.show_tour_page(1, false);

        $("#newpage, #newwebpage, #newusername, #passwordval").val("");
        $("#doupload, #donewuser").removeAttr("disabled");
        
        $("#saveastemplate").bind("mousedown", function () {
            var templatename = $("#templatename").val(),
            path = $("#currentpageselection").val();
            if (templatename === "") {
                $("#saveasfeedback").html("<small>please enter a name</small>");
                return;
            };
            HN.Callbacks.saveAsTemplate(path, templatename);
            $("#templatename").val("");
            $("#saveasfeedback").html("<small>"+templatename+" saved</small>");
        });

        $("#ctrldelete").bind("mousedown", function () {
            var cb = function () {
                window.location.reload(true);
            };
            HN.Util.postPath(hn.finder.asString(), {"delete": "all"}, cb);
        });
        
        $("#passwordform").bind("submit", function (e) { 
            e.preventDefault();
            if ($("#passwordval").val() === "") {
                return;
            }
            setPassword( $("#passwordval").val() );
        });
        
        $("#ctrlclose").bind("mousedown", function () { 
             api.close();
         });
        
        $("#ctrltabs").bind("mousedown", function (e) { 
            e.preventDefault();
            if( e.target.nodeName == "A" ) {
                api.selectTab(e.target.getAttribute("data-tab"));
                api.open();
            }
        });
        
        $("#newpageform").submit(function (e) {
            e.preventDefault(); 
            newPage();
        });
        $("#newwebpageform").submit(function (e) {
            e.preventDefault(); 
            newWebPage();
        });

        ajaxUpload = new AjaxUpload($("#doupload"), {
            "responseType" : "json",
		    "name"         : "Filedata",
		    "onSubmit"     : uploadStart, 
		    "onComplete"   : uploadComplete
        });
    };
    initEvents();
    return api;
};

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
    
    init();
    
    api.currentPath = function () {
        return api.hashUrl.getParam("path") || document.location.pathname;
    };
    
    api.startPoll = function () {
        api.hashUrl = new HashUrl({"changed": urlChanged});
    };
    
    function init() {
        api.logger.start();
    };
    
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
        };
    };
    
    return api ;
};

var hn      = new HN.Site(),
    sheets  = [],
    layout  = null,
    toolbar = null,
    path    = document.location.pathname;

document.heightCalced = 0;

var initialLoad = function (initPath) {

    layout  = new HN.Layout(sheets[initPath]);
    toolbar = new HN.ToolBar(layout);

    HN.UI.init(layout);
    
    var page = hn.data.getPage(initPath);    
    toolbar.userNavigation(page.user);

    $("#loading, #cover").fadeOut("slow");
};

var makeTabs = function () {

    $("#tabs").empty();
    
    $.each(hn.data.getLoadedUrls(), function () {

        var name = (this == "/") ? document.location.host + "/" : this;

        $("<a href='" + this + "'>"+name+"</a>").click(function (e) {
            window.location.hash = 
                hn.hashUrl.setParam("path", $(this).attr("href"));
           return false;
        }).appendTo($("#tabs"));
    });
};

var updateTabs = function () {
    makeTabs();
    $("#tabs a").removeClass("current");
    $("#tabs a[href=" + hn.currentPath(HN.url) + "]").addClass("current");
};

var sheetDataLoader = function (sheetPath) {

    var perms;

    if (typeof sheets[sheetPath] === "undefined") {

        if (hn.finder) {
            hn.finder.addPath(HN.Util.pathToList(sheetPath));
        };

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

var hasLoaded = false;
var options = {

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
        "Sorry, your browser is "
            +"not supported yet.<br />Please try <a href='http://firefox.com'>"
            +"Firefox(3.6+)</a>, <a href='http://www.apple.com/safari/'>Safari</a>"
            +" or <a href='http://www.google.com/chrome'>Chrome</a><br />"
            +"<br />If you have just signed up, please check your email<br />"
            +"for instructions on how to log back in.");
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
    HN.Util.initLogin();

}