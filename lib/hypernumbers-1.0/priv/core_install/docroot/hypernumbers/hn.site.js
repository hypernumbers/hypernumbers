// TODO: Get rid of globals, expose one for use through at the shell
HN.UI.CtrlPanel = function () {
    
    var api        = {},
        currentTab = null,
        ajaxUpload = null,
        currenttourpage = 1,
        tourpages = $("#tourwrapper").children().length;
    
    initEvents();

    api.show_tour_page = function(x) {
        currenttourpage = x;
        HN.Util.id("tourpager").innerHTML = "Page "+currenttourpage
            +" of "+tourpages+" Pages";
        $("#tourwrapper").children().hide();
        $("#tourwrapper").children().eq(x-1).show();
    };
    
    api.open = function () {
        if (currentTab) {
            api.selectTab(currentTab);
        } else {
            api.selectTab($("#ctrltabs li a").attr("data-tab"));
        }
            
        $("#cover, #ctrlpanel").show();
        hn.finder.activate();
    
        // Now set up the tour
        $("#tournext").click( function() {
                                  var id = ( currenttourpage == tourpages )
                                      ? 1 : currenttourpage+1;
                                  api.show_tour_page(id);
                              });
        
        $("#tourback").click( function() {
                                  var id = ( currenttourpage == 1 )
                                      ? tourpages : currenttourpage-1;
                                  api.show_tour_page(id);
                              });
        
        api.show_tour_page(1);
        layout.grabFocus();
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
    };

    api.pageSelected = function (path) { 
        var pathStr = HN.Util.listToPath(path);
        $(".currentpageselection").val(pathStr).text(pathStr);
        $("#visitlink").attr("href", pathStr);        
    };

    function makePath(str) { 
        var path = HN.Util.correctPath( str );
        hn.finder.addPath(HN.Util.pathToList(path));
        hn.finder.select(HN.Util.pathToList(path));
        return path;
    };
    
    function newPage() { 
        var path = makePath( $("#newpage").val() );
        window.location.hash = hn.hashUrl.setParam("path", path);
        $("#newpage").val("");
        api.close();        
    };

    function newWebPage() {
        
        var path = makePath( $("#newwebpage").val() );

        var done = function (data) {
            window.location.hash = hn.hashUrl.setParam("path", path);
            $("#newwebpage").val("");
            api.close();        
        };

        var done1 = function (data) {
            HN.Callbacks.setView(HN.Util.pathToList(path), "webpage",
                                 {"everyone":true, "groups":[]}, done);
        };
        
        HN.Callbacks.setChampion(HN.Util.pathToList(path), 
                                 "webpage", done1);
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
                    .append("<div class='error'>Error: " + data.reason +
                            "</div>");
            }
            $("#passwordval").val("");
            setTimeout(function () { 
                $("#settingsfeedback").empty();
            }, 2000);
        });
    };
    
    function initEvents () {

        HN.Util.initLogin();

        $("#newpage, #newwebpage, #newusername, #passwordval").val("");
        $("#doupload, #donewuser").removeAttr("disabled");
        
        $("#ctrldelete").bind("mousedown", function () {
            var cb = function () {
                window.location.reload(true);
            };
            HN.Util.postPath(hn.finder.asString(), {"delete": "all"}, cb);
        });

        $("#homebutton").bind("mousedown", function () { 
            api.open();
        });
        
        $("#passwordform").bind("submit", function (e) { 
            e.preventDefault();
            setPassword( $("#passwordval").val() );
        });
        
        $("#ctrlclose").bind("mousedown", function () { 
             api.close();
         });
        
        $("#ctrltabs").bind("mousedown", function (e) { 
            if( e.target.nodeName == "A" ) {
                api.selectTab(e.target.getAttribute("data-tab"));
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

    return api;
};

HN.Site = function () {
    
    var api = {};
    
    api.hashUrl   = null;
    api.finder    = null;
    api.logger    = new HN.Logger();
    api.ctrlPanel = new HN.UI.CtrlPanel();
    api.data      = null;
    
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
            path  = api.hashUrl.getParam("path"),
            panel = api.hashUrl.getParam("panel");
        
        if (api.hashUrl.getParam("path")) { 
            sheetDataLoader(path); 
        }
        
        if (panel) { 
            hn.ctrlPanel.selectTab("settings");
            hn.ctrlPanel.open();
        }
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
    
    var lang = sheets[initPath].key("lang");
    hn.data.loadOpaque("/hypernumbers/funs_" + lang + ".json", "functions");
    
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
        toolbar.loadViews(sheets[path].pageData().data.permissions, 
                          hn.data.groups);

        if (perms.champion) {
            $("#newviewas input[value="+perms.champion+"]")
                .attr("checked", "checked");
        } else {
            $("#newviewas input[type=radio]").removeAttr("checked");
        };
        // first clear all attributes
        $(".groupinput").removeAttr("checked");
        for (p in perms.views) {
            perms.views[p].everyone ? 
                $("#newpublic"+p).attr("checked", "checked") 
                : $("#newpublic"+p).removeAttr("checked");
            for (g in perms.views[p].groups) {
                var grp = perms.views[p].groups[g];
                if (grp !== "admin") {
                    var id = p+"_"+grp;
                    $("#"+id).attr("checked", "checked");
                };
            };
        };
        
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
        sheets[path] = new HN.Sheet(path, hn.data);
        hn.data.groups = sheets[path].pageData().groups;
        hn.data.is_admin = sheets[path].pageData().data.is_admin;

        if (!hasLoaded) {
            hn.startPoll();
            initialLoad(path);
            hasLoaded = true;
        }
        sheetDataLoader(path);
    },
    
    "opaqueLoaded": function (name, data) {
        
        if( name == "pages" ) {
            
            var opts = {
                "menuPicked"   : function () { hn.ctrlPanel.open(); },
                "activate"     : function () { },
                "deactivate"   : function () { },
                "itemSelected" : function () { 
                    hn.ctrlPanel.pageSelected(this.asList());
                },
                "itemChosen"   : function () {
                    var newPath = this.asString();
                    window.location.hash =
                        hn.hashUrl.setParam("path", newPath);
                    hn.ctrlPanel.close();
                }
            };
            
            hn.finder = new Finder($("#finder").eq(0), data, opts);
            hn.finder.select(HN.Util.pathToList(path));

            $("#pdvisit").bind("click", function () {
                hn.finder.chooseCurrent();
            });

        } else if (name === "functions") {
            toolbar.loadFunctions(data);
        //} else if (name === "views") {
        //    for (var i in data) {                
        //        $("#currentdefaultview")
        //            .append("<option>" + data[i] + "</option>");
        //    }
        }
    },
    
    "dataReloaded": function (data) {
        data.path = path;
        sheets[path].reload_data(data);
        layout.dataUpdated();
        hn.data.groups = data.groups;
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


if( $.browser.msie || $.browser.opera ) {
    $("#loading").html(
        "Sorry, your browser is "
            +"not supported yet.<br />Please try <a href='http://firefox.com'>"
            +"Firefox(3.6+)</a>, <a href='http://www.apple.com/safari/'>Safari</a>"
            +" or <a href='http://www.google.com/chrome'>Chrome</a><br />"
            +"<br />If you have just signed up, please check your email<br />"
            +"for instructions on how to log back in.");
} else {
    hn.data = new HN.Data(options);
    hn.data.loadOpaque(document.location.pathname, "pages");
    hn.data.addPage(path);
}