// TODO: Get rid of globals, expose one for use through at the shell
HN.UI.CtrlPanel = function() {
    
    var api = {};
    
    var currentTab = null;
    var ajaxUpload = null;
    
    initEvents();
    
    api.open = function() {
        if (currentTab) {
            api.selectTab(currentTab);
        } else {
            api.selectTab($("#ctrltabs li a").attr("data-tab"));
        }
            
        $("#cover, #ctrlpanel").show();
        hn.finder.activate();
        layout.grabFocus();
    };

    api.close = function() { 
        $("#cover, #ctrlpanel").hide();
        hn.finder.deactivate();
        setTimeout( function() {
            layout.resumeSelection();
        }, 0);
    };

    api.selectTab = function(tab) { 
        currentTab = tab;
        $("#ctrltabs li a.selected").removeClass("selected");
        $("#ctrltabs li a[data-tab="+tab+"]").addClass("selected");
        $("#ctrltabdivs > div").hide();
        $("#ctrltabdivs div#"+tab).show();
    };

    api.pageSelected = function(path) { 
        var pathStr = HN.Util.listToPath(path);
        $(".currentpageselection").val(pathStr).text(pathStr);
        $("#visitlink").attr("href", pathStr);
        
        var perms = hn.data.key(pathStr, "permissions");
        
        var update = function(perms) { 

            $("#currentdefaultview").val(perms.champion);
            $("#viewusers, #viewgroups").empty();
            if( perms.champion ) {
                var viewPerms = perms.views[perms.champion];
                $("#everyoneview").attr("checked", viewPerms.everyone);
                $("#viewgroups").append(viewPerms.groups.join(", "));
            } else {
                $("#everyoneview").attr("checked", false);
            }
        };
        
        if( !perms ) {
            $.ajax({
                "url"      : pathStr + "?permissions",
                "dataType" : "json",
                "success"  : function(data) {
                    hn.data.writeKey(pathStr, "permissions", data);
                    update(data);
                }
            });
        } else {
            update(perms);
        }        
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

        var done = function(data){
            window.location.hash = hn.hashUrl.setParam("path", path);
            $("#newwebpage").val("");
            api.close();        
        };

        HN.Callbacks.setChampion(HN.Util.pathToList(path), 
                                 "_g/core/webpage", done);
    };

    function uploadStart(file, extension) { 
        $("#doupload").attr("disabled", "disabled").val("Uploading ...");
        ajaxUpload.setAttribute("action", hn.finder.asString());
    };

    function uploadComplete(file, responce) { 

        var newPath = HN.Util.pathToList(responce.location);
        hn.finder.addPath(newPath);
        hn.finder.select(newPath);
        
        $("#uploadresponce").empty()
            .append("Uploaded: <strong>"+responce.location+"</strong> ");
        $("#doupload").removeAttr("disabled").val("Upload");
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
            setTimeout(function() { 
                $("#settingsfeedback").empty();
            }, 2000);
        });
    };
    
    function defaultViewChanged(path, view) { 
        HN.Callbacks.setChampion(path, view, function(data) { 
            hn.data.deleteKey(HN.Util.listToPath(path), "permissions");
            $("#currentdefaultview")[0].blur();
        });
    };

    function setPermissionsEveryone(path, everyone) { 
        
        var perms = hn.data.key(HN.Util.listToPath(path), "permissions");

        if( !perms.champion ) { 
            alert("cant set blank page permissions");
        }
        
        var champion      = perms.views[perms.champion];
        champion.everyone = everyone;
        
        HN.Callbacks.setView(path, perms.champion, champion, 
                             function(data) { });             
    };

    function initEvents () {

        $("#newpage, #newwebpage, #newusername, #passwordval").val("");
        $("#doupload, #donewuser").removeAttr("disabled");
        
        $("#ctrldelete").bind("mousedown", function() {
            var cb = function() {
                window.location.reload(true);
            };
            HN.Util.postPath(hn.finder.asString(), {"delete": "all"}, cb);
        });

        $("#homebutton").bind("mousedown", function() { 
            api.open();
        });
        
        $("#passwordform").bind("submit", function(e) { 
            e.preventDefault();
            setPassword( $("#passwordval").val() );
        });
        
        $("#ctrlclose").bind("mousedown", function() { 
            api.close();
        });
        
        $("#ctrltabs").bind("mousedown", function(e) { 
            if( e.target.nodeName == "A" ) {
                api.selectTab(e.target.getAttribute("data-tab"));
            }
        });
        
        $("#newpageform").submit( function(e) {
            e.preventDefault(); 
            newPage();
        });
        $("#newwebpageform").submit( function(e) {
            e.preventDefault(); 
            newWebPage();
        });
        $("#newformform").submit( function(e) {
            e.preventDefault(); 
            newForm();
        });


        $("#everyoneview").bind("change", function() { 
            setPermissionsEveryone(hn.finder.asList(), $(this).is(":checked"));
        });

        $("#currentdefaultview").bind("change", function() { 
            defaultViewChanged(hn.finder.asList(), 
                               $("#currentdefaultview").val());
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

HN.Site = function() {
    
    var api = {};
    
    api.hashUrl   = null;
    api.finder    = null;
    api.logger    = new HN.Logger();
    api.ctrlPanel = new HN.UI.CtrlPanel();
    api.data      = null;
    
    init();
    
    api.currentPath = function() {
        return api.hashUrl.getParam("path") || 
            document.location.pathname;
    };
    
    api.startPoll = function() {
        api.hashUrl = new HashUrl({"changed": urlChanged});
    };
    
    function init() {
        api.logger.start();
    };
    
    function urlChanged() {
        
        var view  = api.hashUrl.getParam("view");
        var path  = api.hashUrl.getParam("path");
        var panel = api.hashUrl.getParam("panel");
        
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

var hn = new HN.Site();
var sheets = [], layout = null, toolbar = null;

var path  = document.location.pathname;

var initialLoad = function(initPath) {

    layout  = new HN.Layout(sheets[initPath]);
    toolbar = new HN.ToolBar(layout);
    
    var lang = sheets[initPath].key("lang");
    hn.data.loadOpaque("/hypernumbers/fns_"+lang+".json", "functions");
    
    HN.UI.init(layout);
    
    var page = hn.data.getPage(initPath);    
    toolbar.userNavigation(page.user);
    
    $("#loading, #cover").fadeOut("slow");
};

var showError = function(ind, original) {
    
    var sheet = this.layout.currentSheet();
    var tab   = this.layout.tabUI();
    var cell  = tab.currentSelectedCell();

    HN.UI.open_dialog(layout, "error");

    $("#errorform").unbind().bind("submit", function(e) {
        e.preventDefault();
        HN.UI.close_dialog(layout, "error");
        // TODO : eugh, leaky
        tab.set_cell(ind.y, ind.x);
        tab.bounds = {"x1":ind.x, "x2":ind.x, "y1":ind.y, "y2":ind.y};
        tab.show_selection();
        tab.editor.start_edit(original);
        tab.state = HN.States.EDIT_FULL_CELL;
        return false;
    });
};

var makeTabs = function () {
    
    $("#tabs").empty();
    $.each(hn.data.getLoadedUrls(), function() {

        var name = (this == "/") ? 
            document.location.host+"/" : this;

        $("<a href='"+this+"'>"+name+"</a>").click( function(e) {            
            window.location.hash = 
                hn.hashUrl.setParam("path", $(this).attr("href"));
           return false;
        }).appendTo($("#tabs"));
    });
};

var updateTabs = function() {
    makeTabs();
    $("#tabs a").removeClass("current");
    $("#tabs a[href="+hn.currentPath(HN.url)+"]").addClass("current");
};

var sheetDataLoader = function(sheetPath) {
    
    if( typeof sheets[sheetPath] == "undefined" ) {

        if( hn.finder ) {
            hn.finder.addPath(HN.Util.pathToList(sheetPath));
        };

        return hn.data.addPage(sheetPath);
    }

    updateTabs();
    
    if( sheetPath == hn.currentPath(HN.url) ) {
        
        path = sheetPath;
        document.title = path + " - hypernumbers";

        $("#currentViewPath").text(path);
        
        sheets[path].processLoaded();
        layout.switchTab(sheets[path]);

        HN.Util.id("copied").style.display = "none"; 
            
        if( hn.finder ) {
            hn.finder.select(HN.Util.pathToList(path));
        }
    }
};

var hasLoaded = false;
var options = {

    "authError": function(path) {
        window.location.hash = 
            hn.hashUrl.setParam("path", document.location.pathname);
        alert("Sorry, you do not have permission to access: "+path);
    },

    "dataLoaded": function(path) {
        
        sheets[path] = new HN.Sheet(path, hn.data);
        
        if( !hasLoaded ) {
            hn.startPoll();
            initialLoad(path);
            hasLoaded = true;
        }
        sheetDataLoader(path);
    },
    
    "opaqueLoaded": function(name, data) {
        
        if( name == "pages" ) {
            
            var opts = {
                
                "menuPicked"     : function() { 
                    hn.ctrlPanel.open();
                },
                "activate"     : function() { 
                },
                "deactivate"   : function() { 
                },
                "itemSelected" : function() { 
                    hn.ctrlPanel.pageSelected(this.asList());
                },
                "itemChosen"   : function() {
                    var newPath = this.asString();
                    window.location.hash = hn.hashUrl.setParam("path", newPath);
                    hn.ctrlPanel.close();
                }
            };
            
            hn.finder = new Finder($("#finder").eq(0), data, opts);
            hn.finder.select(HN.Util.pathToList(path));

            $("#pdvisit").bind("click", function() {
                hn.finder.chooseCurrent();
            });
            
            
        } else if( name == "functions" ) {
            toolbar.loadFunctions(data);
        } else if( name == "views" ) {
            for( var i in data ) {                
                $("#currentdefaultview")
                    .append("<option>"+data[i]+"</option>");
            }
        }
    },

    "cellValueChanged": function(cell) {
        sheets[cell.path].cellChanged(cell.y, cell.x);        
    },

    "dataReloaded": function(data) {
        data.path = path;
        sheets[path].reload_data(data);
        layout.dataUpdated();
    },
    
    "update": function() {
        sheets[path].processLoaded();
        layout.dataUpdated();
    },

    "formulaError": function(index, formula) {
        showError(index, formula);
    }
};


if( $.browser.msie || $.browser.opera ) {
    $("#loading").html(
        "<strong>Error: </strong>Sorry, your browser is "
            +"not supported yet.<br />Please try <a href='http://firefox.com'>"
            +"Firefox</a>, <a href='http://www.apple.com/safari/'>Safari</a>"
            +" or <a href='http://www.google.com/chrome'>Chrome</a>");
} else {
    hn.data = new HN.Data(options);
    hn.data.loadOpaque(document.location.pathname, "pages");
    hn.data.addPage(path);
}