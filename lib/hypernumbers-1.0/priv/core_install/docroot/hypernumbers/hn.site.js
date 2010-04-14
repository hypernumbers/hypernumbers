// TODO: Get rid of globals, expose one for use through at the shell
HN.UI.CtrlPanel = function() {
    
    var public = {};
    
    var currentTab = null;
    var ajaxUpload = null;

    initEvents();
    
    public.open = function() {
        public.selectTab($("#ctrltabs li a").attr("data-tab"));
        $("#cover, #ctrlpanel").show();
        hn.finder.activate();
        layout.grabFocus();
    };

    public.close = function() { 
        $("#cover, #ctrlpanel").hide();
        hn.finder.deactivate();
        setTimeout( function() {
            layout.resumeSelection();
        }, 0);
    };

    public.selectTab = function(tab) { 
        currentTab = tab;
        $("#ctrltabs li a.selected").removeClass("selected");
        $("#ctrltabs li a[data-tab="+tab+"]").addClass("selected");
        $("#ctrltabdivs > div").hide();
        $("#ctrltabdivs div#"+tab).show();
    };

    public.pageSelected = function(path) { 
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

    function newPage() { 

        var path = HN.Util.correctPath( 
            hn.finder.asString() + $("#newpage").val() );
        window.location.hash = hn.hashUrl.setParam("path", path);
        
        hn.finder.addPath(HN.Util.pathToList(path));
        hn.finder.select(HN.Util.pathToList(path));
        
        $("#newpage").val("");
        public.close();        
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

    function addUser(user, pass) {         
        HN.Callbacks.addUser(user, pass, function(data) { 
            $("#newusername, #newuserpass").val("");
            $("#donewuser").removeAttr("disabled").val("Add User");
            $("#newuserfeedback")
                .append("<p>User <strong>"+user+
                        "</strong> has been added.</p>");
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
                             function(data) { 
                                 console.log(data);
                             });             
    };

    function initEvents() {

        $("#newpage, #newusername, #newuserpass").val("");
        $("#doupload, #donewuser").removeAttr("disabled");

        $("#ctrldelete").bind("mousedown", function() {
            var cb = function() {
                window.location.reload(true);
            };
            HN.Util.postPath(hn.finder.asString(), {"delete": "all"}, cb);
        });

        $("#homebutton").bind("mousedown", function() { 
            public.open();
        });
        
        $("#newusers").bind("submit", function(e) { 
            e.preventDefault();
            $("#donewuser").attr("disabled", "disabled")
                .val("Adding User ...");
            addUser($("#newusername").val(), $("#newuserpass").val());
        });
        
        $("#ctrlclose").bind("mousedown", function() { 
            public.close();
        });
        
        $("#ctrltabs").bind("mousedown", function(e) { 
            if( e.target.nodeName == "A" ) {
                public.selectTab(e.target.getAttribute("data-tab"));
            }
        });
        
        $("#newpageform").submit( function(e) {
            e.preventDefault(); 
            newPage();
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

    return public;
};

HN.Site = function() {
    
    var public = {};
    
    public.hashUrl = null;
    public.finder  = null;
    public.logger  = new HN.Logger();
    public.ctrlPanel = new HN.UI.CtrlPanel();
    public.data    = null;
    
    init();
    
    public.currentPath = function() {
        return public.hashUrl.getParam("path") || document.location.pathname;
    };

    public.startPoll = function() {
        public.hashUrl = new HashUrl({"changed": urlChanged});
    };
    
    function init() {
        public.logger.start();
    };
    
    function urlChanged() {
        
        var view = public.hashUrl.getParam("view");
        var path = public.hashUrl.getParam("path");
        
        if( public.hashUrl.getParam("path") ) { 
            sheetDataLoader(path); 
        }
        
        if(!builder) { 
            return;
        }

        if( view == "create" && builder.state() != "CREATE" ) {
            
            if( builder.state() == "RENDER" ) {
                $("#mainviewbuilder").hide();
                builder.saveCurrentView(true, null, true);
            }

            $("#viewbuilderintro, #closeviewbtn").show();

            builder.viewCreate();
            layout.state = layout.states.MOST_VIEWBUILDER;
            layout.gridResize();

        } else if( view ) {

            var tmp      = view.split(":");
            var url      = tmp[0];
            var filepath = tmp[1];

            if( !builder.isLoaded(url, filepath) ) {   

                if( builder.state() == "RENDER" ) {
                    builder.saveCurrentView(true, null, true);
                }

                $("#mainviewbuilder, #closeviewbtn").show();

                builder.loadTplFile(url, filepath);
                layout.state = layout.states.MOST_VIEWBUILDER;
                layout.gridResize();
            }
        } else {

            if( builder.state() == "RENDER" ) {
                builder.saveCurrentView(true, null, true);
            }
            builder.close();

            $("#viewbuilderintro, #mainviewbuilder, #closeviewbtn").hide();
            layout.state = layout.states.FULL_SPREADSHEET;
            layout.gridResize();
        }
    };
    
    return public;
};

var hn = new HN.Site();
var sheets = [], layout = null, toolbar = null;

var path  = document.location.pathname;

var initialLoad = function(initPath) {

    layout  = new HN.Layout(sheets[initPath]);
    toolbar = new HN.ToolBar(layout);
    builder = new HN.Builder(hn.data, layout);
    
    var lang = sheets[initPath].key("lang");
    hn.data.loadOpaque("/hypernumbers/fns_"+lang+".json", "functions");
    
    HN.UI.init(layout);
    
    var page = hn.data.getPage(initPath);    
    toolbar.user_navigation(page.user);
    
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
        tab.c.start_edit(original);
        tab.state = HN.States.EDIT_FULL_CELL;
        return false;
    });
};

var makeTabs = function() {    
    $("#tabs").empty();
    $.each(hn.data.getLoadedUrls(), function() {
        $("<a href='"+this+"'>"+this+"</a>").click( function(e) {            
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

var updateViewMenu = function() { 

    // $("#existingviews a[name=view]").remove();
    // $.each(hn.data.key(path, "views"), function() {
    //     if( this.match("/core/") ) {
    //         return;
    //     }
    //     var name     = this.replace(".tpl", "");
    //     var fullpath = path+":"+name;
    //     var $item    = $("<li><a name='view' data-path='"+fullpath+"' "
    //                      +"date-type='view'>"+name+"</a></li>");
    //     $("#existingviews").append($item);
    // });    
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
        
        updateViewMenu();        
        sheets[path].processLoaded();
        layout.switchTab(sheets[path]);

        HN.Util.id("copied").style.display = "none"; 
            
        if( hn.finder ) {
            hn.finder.select(HN.Util.pathToList(path));
        }
    }

    if( builder.isRendering() ) {
        builder.render();
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
    
    "dataReloaded": function(data) {
        data.path = path;
        sheets[path].reload_data(data);
        layout.dataUpdated();
    },
    
    "update": function() {
        sheets[path].processLoaded();
        layout.dataUpdated();
        if( builder.isRendering() ) {
            builder.render();
        }
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
    hn.data.loadOpaque(document.location.pathname, "views");
    hn.data.addPage(path);
}