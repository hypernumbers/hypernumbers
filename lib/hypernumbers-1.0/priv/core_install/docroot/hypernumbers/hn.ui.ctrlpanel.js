/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, AjaxUpload: false, $: false, layout: false, alert: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false , toolbar: false */
HN.UI.CtrlPanel = function () {
    
    var api             = {},
        currentTab      = null,
        ajaxUpload      = null,
        currenttourpage = 1,
        tourpages = $("#tourwrapper").children().length;
    
    api.show_tour_page = function (x, firemark) {
        currenttourpage = x;
        HN.Util.id("tourpager").innerHTML = "Page " + currenttourpage +
            " of " + tourpages + " Pages";
        $("#tourwrapper").children().hide();
        $("#tourwrapper").children().eq(x - 1).show();
        if (firemark) {
            HN.Callbacks.setMark("viewing page " + x + " of the tour");
        }
    };

    api.loadTemplates = function (templates) {
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
        }
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
        var path = HN.Util.correctPath(str);
        hn.finder.addPath(HN.Util.pathToList(path));
        hn.finder.select(HN.Util.pathToList(path));
        return path;
    }
    
    function newPage() { 
        var path = makePath($("#newpage").val()),
        template = $("#templateslist :selected").val();
        if (template !== "blank") {
            HN.Callbacks.loadTemplate(path, template);
        }
        
        window.location.hash = hn.hashUrl.setParam("path", path);
        $("#newpage").val("");
        api.close();        
    }

    function uploadStart(file, extension) { 
        $("#doupload").attr("disabled", "disabled").val("Uploading ...");
        ajaxUpload.setAttribute("action", hn.finder.asString());
    }

    function uploadComplete(file, response) {
        document.location.href = response.location +
            "?view=spreadsheet";
    }
    
    function initEvents() {
        
        // Now set up the tour
        $("#tournext").click(function () {
            var id = (currenttourpage === tourpages) ?
                1 : currenttourpage + 1;
            api.show_tour_page(id, true);
        });
        
        $("#tourback").click(function () {
            var id = (currenttourpage === 1) ?
                tourpages : currenttourpage - 1;
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
            }
            HN.Callbacks.saveAsTemplate(path, templatename);
            $("#templatename").val("");
            $("#saveasfeedback").html("<small>" + templatename + " saved</small>");
        });
        
        $("#ctrldelete").bind("mousedown", function () {
            var cb = function () {
                window.location.reload(true);
            };
            var pathStr = hn.finder.asString();
            if (confirm("Are you sure that you want to delete "+pathStr+"?")) {
		HN.Util.postPath(pathStr, {"delete": "all"}, cb);		
	    }
        });
        
        $("#passwordform").bind("submit", function (e) { 
            e.preventDefault();
            HN.Util.setPassword($("#hn_passwordval").val());
        });
        
        $("#ctrlclose").bind("mousedown", function () { 
            api.close();
        });
        
        $("#ctrltabs").bind("mousedown", function (e) { 
            e.preventDefault();
            if (e.target.nodeName === "A") {
                api.selectTab(e.target.getAttribute("data-tab"));
                api.open();
            }
        });
        
        $("#newpageform").submit(function (e) {
            e.preventDefault(); 
            newPage();
        });
        
        //$("#newwebpageform").submit(function (e) {
        //    e.preventDefault(); 
        //    newWebPage();
        //});
        
        ajaxUpload = new AjaxUpload($("#doupload"), {
            "responseType" : "json",
		        "name"         : "Filedata",
		        "onSubmit"     : uploadStart, 
		        "onComplete"   : uploadComplete
        });
    }
    initEvents();
    return api;
};
