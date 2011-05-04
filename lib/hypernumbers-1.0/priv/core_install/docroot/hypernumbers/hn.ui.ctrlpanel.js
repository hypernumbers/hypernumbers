
/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, AjaxUpload: false, $: false, layout: false, alert: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false , toolbar: false */
HN.UI.CtrlPanel = function () {
    
    var api             = {},
        currentTab      = null,
        ajaxUpload      = null,
        currenttourpage = 1,
        datauploadvalidation = [],
        datauploadmapping = [],
        datauploadmap = [],
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
        html = HN.Util.makeTemplates("templateslist");
        $("#templates").html(html);
        html = HN.Util.makeTemplates("hn_map_templates");
        $("#hn_map_temp").html(html);
        // data upload needs to run after the templates have loaded/reloaded
        initDataUpload();
    };
    
    api.open = function () {
        if (currentTab) {
            api.selectTab(currentTab);
        } else {
            api.selectTab($("#ctrltabs li a").attr("data-tab"));
        }
            
        $("#cover, #ctrlpanel").show();
        if (hn.finder && hn.finder.hasOwnProperty("activate")) {
            hn.finder.activate();
        };
    
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
        if (hn.finder) {
            hn.finder.deactivate();
        }
        setTimeout(layout.resumeSelection, 0);
    };

    api.loadPages = function () {
        if (!hn.finder) {
            hn.finder = {};
            var pageSuccessFun = function (data) {
                hn.pages = data.pages;
                toolbar.loadPages();
                // finally allow the punters in
                $("#cover").css("z-index", 10000000);
                $("#loading").fadeOut("slow");
            };
            $("#cover").css("z-index", 10000003);
            $("#loading").fadeIn("fast");
            hn.sitedata.loadPageData(pageSuccessFun);
        }
    };
        
    api.selectTab = function (tab) {
        currentTab = tab;
        if (currentTab === "sitemypages") {
            api.loadPages();
        }
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
        //hn.finder.select(HN.Util.pathToList(path));
        return path;
    }
    
    function newPage() { 
        var path = makePath($("#newpage").val()),
        template = $("#templateslist :selected").val();
        if (template !== "blank") {
            HN.Callbacks.loadTemplate(path, template);
        }
        hn.finder.openPageByPath(path); 
        //window.location.hash = hn.hashUrl.setParam("path", path);
        $("#newpage").val("");
        api.close();        
    }

    function uploadStart(file, extension) { 
        $("#doupload").attr("disabled", "disabled").val("Uploading ...");
        var path = $(".currentpageselection").first().val();
        ajaxUpload.setAttribute("action", path);
    }

    function uploadComplete(file, response) {
        document.location.href = response.location +
            "?view=spreadsheet";
    }
    
    function buildMapHeader() {
        var map_type = $("#hn_map_type").val(),
            map_file_type = $("#hn_map_file_type").val(),
            map_template = $("#hn_map_templates").val(),
            map_overwrite = $("#hn_map_overwrite").val(),
            json= "",
            html;
        if (map_template === "") {
            json = {type      : map_type,
                    filetype  : map_file_type,
                    overwrite : map_overwrite};
        } else {
            json = {type      : map_type,
                    filetype  : map_file_type,
                    template  : map_template,
                    overwrite : map_overwrite};
        }
        html = HN.Util.objToHTML(json, 1);
        $("#hn_map_head").html(html);
        return json;
    }

    function setupFileType() {
        var els = $("#hn_validation_sheet, #hn_upload_sheet"),
        type = $("#hn_map_file_type").val();
        if (type === "csv") {
            els.val("csv");
            els.attr("title", "");
            els.attr("readonly", "readonly");
        } else if (type === "xls") {
            els.val("Sheet1");
            els.attr("title", "input the sheet name as it appears in Excel");
            els.attr("readonly", "");
        }
    }

    function setupMapType() {
        var els  = $("#hn_validation, #hn_cell_from"),
            type = $("#hn_map_type").val();
        if (type === "sheet") {
            els.val("A1");
            els.attr("title", "use cell references like 'A1'");
            datauploadvalidation = [];
            datauploadmapping = [];
            displayMapBody();
        } else if (type === "row") {
            els.val("A");
            els.attr("title", "use column references like A");
            datauploadvalidation = [];
            datauploadmapping = [];
            displayMapBody();
        }            
    }

    function addValidation() {
        var sheet      = $("#hn_validation_sheet").val(),
            cell       = $("#hn_validation").val(),
            constraint = $("#hn_constraints").val(),
            type       = $("#hn_map_type").val(),
            json       = {sheet      : sheet,
                          cell       : make(cell, type),
                          constraint : constraint},
            len        = datauploadvalidation.length;
        if (isValid(cell, type)) {
            if (isNotIn(datauploadvalidation, make(cell, type), "cell")) {
                datauploadvalidation[len] = json;
                displayMapBody();
            } else {
                alert(cell + " already has a validation");
            }
        } else {
            alert(cell + " is not a valid description");
        }
    }

    function isNotIn(array, cell, type) {
        for (i in array) {
            if (array[i][type] === cell) { 
                return false;
            }
        }
        return true;
    }
    
    function isValid(cell, type) {
        var RegCell = /^[a-zA-Z]+[0-9]+$/,
            RegCol = /^[a-zA-Z]+$/;
        if (type === "sheet") {
            return RegCell.test(cell);
        } else if (type === "row") {
            return RegCol.test(cell);
        }
    }
    
    function displayMapBody() {
        var body = $("#hn_map_body"),
            html = "";
        body.html("");
        for (i in datauploadvalidation) {
            html = body.html(); 
            body.html(html + HN.Util.objToHTML(datauploadvalidation[i],
                                               "validation", i));
        };
        for (i in datauploadmapping) {
            html = body.html(); 
            body.html(html + HN.Util.objToHTML(datauploadmapping[i],
                                               "mapping", i));
        };            
    }

    function deleteElement() {
        var selected = $(".hn_selected").attr("data-index"),
        type = $(".hn_selected").attr("data-type");
        if (type === "validation") {
            datauploadvalidation.splice(selected, 1);
            } else if (type === "mapping") {
                datauploadmapping.splice(selected, 1);
            }
        displayMapBody();
    }
    
    function selectElement(e) {
        $(".hn_selected").removeClass("hn_selected");
        $(e.target).addClass("hn_selected");
    }
    
    function addMapping() {
        var sheet = $("#hn_upload_sheet").val(),
            from  = $("#hn_cell_from").val(),
            to    = $("#hn_cell_to").val(),
            type  = $("#hn_map_type").val(),
            json  = {sheet : sheet,
                     from  : make(from, type),
                     to    : to},
            len   = datauploadmapping.length;
        if (!isValid(to, "sheet")) {
            alert(to + " is not a valid description");
        }
        if (isValid(from, type)) {
            if (isNotIn(datauploadmapping, make(from, type), "from")) {
                datauploadmapping[len] = json;
                displayMapBody();
            } else {
                alert(from + " already has a validation");
            }
        } else {
            alert(from + " is not a valid description");
        }
    }

    function make(ref, type) {
        if (type === "row") {
            return ref.toUpperCase() + ":" + ref.toUpperCase();
        } else if (type === "sheet") {
            return ref.toUpperCase();
        }
    }
    
    function bindMapEvents() {
        
        var selector = "#hn_map_type, #hn_map_file_type, " +
            "#hn_map_templates, #hn_map_overwrite";
        $(selector).bind("change", function() { buildMapHeader(); });

        $("#hn_map_file_type").bind("change", function() { setupFileType(); });
        $("#hn_map_type").bind("change", function() { setupMapType(); });
        
        $("#hn_add_val").bind("click", function() { addValidation(); });
        $("#hn_add_map").bind("click", function() { addMapping(); });

        $("#hn_map_body").bind("click", function(e) { selectElement(e); });

        $("#hn_map_delete").bind("click", function() { deleteElement(); });
        
        //console.log($("#hn_map_delete"));
        //console.log($("#hn_map_save"));
        //console.log($("#hn_map_select"));
    }
    function buildMapDropDown() {
        //console.log("building map dropdown");
    }
    
    function initDataUpload() {
        //console.log("setting up Data Upload");
        bindMapEvents();
        buildMapHeader();
        buildMapDropDown();
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
            path = $(".currentpageselection").first().val(),
            msgstr;
            if (templatename === "") {
                $("#saveasfeedback").html("<small>please enter a name</small>");
                return;
            }
            if ($.inArray(templatename, hn.templates) < 0) {
                msgstr = "Are you sure that you want to save "+ path +
                        " as "+ templatename + "?";
                } else {
                    msgstr = "Are you sure that you want to save "+ path +
                        " as "+ templatename + "?. It will overwrite the " +
                        "existing template of that name";
                }
            if (confirm(msgstr)) {
                HN.Callbacks.saveAsTemplate(path, templatename);
                $("#templatename").val("");
                $("#saveasfeedback").html("<small>" +
                                          templatename + " saved</small>");
            }
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
        
        $("#hn_newpageform").submit(function (e) {
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
