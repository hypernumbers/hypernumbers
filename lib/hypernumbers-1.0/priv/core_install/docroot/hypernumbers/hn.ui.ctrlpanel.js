/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global HN: false, hn: false, AjaxUpload: false, $: false, layout: false, alert: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false , toolbar: false */
HN.UI.CtrlPanel = function () {
    
    var api             = {},
        currentTab      = null,
        ajaxUpload      = null,
        currenttourpage = 1,
        datauploadvalidation = [],
        datauploadmapping = [],
        datauploadheading = "",
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

    api.loadMaps = function (maps) {
        var html = "";
        html = HN.Util.makeSelects(maps, "hn_maps", "maps");
        $("#hn_map_select").html(html);
    };
    
    api.loadTemplates = function (templates) {
        var html = "";
        html = HN.Util.makeSelects(templates, "templates", "templateslist");
        $("#templates").html(html);
        html = HN.Util.makeSelects(templates, "templates", "hn_map_templates");
        $("#hn_map_temp").html(html);
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
        parent.location.hash = '';
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

        api.initDataUpload = function () {

            // now do the pages panel
            if (!hn.is_admin) {
                $("#saveastemplate").attr("disabled", "disabled");
                $("#templatename").attr("disabled", "disabled");
            }

            // sort out the data upload panel
            bindMapEvents();
            // make the default template 'blank'
            $("#hn_map_templates").val("blank");
            buildMapHeader();

    };

    var makePath = function (str) { 
        var path = HN.Util.correctPath(str);
        hn.finder.addPath(HN.Util.pathToList(path));
        return path;
    };
    
    var newPage = function () { 
        var path = makePath($("#newpage").val()),
        template = $("#templateslist :selected").val();
        if (template !== "blank") {
            HN.Callbacks.loadTemplate(path, template);
        }
        hn.finder.openPageByPath(path); 
        //window.location.hash = hn.hashUrl.setParam("path", path);
        $("#newpage").val("");
        api.close();        
    };

    var uploadStart = function (file, extension) { 
        $("#doupload").attr("disabled", "disabled").val("Uploading ...");
        var path = $(".currentpageselection").first().val();
        ajaxUpload.setAttribute("action", path);
    };

    var uploadComplete = function (file, response) {
        if (response.error) {
            $("#doupload").attr("disabled", "").val("Upload");
            HN.Util.showDialog("upload failed: " + response.error);
        } else {
            document.location.href = response.location +
                "?view=spreadsheet";
        }
    };
    
    var buildMapHeader = function () {
        var map_type      = $("#hn_map_type").val(),
            map_file_type = $("#hn_map_file_type").val(),
            map_template  = $("#hn_map_templates").val(),
            map_overwrite = $("#hn_map_overwrite").val(),
            json= "",
            html;
        json = {type      : map_type,
                filetype  : map_file_type,
                template  : map_template,
                overwrite : map_overwrite};
        datauploadheading = json;
        html = HN.Util.objToHTML(json, 1);
        $("#hn_map_head").html(html);
        return json;
    };

    var setupFileType = function () {
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
    };

    var setupMapType = function () {
        var els  = $("#hn_validation, #hn_cell_from"),
            type = $("#hn_map_type").val();
        if (type === "sheet") {
            els.val("A1");
            els.attr("title", "use cell references like 'A1'");
            displayMapBody();
        } else if (type === "row") {
            els.val("A");
            els.attr("title", "use column references like A");
            displayMapBody();
        }            
    };

    var clearMapHeadAndBody = function () {
            datauploadvalidation = [];
            datauploadmapping = [];
        };

    var addValidation = function () {
        var sheet      = $("#hn_validation_sheet").val(),
            cell       = $("#hn_validation").val(),
            constraint = $("#hn_constraints").val(),
            type       = $("#hn_map_type").val(),
            json       = {sheet      : sheet,
                          cell       : make(cell, type),
                          constraint : constraint},
            len        = datauploadvalidation.length;
        if (isValid(cell, type)) {
            if (isNotIn(datauploadvalidation, json.sheet, json.cell, "cell")) {
                datauploadvalidation[len] = json;
                displayMapBody();
            } else {
                alert(json.sheet + "/" + json.cell +
                      " already has a validation");
            }
        } else {
            alert(cell + " is not a valid description");
        }
    };

        var isNotIn = function (array, sheet, cell, type) {
        for (i in array) {
            if (array.hasOwnProperty(i) &&
                array[i]["sheet"] === sheet &&
                array[i][type] === cell) { 
                return false;
            }
        }
        return true;
    };

    var isIn = function (array, sheet, cell) {
        for (i in array) {
            if (array.hasOwnProperty(i) &&
                array[i]["sheet"] === sheet &&
                array[i]["to"] === cell) { 
                return true;
            }
        }
        return false;
    };
    
    var isValid = function (cell, type) {
        var RegCell = /^[a-zA-Z]+[0-9]+$/,
            RegCol = /^[a-zA-Z]+$/;
        if (type === "sheet") {
            return RegCell.test(cell);
        } else if (type === "row") {
            return RegCol.test(cell);
        }
    };
    
    var displayMapBody = function () {
        var body = $("#hn_map_body"),
            html = "";
        body.html("");
        for (i in datauploadvalidation) {
            if (datauploadvalidation.hasOwnProperty(i)) {
                html = body.html(); 
                body.html(html + HN.Util.objToHTML(datauploadvalidation[i],
                                               "validation", i));
            }
        };
        for (i in datauploadmapping) {
            if (datauploadmapping.hasOwnProperty(i)) {
                html = body.html(); 
                body.html(html + HN.Util.objToHTML(datauploadmapping[i],
                                                   "mapping", i));
            }
        };            
    };

    var deleteElement = function () {
        var selected = $(".hn_selected").attr("data-index"),
        type = $(".hn_selected").attr("data-type");
        if (type === "validation") {
            datauploadvalidation.splice(selected, 1);
            } else if (type === "mapping") {
                datauploadmapping.splice(selected, 1);
            }
        displayMapBody();
    };
    
    var selectElement = function (e) {
        $(".hn_selected").removeClass("hn_selected");
        $(e.target).addClass("hn_selected");
    };
    
    var addMapping = function () {
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
        } else {
            if (isIn(datauploadmapping, json.sheet, json.to)) {
                alert(json.to + " already has a cell mapped to it");
                return;
            }
        }
        if (isValid(from, type)) {
            if (isNotIn(datauploadmapping, json.sheet, json.from, "from")) {
                datauploadmapping[len] = json;
                displayMapBody();
            } else {
                alert(json.sheet + "/" + json.from +
                      " is already mapped");
            }
        } else {
            alert(from + " is not a valid description");
        }
    };

    var make = function (ref, type) {
        if (type === "row") {
            return ref.toUpperCase() + ":" + ref.toUpperCase();
        } else if (type === "sheet") {
            return ref.toUpperCase();
        }
    };
    
    var bindMapEvents = function () {
        
        var selector1 = "#hn_map_type, #hn_map_file_type, " +
            "#hn_map_templates, #hn_map_overwrite",
            selector2 = "#hn_map_type, #hn_map_file_type";
        $(selector1).bind("change", function() { buildMapHeader(); });

        $("#hn_map_file_type").bind("change", function()
                                    { setupFileType(); });
        $(selector2).bind("change", function()
                          { clearMapHeadAndBody();
                            setupMapType(); });
        
        $("#hn_add_val").bind("click", function()
                              { addValidation(); });
        $("#hn_add_map").bind("click", function()
                              { addMapping(); });

        $("#hn_map_body").bind("click", function(e)
                               { selectElement(e); });

        $("#hn_map_delete").bind("click", function()
                                 { deleteElement(); });
        $("#hn_map_save").bind("click", function()
                               { saveMap(); });

        $("#hn_map_load").bind("click", function()
                               { loadMap(); });
    };

    var displayMap = function (data) {
        // first set up the headers
        $("#hn_map_type").val(data.map.head.type);
        $("#hn_map_file_type").val(data.map.head.filetype);
        $("#hn_map_templates").val(data.map.head.template);
        $("#hn_map_overwrite").val(data.map.head.overwrite);
        buildMapHeader();
        // now the body
        datauploadvalidation = data.map.validation;
        datauploadmapping = data.map.mapping;
        displayMapBody();
    };

    var loadMap = function () {
         var name = $("#hn_map_select :selected").val(),
            successFun = function(data) { displayMap(data); setupMapType(); };
        hn.map.loadMap(successFun, name);
    };
    
    var saveMap = function () {
        var json = {"head"       : datauploadheading,
                    "validation" : datauploadvalidation,
                    "mapping"    : datauploadmapping},
            name = $("#hn_map_name").val();
        if (name === "") {
            alert("Please supply a name for your data map.");
        } else {
            if (confirm("Are you sure you want to save this " +
                                     "map as " + name + "?")) {
                HN.Callbacks.saveMap(name, json);
            }
        }
    };
    
    var initEvents = function () {
        
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
                $("#saveasfeedback").html("<small>please enter " +
                                          "a name</small>");
                return;
            }
            if ($.inArray(templatename, hn.templates) < 0) {
                msgstr = "Are you sure that you want to save "+
                    path + " as "+ templatename + "?";
                } else {
                    msgstr = "Are you sure that you want to save " +
                        path + " as "+ templatename +
                        "?. It will overwrite the " +
                        "existing template of that name";
                }
            if (confirm(msgstr)) {
                HN.Callbacks.saveAsTemplate(path, templatename);
                $("#templatename").val("");
                $("#saveasfeedback").html("<small>" +
                                          templatename +
                                          " saved</small>");
            }
        });
        
        $("#ctrldelete").bind("mousedown", function () {
            var cb = function () {
                window.location.reload(true);
            };
            var pathStr = hn.finder.asString();
            if (confirm("Are you sure that you want to delete "+
                        pathStr+"?")) {
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
    };
    
    initEvents();
    return api;
};
