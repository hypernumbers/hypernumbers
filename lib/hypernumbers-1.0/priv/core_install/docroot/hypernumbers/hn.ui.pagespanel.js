/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, hn: false, AjaxUpload: false, $: false, layout: false, alert: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false , toolbar: false  */
HN.UI.PagesPanel = function () {
    
    var api = {};
    
    api.loadTemplates = function (templates) {

        var html = "", selectTempFn;
        
        html = HN.Util.makeSelects(templates, "templates", "templateslist2");
        $("#templates2").html(html);

        selectTempFn = function (e) {
            var selection = $("#templates2 option:selected").text();
            $("#templatename").attr("value", selection);
        };
        
        $("#templateslist2").bind("change", selectTempFn);

    };
    
    api.initPagesPanel = function () {

        var uploadStartFn, uploadCompleteFn, ajaxUpload,
        templateKeyDownFn, saveAsTempFn;

        uploadStartFn = function (file, extension) { 
            $("#uploadxls").attr("disabled", "disabled")
                .val("Uploading ...");
            var path = hn.currentPath();
            ajaxUpload.setAttribute("action", path);
        };
        
        uploadCompleteFn = function (file, response) {
            if (response.error) {
                $("#upload").attr("disabled", "").val("Upload");
                HN.Util.showDialog("upload failed: " + response.error);
            } else {
                document.location.href = response.location +
                    "?view=spreadsheet";
            }
        };

        templateKeyDownFn = function (e) {
            if (null == e) {
                e = window.event;
            }

            if (e.keyCode == 13)  {
                $("#saveastemplate").mousedown(); 
            }
        };
        
        saveAsTempFn = function (e) {

            var templatename = $("#templatename").val(),
            path = hn.currentPath(), strPath,
            msgstr;

            e.preventDefault();

            if (path === "/") {
                strPath = "(Home Page)";
            } else {
                strPath = path;
            }
                       
            if (templatename === "") {
                $("#saveasfeedback").html("<small>please enter " +
                                          "a name</small>");
                return "";
            }

            if ($.inArray(templatename, hn.templates) < 0) {
                msgstr = "Are you sure that you want to save " +
                    strPath + " as " + templatename + "?";
            } else {
                msgstr = "Are you sure that you want to save " +
                    strPath + " as " + templatename +
                    "?. It will overwrite the " +
                        "existing template of that name";
            }
            
            if (confirm(msgstr)) {
                HN.Callbacks.saveAsTemplate(path, templatename);
                $("#templatename").val("");
                $("#saveasfeedback").html("<small>" + templatename +
                                          " saved</small>");
            }
        };

        ajaxUpload = new AjaxUpload($("#uploadxls"), {
                                        "responseType" : "json",
		                                    "name"         : "Filedata",
		                                    "onSubmit"     : uploadStartFn, 
		                                    "onComplete"   : uploadCompleteFn
                                    });
        
        $("#templatename").bind("keydown", templateKeyDownFn); 

        $("#saveastemplate").bind("mousedown", saveAsTempFn);

        $("#pagesclose").bind("click", api.dialogclose);
    };

    api.open = function (type) {

        var title, path = hn.currentPath();

        $("#cover, #pagespanel, #hn-small-dialog-hdr").show();
        $("#" + type + ", #" + type + " div, #" + type + " form").show();

        if (path === "/") {
            path = "(Home Page)";
        }
        
        // now do the title, etc
        if (type === "sitesaveastemplate") {
            title = "Save Page As Template";
            $("#hn_save_as_temp_text").text("Save page " + path + " " +
                                            "as a new template or overwrite " +
                                            "an existing template");
            layout.grabFocus();
            $("#templatename").focus();
        } else if (type === "siteuploadxls") {
            title = "Upload Excel .xls File";
            $("#hn_uploadxls_details").text("The file will be uploaded " + ""
                                            + "to the page " + path);
        }
        $("#hn-small-dialog-hdr > span").text(title);
    };
    
    api.dialogclose = function () {
        $("#pagespanel div, #pagespanel form").hide();
        $("#cover, #pagespanel").hide();        
        setTimeout(layout.resumeSelection, 0);
    };

    return api;
};
