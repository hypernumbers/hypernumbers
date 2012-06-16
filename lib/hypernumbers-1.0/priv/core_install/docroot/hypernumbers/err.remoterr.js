/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global Err: false, console: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

Err = {};

Err.Remoterr = {};

Err.Remoterr.onerror = function (msg, errorfileurl, lineno) {

    var jsonstring, response, pageurl, cookies, ourfiles, ours, errorpath,
    retval;
    
    ourfiles = [
        "bootstrap/js/bootstrap.js",
        "bootstrap/js/bootstrap.min.js",
        "bootstrap/js/helper.js",
        "cleditor/jquery.cleditor.js",
        "cleditor/jquery.cleditor.min.js",
        "contact/hn.phone.js",
        "hypernumbers/2.0.0-crypto-md5.js",
        "hypernumbers/2.0.0-crypto-sha256.js",
        "hypernumbers/ajaxfileupload.js",
        "hypernumbers/err.remoterr.js",
        "hypernumbers/fb_lang.js",
        "hypernumbers/finder.js",
        "hypernumbers/hn.callbacks.js",
        "hypernumbers/hn.cell.js",
        "hypernumbers/hn.data.js",
        "hypernumbers/hn.demopage.js",
        "hypernumbers/hn.dialog.js",
        "hypernumbers/hn.hashurl.js",
        "hypernumbers/hn.js",
        "hypernumbers/hn.layout.js",
        "hypernumbers/hn.layout.pane.js",
        "hypernumbers/hn.layout.panes.js",
        "hypernumbers/hn.logger.js",
        "hypernumbers/hn.logs.js",
        "hypernumbers/hn.main.js",
        "hypernumbers/hn.map.js",
        "hypernumbers/hn.pagemenu.js",
        "hypernumbers/hn.renderpage.js",
        "hypernumbers/hn.rendertable.js",
        "hypernumbers/hn.richtext.js",
        "hypernumbers/hn.selection.js",
        "hypernumbers/hn.sheet.js",
        "hypernumbers/hn.sitedata.js",
        "hypernumbers/hn.site.js",
        "hypernumbers/hn.startupdebug.js",
        "hypernumbers/hn.toolbar.js",
        "hypernumbers/hn.ui.ctrlpanel.js",
        "hypernumbers/hn.ui.js",
        "hypernumbers/hn.ui.pagespanel.js",
        "hypernumbers/hn.userdeffns.js",
        "hypernumbers/hn.util.js",
        "hypernumbers/jquery-1.4.2.js",
        "hypernumbers/jquery-1.4.2.min.js",
        "hypernumbers/jquery-1.4.4.js",
        "hypernumbers/jquery-1.4.4.min.js",
        "hypernumbers/jquery-1.5.js",
        "hypernumbers/jquery-1.5.min.js",
        "hypernumbers/jquery-1.7.1.js",
        "hypernumbers/jquery-1.7.1.min.js",
        "hypernumbers/jquery.columnmanager.js",
        "hypernumbers/jquery.columnmanager.min.js",
        "hypernumbers/jquery.cookie.js",
        "hypernumbers/jquery.cookie.min.js",
        "hypernumbers/jquery-ext.js",
        "hypernumbers/jquery.filemenu.js",
        "hypernumbers/jquery.jstree.js",
        "hypernumbers/jquery.jstree.min.js",
        "hypernumbers/jquery.scrollbarWidth.js",
        "hypernumbers/jquery.scrollbarWidth.min.js",
        "hypernumbers/jquery.tablesorter.jfilterselect.js",
        "hypernumbers/jquery.tablesorter.jfilterselect.min.js",
        "hypernumbers/jquery.tablesorter.js",
        "hypernumbers/json2.js",
        "hypernumbers/json2.min.js",
        "jscolor/jscolor.js",
        "tooltip/jquery.tooltip.js",
        "tooltip/jquery.tooltip.min.js",
        "webcomponents/froogaloop.js",
        "webcomponents/hn.factory.js",
        "webcomponents/hn.google.js",
        "webcomponents/hn.htmlpanel.js",
        "webcomponents/hn.loadtemplates.js",
        "webcomponents/hn.mapcustom.js",
        "webcomponents/hn.maprows.js",
        "webcomponents/hn.mapsheet.js",
        "webcomponents/hn.newwebcomponents.js",
        "webcomponents/hn.siteadmin.js",
        "webcomponents/hn.toggle.js",
        "webcomponents/hn.twitter.js",
        "webcomponents/hn.usersandgroups.js",
        "webcomponents/hn.vimeo.js",
        "webcomponents/hn.webcomponents.js",
        "webcomponents/jquery.tabs.js",
        "webcomponents/jquery.ui.potato.menu.js",
        "webcomponents/timmenu.js"
    ];

    errorpath = errorfileurl.replace(window.location.href, "");
    // if it ain't oors it will have an index of -1
    ours = jQuery.inArray(errorpath, ourfiles);

    // get wierdo errors sometimes that mean hee-haw
    if ((errorfileurl === "") || (lineno === 0) || (ours === -1)) {
        // return true 'cos we don't think this is a real error
        response = "Someone elses bug. User not prompted";
        // Returning 'true' will clear the error in the browser
        retval =  true;
    } else {
        // Get some user input
        response = prompt("There has been an error. " +
                          "It has been logged and will be investigated.", 
                          "Put in comments (and e-mail or phone number for" + 
                          " response.)");
        // I don't want the page to 'pretend' to work 
        // so I am going to return 'false' here
        retval = false;
    }        
    // get some context of where and how the error occured
    // to make debugging easier
    pageurl = window.location.href;
    cookies = document.cookie;

    // Make the json message we are going to post
    // Could use JSON.stringify() here if you are sure that
    // JSON will have run when the error occurs
    // http://www.JSON.org/js.html
    jsonstring = "{\"set\": {\"jserr\": " +
        "{\"msg\": \""         + msg + "\", " + 
        "\"errorfileurl\": \"" + errorfileurl + "\", " +
        "\"pageurl\": \""      + pageurl + "\", " +
        "\"cookies\": \""      + cookies + "\", " +
        "\"lineno\": \""       + lineno + "\", " +
        "\"response\": \""     + response + "\"}}}";

    // Use the jquery cross-browser post
    // http://api.jquery.com/jQuery.post/
    // this assumes that no errors happen before jquery has initialised
    $.post("?jserr", jsonstring, null, "json");

    return retval;
};

Err.ConsoleErr = {};

Err.ConsoleErr.initialise = function () {
    if (typeof console === "undefined" || typeof console.log === "undefined") {
        console = {};
        console.log = function (msg) {
            // do nothing
        };
    } 
};
// Add Ajax errors as well
//$(document).ajaxError(function(e, xhr, settings) {
//  logError(settings.url + ':' + xhr.status + '\n\n' + xhr.responseText);
//});
window.onerror = Err.Remoterr.onerror;
Err.ConsoleErr.initialise();
