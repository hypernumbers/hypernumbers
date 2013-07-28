HN.APIKeys = {};

HN.APIKeys.model = {};

HN.APIKeys.update_api = function (api, key) {
    HN.APIKeys.model[key] = api;
    var args, path;
    args = {"update_api_key": api};
    path = document.location.pathname + $(".hn_user_admin").parent().attr("data-ref");
    HN.Util.postPath(path, args, null);
};

HN.APIKeys.changed = function(e) {
    var key, url, type, api, newapi, newurls;
    e.stopPropagation();
    key = parseInt($(e.currentTarget).attr("data-api-no"), 10);
    url = parseInt($(e.target).attr("data-api-url-no"), 10);
    type = $(e.target).attr("data-api-type");
    api = HN.APIKeys.model[key];
    if (type === "notes") {
        api.notes = e.target.value;
    } else if (type === "path") {
        api.urls[url]["path"]  = e.target.value;
    } else if (type == "isadmin") {
        api.urls[url]["admin"] = $(e.target).is(":checked");
    } else if (type == "incsubs") {
        api.urls[url]["include_subs"] = $(e.target).is(":checked");
    } else if (type == "append_only") {
        api.urls[url]["append_only"] = $(e.target).is(":checked");
    }
};

HN.APIKeys.populateControls = function () {
   var i, j, api, admin, subdirs, appendonly ;
     for (i = 0; i < HN.APIKeys.model.length; i = i + 1) {
         api = HN.APIKeys.model[i];
         for (j = 0; j < api.urls.length; j = j + 1) {
             if (api.urls[j].admin) {
                 $(".hn_api_key_" + i + 
                   "  .hn_url_" + j + 
                   "  .hn_apikey_isadmin").attr("checked", "checked");
             }
             if (api.urls[j].include_subs) {
                 $(".hn_api_key_" + i + 
                   "  .hn_url_" + j + 
                   "  .hn_apikey_incsubs").attr("checked", "checked");
             }
             if (api.urls[j].append_only) {
                 $(".hn_api_key_" + i + 
                   "  .hn_url_" + j  + 
                   "  .hn_apikey_append").attr("checked", "checked");
             }
         }
     }
};

HN.APIKeys.renderUrls = function (urls) {
        var i, html;
        html = "";
        for (i = 0; i < urls.length; i = i + 1) {
            html += "<div class='hn_url_" + i + "'>" +
            "<div class='hn_api_pkey'>Path</div>" +
                "<input class='hn_api_path hn_api_control' " + 
                "data-api-url-no='" + i + "' " +
                "data-api-type='path'" +
                "type='text' value='" + 
                urls[i]["path"] + "' disabled />" +
                "<div class='clear'>" +
                "<span>Is Admin?</span>" + 
                "<input class='hn_api_control hn_apikey_isadmin' " + 
                "data-api-url-no='" + i + "' " +
                "data-api-type='isadmin'" +
                "type=checkbox disabled />" +
                "<span>Include Sub-Dirs?</span>" +
                "<input class='hn_api_control hn_apikey_incsubs' " + 
                "data-api-url-no='" + i + "' " +
                "data-api-type='incsubs'" +
                "type=checkbox disabled />" +
                "<span>Append Only</span>" +
                "<input class='hn_api_control hn_apikey_append' " + 
                "data-api-url-no='" + i + "' " +
                "data-api-type='append_only'" + 
                "type=checkbox disabled />" +
                "</div>" +
                "</div>";
        };
        return html;   
};

HN.APIKeys.render = function () {
    var i, json, html, api, urlRenderFn, isEnabled;
    html = "";
    for (i = 0; i < HN.APIKeys.model.length; i = i + 1) {
        api = HN.APIKeys.model[i];
        html += "<div class='hn_api_key hn_api_key_" + i +
            "' data-api-no='" + i + "' >" +
            "<div class='hn_api_pkey'>Public Key:</div>" +
            "<div class='hn_code floatleft'>" + i + api["publickey"] + 
            "</div>" +
            "<div class='clear'></div>" +
            "<div class='hn_api_pkey'>Private Key:</div>" +
            "<div class='hn_code'>" + api["privatekey"] + 
            "</div>" +
            "<div><input class='hn_api_notes' type='textarea' value='"  +
            api["notes"] + "' " +
            "data-api-url-no='notes'" +
            "data-api-type='notes'" +
            "' disabled /></div>" +
            "<div class=hn_api_url_container>" +
            HN.APIKeys.renderUrls(api["urls"]) +
            "</div>" +
            "</div>";
    }
    $(".hn_api_management").html(html);
    HN.APIKeys.populateControls();
    isEnabled = $(".hn_api_management").attr("data-api-enabled");
    if (isEnabled) {
        $(".hn_api_notes").removeAttr("disabled");
        $(".hn_api_control").removeAttr("disabled");
        $(".hn_api_key").change(HN.APIKeys.changed);
    }
};

HN.APIKeys.reload_apikeys = function () {
    var JSON = $(".hn_api_management").attr("data-json");
    HN.APIKeys.model = eval(JSON);
    HN.APIKeys.render();
};
