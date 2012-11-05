HN.postMessage = {};

HN.postMessage = function () {
    // Get the parent page URL as it was passed in, 
    // for browsers that don't support
    // window.postMessage
    var uri = document.location.hash.replace(/^#/,''),
    parent_url = decodeURIComponent(uri),
    api = {};
    
    // The first param is serialized using $.param (if not a string) 
    // and passed to the parent window. If window.postMessage exists, 
    // the param is passed using that, otherwise it is passed in the 
    // location hash (that's why parent_url is required).
    // The second param is the targetOrigin.
    api.setHeight = function () {
        var height = $("#outer").css("height"),
        width = $("#outer").css("width"),
        msg = {'height' : height, 'width' : width};
        
        $.postMessage(msg, parent_url, parent);
    };
    return api;
};

// Only run this if it is inside a frame
if (top.frames.length !== 0) {
    var pm = new HN.postMessage();

    // need to override the css overflow for body
    $("body").css("overflow", "hidden");

    pm.setHeight();
}