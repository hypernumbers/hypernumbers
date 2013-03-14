Vixo = {};

Vixo.init = function () {
    var getAuthCookieFn, getAuthCookieQueryFn, successFn, name, ca, i, c;

    getAuthCookieFn = function () {
        name = "auth=";
        ca = document.cookie.split(';');
        for (i = 0; i < ca.length; i = i + 1) {
            c = ca[i];
            while (c.charAt(0) === ' ') {
                c = c.substring(1, c.length);
            }
            if (c.indexOf(name) === 0) {
                return true;
            }
        }
        return false;
    };

    getAuthCookieQueryFn = function () {
        var i, name, value, querystring = location.search.replace( '?', '' ).split( '&' );
        for ( i=0; i<querystring.length; i++ ) {
            // get name and value
            name = querystring[i].split('=')[0];
            value = querystring[i].split('=')[1];
            if (name === "auth") {
                document.cookie = name + "=" + value;
                return true;
            }
        }
        return false;
    }

    // Basically we set an auth cookie for an
    if (!getAuthCookieQueryFn() && !getAuthCookieFn()) {
        window.location = "http://hypernumbers.com/_sync/singlecookie/?return=" + window.location;
    };
};

Vixo.init();