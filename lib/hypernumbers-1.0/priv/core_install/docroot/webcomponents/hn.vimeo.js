HN.Vimeo = {};

HN.Vimeo.bindPlayer = function(id) {

    var iframe = $("#" + id)[0],
    player = $f(iframe),
    tag = $(iframe).attr("data-tag");

    // When the player is ready, add listeners for pause, 
    // finish, and playProgress

    player.addEvent('ready', function() {
                        player.addEvent('play', onPlay);
                        player.addEvent('pause', onPause);
                        player.addEvent('finish', onFinish);
                    });
        
    function onPlay(id) {
        var mark = {"vimeo":  tag,
                    "status": "play"};
        HN.Callbacks.setMark(mark);
    };

    function onPause(id) {
        var mark = {"vimeo":  tag,
                    "status": "pause"};
        HN.Callbacks.setMark(mark);
    };

    function onFinish(id) {
        var mark = {"vimeo":  tag,
                    "status": "finish"};
        HN.Callbacks.setMark(mark);
    };
        
};

HN.Vimeo.reload = function () {
    
    var i, players = $(".hn_vim");
    for (i = 0; i < players.length; i++) {
        HN.Vimeo.bindPlayer($(players[i]).attr("id"));
    }    
};

