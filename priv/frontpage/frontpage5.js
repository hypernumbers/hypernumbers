var Frontpage = function() {

    var api = {},
    currentStory = 1;
    
    api.showStory = function(story) {
        $("#fp5_story_"+currentStory).hide();
        $("#fp5_story_"+story).show();
        $("#fp5_menu_"+currentStory).css('background', 'url(http://files.hypernumbers.com/redesign/menu.png) no-repeat');
        $("#fp5_menu_"+story).css('background', 'url(http://files.hypernumbers.com/redesign/activemenu.png) no-repeat');
        currentStory = story;
    };
    
    api.init = function() {
        var click = function(e) {
            var menu = e.target.id.split("_")[2];
            api.showStory(menu);
            };
        
        $("#fp5_menu").bind("click", click);            
    };

    api.init();
    api.showStory(1);

    return api;
};

var HN_Frontpage = new Frontpage();

