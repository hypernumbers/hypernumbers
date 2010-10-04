var Frontpage = function() {

    var api = {},
    currentStory = 1;
    
    api.showStory = function(story) {
        $("#fp4_story_"+currentStory).hide();
        $("#fp4_story_"+story).show();
        $("#fp4_menu_"+currentStory).css('background', 'url(http://files.hypernumbers.com/redesign/menu.png) no-repeat');
        $("#fp4_menu_"+story).css('background', 'url(http://files.hypernumbers.com/redesign/activemenu.png) no-repeat');
        currentStory = story;
    };
    
    api.init = function() {
        var click = function(e) {
            var menu = e.target.id.split("_")[2];
            api.showStory(menu);
            };
        
        $("#fp4_menu").bind("click", click);            
    };

    api.init();
    api.showStory(1);

    return api;
};

var HN_Frontpage = new Frontpage();

