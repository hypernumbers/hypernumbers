/**
 *
 **/

(function($) {

$.fn.filemenu = function(options) 
{        
    $.fn.filemenu.defaults = 
    {
    };
    
    var opts = $.extend({}, $.fn.filemenu.defaults, options);
    var args = arguments;
    
    /**
     * Entry point
     */   
    return this.each(function() 
    {
        var root = $(this);
        var top  = root.children("li");
        var anchors = root.find("a");
        var active = false;
        
        var hide = function()
        {
            $(document).unbind('click');
            root.find("ul").hide();
            root.find(".hover").removeClass("hover");
            active = false;
        };

        var is_parent = function(parent,child)
        {
            var f = function()
            {
                if(this == parent)
                {
                    is = true;
                    return true;
                }
            }
            
            var is = false;
            $.each($(child).parents(),f);
            return is;
        }
        
        var init = function(e)
        {
            if(!active)
            {
                active = this;
                $(this).addClass("hover");
                $(this).siblings("ul").show();
                
                var fun = function(e)
                {                               
                    if(!is_parent(root[0],e.target))
                        hide();
                };
                $(document).bind('click',fun);
            }
            else
                hide();
        }
        top.children("a").bind('click',init);
        
        var add_parent_class = function()
        {
            if($(this).next().size() > 0)
            {
                $(this).append("<div class='arrow'>&nbsp;</div>");
                $(this).addClass("parent");
                $(this).next().addClass("parent");
            }
        };
        anchors.each(add_parent_class);
                     
        var mouseover = function() 
        {
            if(active && !is_parent($(this).parent()[0],active))
            {
                var par = $(this).parent();
                var anc = $(this).parents("ul.parent").siblings("a").add(this);
                
                var check_desc = function()
                {
                    if($.inArray(this,anc) == -1)
                        $(this).removeClass("hover");
                    else 
                        $(this).addClass("hover");
                };
                anchors.each(check_desc);
                
                par.siblings().find("ul").hide();
                $(this).siblings("ul").show();
                active = this;
            }
        }
        anchors.mouseover(mouseover);

        anchors.mousedown(function() { return false; });
        anchors.bind('onselectstart',function() { return false; });
        //anchors.not(".parent").click(function() { hide(); })
    });
};

})(jQuery);
