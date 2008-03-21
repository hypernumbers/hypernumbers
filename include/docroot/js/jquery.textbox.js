/**
 * jQuery TextBox
 * Version 0.1 - 18/03/2008
 * @author Dale Harvey <harveyd@gmail.com>
 *
 * A combination of a text input and a drop down
 * select box, used by
 * http://code.google.com/p/jqueryspreadsheet/
 *
 **/

(function($) {

$.fn.textbox = function(options) 
{
    // Add items to the list
    var addItem = function(list,item)
    {
        var li = $("<li />").append($("<a>"+item+"</a>"));
        list.append(li);   
    };
        
    $.fn.textbox.defaults = 
    {
        items:      [],     // Default list
        onSelect:   null,   // Callback for item selected
        onChange:   null    // Callback for text changed
    };
    
    // default options used on initialisation
    // and arguments used on later calls
    var opts = $.extend({}, $.fn.textbox.defaults, options);
    var args = arguments;
    
    /**
     * Entry point
     */   
    return this.each(function() 
    {
        // Initialisation
        if(typeof $.data(this,"textbox") == "undefined")
        {
            var $t   = $(this);
            
            var top  = $t.offset().top;
            var left = $t.offset().left;
            var height = this.offsetHeight + parseInt($t.css("border-top-width"));
            var width  = this.offsetWidth  + parseInt($t.css("border-left-width"));
            
            // Wrap all the dom elements
            var root = $t.wrap("<div class='textbox' />").parent();
            root.width(width).height(height);
                
            // The drop down list
            var list = $("<ul />").appendTo(root).width(width
            ).css("top",(top + height) + "px"
            ).mousedown(function(e)
            {
                list.toggle();
                $t.val($(e.target).text());
                    
                if(typeof opts.onSelect == "function")
                     opts.onSelect($(e.target).text());
            });            
            
            // The arrow that shows the list onclick
            var arrow = $("<div class='arrow' />").appendTo(root
            ).css("left", (width - 16) + "px"
            ).mousedown(function()
            {
                list.toggle();
            });
                
            $t.click(function()
            {
                // Make sure the text is selected so
                // users can type to overwrite current text
                this.select();
            }
            ).focus(function()
            {
                var val = $(this).val();
                this.select();
                    
                // Run callback if the user has typed
                // something new
                $(this).bind('blur',function()
                {
                    $(this).unbind("blur").unbind("keyup");
                        
                    if(typeof opts.onChange == "function"
                        && $(this).val() != val
                        && $(this).val() != "")
                    {
                        opts.onChange($(this).val());
                    }
                }
                ).bind('keyup',function(e)
                {
                    // When the user presses return, lose focus
                    if(e.keyCode == 13)
                    {
                        $(this).unbind("keyup");
                        $(this).blur();
                    }
                });
            });
            
            // Store ths list so it can
            // be added to in later calls
            $.data(this,"textbox",{list:list});
            
            // Setup the initial list
            $.each(opts.items,function(i)
            {
                addItem(list,this);
            });
        }
        
        // The plugin has already been created on this object
        // must be an external call to modify
        else if(args[0] == "add")
            addItem($.data(this,"textbox").list,args[1]);
    });
};

})(jQuery);