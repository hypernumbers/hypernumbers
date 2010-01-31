HN.FileMenu = function($root, opts) {
    
    var active   = false;
    var topLinks = $root.find("> li > a");
    var anchors  = $root.find("a");
    var hovers   = [];

    var hide = function() {
        $(document).unbind('mousedown.menu');
        $root.find("ul").hide();
        $root.find(".hover").removeClass("hover");
        active = false;
    };

    var is_parent = function(parent, child) {
        
        var f = function() {
            if( this == parent ) {
                is = true;
            }
        };
        
        var is = false;
        $.each($(child).parents(), f);
        return is;
    };

    var init = function(menuItem) {
        active = menuItem;
        $(menuItem).addClass("hover");
        hovers.push(menuItem);
        $(menuItem).siblings("ul").show();
        
        var f = function(e) {
            if( !is_parent($root[0], e.target) ) {
                hide();
            }
        };
        $(document).bind('mousedown.menu', f);
    };

    var click = function(e) {      

        if( $.inArray(e.target, topLinks) != -1 && !active ) {
            init(e.target);
        } else if( $.inArray(e.target, topLinks) != -1 && active ) {
            hide();
        } else if( $(e.target).is("a:not(.parent)") ) {  
            if( typeof opts.callback == "function" ) {
                if( opts.callback(e.target) ) {
                    hide();
                }
            }
        } 
    };
    $root.bind("click", click);

    var add_parent_class = function() {
        if( $(this).next().size() > 0 ) {
            $(this).append("<div class='arrow'>&nbsp;</div>");
            $(this).addClass("parent");
            $(this).next().addClass("parent");
        }
    };
    anchors.each(add_parent_class);
    
    var mouseover = function(item) {
        
        if( active ) {
            
            var ancestors = $(item).parents("ul.parent").siblings("a");
            var nhovers   = [];

            for( x in hovers ) {
                if( $.inArray(hovers[x], ancestors) == -1 ) {
                    $(hovers[x]).removeClass("hover");
                } else {
                    nhovers.push(hovers[x]);
                }
            }
            hovers = nhovers;
            // var check_desc = function() {
            //     if( $.inArray(this, anc) == -1 ) {
            //         $(this).removeClass("hover");
            //         return false;
            //     } 
            //     return true;
            // };
            // hovers = hovers.filter(check_desc);
            
            $(item).addClass("hover");
            hovers.push(item);

//            console.log(hovers);
            
            $(item).parent().siblings().find("ul").hide();
            $(item).siblings("ul").show();
            active = item;
        }
    };

    $root.bind("mouseover", function(e) {
        if( e.target.nodeName == "A" ) {
            mouseover(e.target)
        };
    });
};

// (function($) {
    
//     $.fn.filemenu = function(options)
//     {
//         $.fn.filemenu.defaults = {
//             callback : null
//         };
        
//         var opts = $.extend({}, $.fn.filemenu.defaults, options);
//         var args = arguments;
        
//         var fun = function()
//         {
//             var root    = $(this);
//             var top     = root.children("li");
//             var anchors = root.find("a");
//             var active  = false;
            
//             var hide = function() {
//                 $(document).unbind('click');
//                 root.find("ul").hide();
//                 root.find(".hover").removeClass("hover");
//                 active = false;
//             };
            
//             var is_parent = function(parent, child) {
                
//                 var f = function() {
//                     if( this == parent ) {
//                         is = true;
//                     }
//                 };
                
//                 var is = false;
//                 $.each($(child).parents(), f);
//                 return is;
//             };
            
//             var init = function(e)
//             {
//                 if( !active ) {
//                     active = this;
//                     $(this).addClass("hover");
//                     $(this).siblings("ul").show();
                    
//                     var fun = function(e) {
//                         if( !is_parent(root[0],e.target) ) {
//                             hide();
//                         }
//                     };
//                     $(document).bind('mousedown',fun);
//                 }
//                 else {
//                     hide();
//                 }
//             };
            
//             top.children("a").bind('click',init);

//             var add_parent_class = function() {
//                 if( $(this).next().size() > 0 ) {
//                     $(this).append("<div class='arrow'>&nbsp;</div>");
//                     $(this).addClass("parent");
//                     $(this).next().addClass("parent");
//                 } else {
//                     $(this).bind('click', function(e) {
//                         if( typeof opts.callback == "function" ) {
//                             if( opts.callback(e.target) ) {
//                                 hide();
//                             }
//                         } else {
//                             hide();
//                         }
//                     });
//                 }
//             };
//             anchors.each(add_parent_class);

//             var mouseover = function($this) {

//                 if( active && !is_parent($($this).parent()[0], active) ) {
//                     var par = $($this).parent();
//                     var anc = $($this).parents("ul.parent")
//                         .siblings("a").add($this);

//                     var check_desc = function() {
//                         if( $.inArray($this,anc) == -1 ) {
//                             $($this).removeClass("hover");
//                         } else {
//                             $($this).addClass("hover");
//                         }
//                     };
//                     anchors.each(check_desc);

//                     par.siblings().find("ul").hide();
//                     $($this).siblings("ul").show();
//                     active = $this;
//                 }
//             };

//             $(this).bind("mouseover", function(e) {
//                 if( $.inArray(e.target, anchors) != -1 ) {
//                     mouseover(e.target)
//                 };
//             });

//             $(this).bind("mousedown", function(e) {
//                 if( $.inArray(e.target, anchors) != -1 ) {
//                     return false;
//                 };
//             });
            
//             // anchors.mouseover(mouseover);            
//             // anchors.mousedown(function(e) { return false; });
//         };
        
//         return this.each(fun);  
//     };

// })(jQuery);
