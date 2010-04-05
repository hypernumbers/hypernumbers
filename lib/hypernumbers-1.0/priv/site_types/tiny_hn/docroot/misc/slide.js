var SlideShow = function($wrapper) { 

    var public = {};

    var numChildren  = $wrapper.children().length;            
    var currentIndex = null;
    var currentDom   = null;
    var interval     = null; 

    init();

    public.start = function(length) {
        interval = window.setInterval(next, length);
    };

    public.stop = function() {
        window.clearInterval(interval);
    };

    public.createPicker = function($dom) { 
        for( var i = 1; i <= numChildren; i++ ) {
            var link = $("<a>"+i+"</a>").click( function() { 
                var tmp = parseInt($(this).text(), 10);
                public.stop();
                if( tmp != currentIndex ) {
                    show(tmp);
                }
            });
            $dom.append(link);
        }
    };

    function next() { 
        var tmp = (currentIndex == numChildren ) 
            ? 1 : currentIndex + 1;
        show(tmp);
    };            

    function show(i) { 
        
        var toHide = currentDom;
        var toShow = $wrapper.find("div:nth-child("+i+")");

        var f = function() { 
            currentIndex = i;             
            currentDom = toShow;
            currentDom.fadeIn(); 
        };

        if( toHide ) { 
            toHide.fadeOut('fast', f);
        } else { 
            f();
        }
    };

    function init() { 
        show(1);
    }

    return public;
};

var slide = new SlideShow($("#slideshow"));
slide.createPicker($("#slideshowPicker"));
slide.start(8000);

