HN.Widgets.text.create =
    function() {
        return "<div class='hn' data-type='text'></div>";
    };

HN.Widgets.text.help =
    "<div class='help'>To enter cells on a page, pick a selection from "
    +"the spreadsheet and press the 'get data from', you can pick one cell"
    +" or a whole range and it will all be displayed.</div>";


HN.Widgets.form.create =
    function() {
        return "<form class='hn droppable' data-type='form'></form>";
    };

HN.Widgets.form.bindFrom =
    function(self, path, range) {

        var $form = $(self).empty();
        for( var x = range.x1; x <= range.x2; x++ ) {
            
            var to = path +
                HN.Util.refToStr({"type":"col_range", "obj":{"x1":x, "x2":x}});
            var from = path +
                HN.Util.refToStr({"type":"cell", "obj":{"x":x, "y":range.y1}});
            
            var html = "<div data-binding-from='"+from+"' "
                +"class='hn' data-type='text'></div>"
                +"<div class='hn' data-type='input' "
                +"data-binding-to='"+to+"'></div>";

            $form.append(html);
        }
        return true;
    };

HN.Widgets.form.help =
    "<div class='help'>To create a form, pick a row of headers and click"
    +" the 'get data from' button, when users fill out your form the data "
    +"will be appended to these rows.</div>";

HN.Widgets.input.create =
    function() {
        return "<div class='hn' data-type='input'></div>";
    };

HN.Widgets.radio.create =
    function() {
        return "<div class='hn' data-type='radio'></div>";
    };

HN.Widgets.select.create =
    function() {
        return "<div class='hn' data-type='select'></div>";
    };


HN.Builder = function(data, layout)
{
    var public     = {};
    public.model   = null;
    public.details = {};
    
    var dirty       = false;
    var globid      = 1;
    var state       = "INIT";
    var configuring = null;
    var render      = new HN.HTMLRender(data);
    var cachedCSS   = [];

    loadData();
    bindEvents();

    selectTab("add");
    
    public.loadTplFile = function(url, path) { 
        public.setDetails(url, parsePath(path));
        $.get("", {"rawview": path}, public.loadHtml);
    };

    public.isRendering = function() { 
        return state == "RENDER";
    };
    
    public.loadHtml = function(str) { 
        state = "RENDER";
        public.model = public.makeModelObj(str);
        markDirty();
    };

    public.makeModelObj = function(string) {
        var base = $(string)[0];
        return addIds(base);
    };
    
    public.render = function() {
        var $toRender = $(public.model).clone();
        $("#preview").empty().append($toRender);
        render.render($("#preview"));
//        inlineCSS($toRender);
        builder.configure();
    };
    
    public.configure = function() {
        $.each($("#preview").find(".hn"), function() {
            var $tmp = $(this);
            
            if( $tmp.html() == "" || ($tmp.attr("data-type") == "form" 
                                      && $tmp.children().length < 2 )) {
                
                var type = $tmp.attr("data-type");
                if( typeof HN.Widgets[type].help != "undefined" ) {
                    $tmp.html(HN.Widgets[type].help);
                } else { 
                    $tmp.html("click me to configure");
                }
            }
        });
        
        if( configuring ) {
            configureElement(configuring);
        }
    };

    // We strip the outer <div id="hnbody"> and readded it again
    // when we parse the code again, it is required and cant
    // have people deleting it
    public.viewCode = function() {
        stopConfiguring();
        
        var $html = $(public.model,"#hnbody").children().clone();
        $html.find("*").removeAttr("data-hnid").removeAttr("style");
        var html = render.renderHtml($html);
        var text = $("<textarea id='previewcode'>"+html+"</textarea>")
            .height($("#preview").height()-10);
        $("#preview").empty().append(text);
    };
    
    public.setDetails = function(path, details) {
        public.details      = details;
        public.details.path = path;
        var url = public.details.path + "?view=" 
            + pathFromDetails().replace(".tpl", "");
        $("#viewname").text("View My Site").attr("href", url);
    };

    public.viewCreate = function() { 
        $("#viewbuilder,#viewbuilderintro").show();
        $("#mainviewbuilder").hide();
        state = "CREATE";
    };

    public.saveCurrentView = function(overwrite, callback, async) {
        
        if( dirty ) { 
            dirty = false;
        } else {
            return;
        }
           
        var $html = $(public.model,"#hnbody").clone();
        $html.find("*").removeAttr("data-hnid").removeAttr("style");
        render.render($html);
        var html = render.renderHtml($html);

        var params = {"saveview": {
            "name"      : pathFromDetails(),
            "tpl"       : html,
            "overwrite" : overwrite
        }};

        $.ajax({
            "type"     : 'POST',
            "async"    : async,
            "url"      : public.details.path,
            "data"     : JSON.stringify(params),
            "success"  : callback, 
            "dataType" : "json"
        });
    };

    public.isLoaded = function(path, view) {

        var details  = parsePath(view);
        details.path = path;

        return state == "RENDER" && 
            HN.Util.equals(public.details, details);
    };

    public.state = function() {
        return state;
    };

    public.close = function() {
        selectTab("add");
        state          = "INIT";
        public.details = {};
        configuring    = null;
    }

    function loadData() {

        $.getJSON("?templates", function(data) {
            for(var i in data) { 
                if( data[i] != "tiny" && data[i] != "ventris" ) {
                    var li = "<option name='"+data[i]+"'>"+data[i]+"</option>";
                    $("#viewtemplates").append(li);
                }
            }
        });

        $.each(data.key(path, "groups"), function() {
            var li = "<option name='"+this+"'>"+this+"</option>";
            $("#groups").append(li);
        });
    };

    function bindEvents() {

        $("toggleview").val(false);

        $("#required").bind("change", function() {
            if( this.checked ) {
                getModelObj(public.model, configuring)
                    .setAttribute("data-required", "true");
            } else {
                getModelObj(public.model, configuring)
                    .removeAttribute("data-required");
            }
            markDirty();
        });        

        // auto save, will probably need refined
        $(window).bind("unload blur", function(e) { 
            if( state == "RENDER" ) {
                public.saveCurrentView(true, null, true);
            }
        });

        $("#viewname").click( function(e) { 
            public.saveCurrentView(true, function() { return true; }, false);
        });

        $("#bindto, #bindfrom").click( function() {

            $("#sheetpanel").addClass("disabled");
            var toOrFrom = ($(this).attr("id") == "bindto")
                ? "bindTo" : "bindFrom" ;
            var obj    = getModelObj(public.model, configuring);
            var type   = obj.getAttribute("data-type");
            var bounds = layout.tabUI().currentSelectedBounds();
            
            if( typeof HN.Widgets[type][toOrFrom] != "undefined" ) {
                
                var path =  $("#togglepathstyle").is(":checked") 
                    ? HN.Util.make_relative(hn.currentPath(), 
                                            layout.currentSheet().path())
                    : layout.currentSheet().path();
                

                HN.Widgets[type][toOrFrom](obj, path, bounds);
            } else {
                var attr = ( toOrFrom == "bindTo" )
                    ? "data-binding-to" : "data-binding-from";
                var ref = HN.Util.range_to_str2(bounds, layout.s);

                var path =  $("#togglepathstyle").is(":checked") 
                    ? HN.Util.make_relative(hn.currentPath(), 
                                            layout.currentSheet().path())
                    : layout.currentSheet().path();

                obj.setAttribute(attr, path + ref);
            }

            addIds(public.model);
            markDirty();
        });

        $("#viewconfbinding .clear").click( function() {
            var toOrFrom = $(this).hasClass("to") 
                ? "data-binding-to" : "data-binding-from";
            var obj      = getModelObj(public.model, configuring);
            obj.removeAttribute(toOrFrom);
            markDirty();
        });

        $("#toggleview").bind("change", function() {
            if( this.checked ) {
                public.viewCode();
            } else {
                public.loadHtml("<div id='hnbody'>\n"
                                +$("#previewcode")[0].value
                                +"\n</div>");
            }
        });

        $("#delete").click( function() {
            if( configuring ) {
                var obj = getModelObj(public.model, configuring);
                obj.parentNode.removeChild(obj);
                selectTab("add");
                configuring = null;
                markDirty();
            }
        });

        $("#closeviewbtn").click( function() {
            window.location.hash = hn.hashUrl.deleteParam("view");
        });

        $("#preview").bind("click", function(e) {            
            if( e.target.nodeName == "A" ) {
                e.preventDefault();
            }
        });


        $("#preview").bind("mousedown", function(e) {

            var domid = e.target.getAttribute("id");
            
            if( (e.target.nodeName == "TEXTAREA" && domid == "previewcode") || 
                domid == "delete" ) {
                return;
            }

            e.preventDefault();
            var obj = find_parent_hnobj(e.target);
   
            if( obj ) {

                var id = obj.getAttribute("data-hnid");
                configureElement(id);
                
                var wrap = $(obj).parents(".container").length > 0
                    ? $(obj).parents(".container")
                    : false;
                
                startDrag(obj, "remove", function(drop) {

                    var targ = getModelObj(public.model, id);
                    
                    if( drop.action == "move" ) {
                        addElement(targ, drop.target);
                    }
                }, wrap);

            } else { 
                if( configuring ) {
                    stopConfiguring();
                }
            }
        });
        
        $("#add").bind("mousedown", function(e) {
            
            if( e.target.nodeName == "INPUT") {

                var obj = e.target.getAttribute("data-obj");
                var el  = public.makeModelObj(HN.Widgets[obj].create());

                var placer = $("<div id='placer' drop-type='"
                               +e.target.getAttribute("data-obj")
                               +"'> </div>")[0];
                
                startDrag(placer, "clone", function(drop) {                    

                    if( !drop.target || drop.action == "click" ) { 
                        return;
                    }

                    var id = globid++;
                    el.setAttribute("data-hnid", ""+id);
                    addElement(el, drop.target);
                    configureElement(id);
                });
            }
        });

        $("#changetype").bind("change", function() {

            $(getModelObj(public.model, configuring))
                .attr("data-type", $(this).val());
            markDirty();
        });

        $("#viewtabs a").click( function(e) {
            e.preventDefault();
            var name = $(this).attr("href").replace("#",""); 
            if( name == "configuring" && !configuring ) {
                return true;
            }
            selectTab(name);
        });

        $("#overwritecancel").click( function() {
            $("#viewexists").hide();
            $("#newviewname").val("");
            $("#createviewbtn").show();
        });

        $("#createviewbtn, #overwrite").click( function() {

            var personal  = $("#groups").val() == "me";
            var userName  = data.key(path, "user");
            var groupName = $("#groups").val();
            var tpl       = $("#viewtemplates").val();
            var overwrite = $(this).attr("id") == "overwrite";
            
            var viewName = (personal ? "_u" : "_g") 
                + "/" + (personal ? userName : groupName)
                + "/" + $("#newviewname").val();
            
            var viewHashUrl = hn.currentPath() + ":" 
                + viewName;

            var errorMsg = "Sorry, A view with that name already exists, "
                +"overwrite?";
            
            var loaded = function(html) {
                
                var params = {"saveview": {
                    "name"      : viewName,
                    "tpl"       : html,
                    "overwrite" : overwrite
                }};
                
                $.post(hn.currentPath(), JSON.stringify(params), 
                       function(data) {
                           if( data == "error" ) {
                               $("#viewexists").show();
                               $("#createviewbtn").hide();
                           } else if( data == "success" ) {
                               $("#viewbuilderintro").hide();
                               window.location.hash = hn.hashUrl
                                   .setParam("view", viewHashUrl);
                           }
                       }, "json");
            };

            if( $("#newviewname").val() == "" ) {
                $("#newviewfeedback").text("A view name cannot be blank");
                return; 
            }
            
            $.get("/templates/"+tpl+"/index.tpl", loaded);
        });
        
        var $saveview = $("#saveviewbtn");
        $saveview.click( function() {
            
            $saveview.val("saving...");
            
            public.saveCurrentView(true, function(data) {
                $saveview.val("saved!");
                setTimeout( function() { $saveview.val("save"); }, 1000);
            }, true);
        });
    };
                         
    function addIds(obj) {
        
        if( typeof obj.getAttribute != "undefined"
            && obj.getAttribute("data-hnid") == null ) {
            obj.setAttribute("data-hnid", ""+globid++);
        }

        for( var i = 0; i < obj.childNodes.length; i++ ) {
            addIds(obj.childNodes[i]);
        }
        return obj;
    };

    function getModelObj(model, id) {
        id = getIdFromObj(id);
        return $(model).find("[data-hnid="+id+"]")[0];
    };

    function stopConfiguring() {
        $("#highlightfrom, #highlightto").hide();
        configuring = null;
        $("#delete").remove();
        $(".configuring").removeClass("configuring");
        selectTab("add");
    };
    
    function getIdFromObj(obj) {
        return ( typeof obj == "object" )
            ? obj.getAttribute("data-hnid")
            : obj;
    };

    function addElement(el, target) {
        
        if( !target ) { 
            return;
        }

        var targ = getModelObj(public.model, target);
        var type = $(targ).attr("data-type");
        
        if( $(targ).hasClass("hn") && 
            type != "blog" && type != "form" ) {
            targ.parentNode.insertBefore(el, targ);
        } else {
            targ.appendChild(el);
        }

        markDirty();
    };

    function find_parent_hnobj(obj) {
        
        if( obj.getAttribute("id") == "preview" ) {
            return false;
        }
        
        if( is_hn_obj(obj) ) {
            return obj;
        }

        return find_parent_hnobj(obj.parentNode);
    };
    
    function is_hn_obj(obj) {
        return (typeof obj.className !== "undefined"
                && obj.className.match("hn"));
    };

    function startDrag(orig, remove, fun, $parent) {

        $("#preview").addClass("dragging");
        document.body.style.cursor = "pointer";

        var droptarget = null;
        var $placer    = $(orig);//
        $parent    = (!$parent) ? $("#preview") : $parent;

        var is_inside = function(obj, parent)
        {
            return ( obj == parent ) ||
                ( obj.parentNode != null && 
                  is_inside(obj.parentNode, parent) );
        };

        var check_target = function(hover) {

            var clas = hover.className || "";

            if( droptarget && hover == droptarget || 
                hover == orig || 
                !clas.match("droppable") ||
                is_inside(hover, $placer[0]) || 
                (hover == document.body || hover == document) || 
                !is_inside(hover, $parent[0])
              ) { return; }

            if( $(hover).attr("data-type") == "form" ) {
                droptarget = $(hover).append($placer.remove())[0];
            } else if( $(hover).hasClass("hn") ) {
                droptarget = $(hover).before($placer.remove())[0];
            } else {
                droptarget = $(hover).append($placer.remove())[0];
            }
        };

        var has_dragged = false;

        var move = function(e) {
            if( !has_dragged ) {
                has_dragged = true;
            } else {
                check_target(e.target);
            }
        };

        var up = function(e) {

            $("#preview").removeClass("dragging");
            document.body.style.cursor = "default";

            HN.Util.removeEvent(document, "mousemove", move);
            HN.Util.removeEvent(document, "mouseup", up);

            if( has_dragged ) {
                fun({"action":"move", "target": droptarget});
            } else {
                fun({"action":"click"});
            }
        };

        HN.Util.addEvent(document, "mousemove", move);
        HN.Util.addEvent(document, "mouseup", up);
    };

    function configureElement(id) {

        if( configuring ) { 
            stopConfiguring();
        }

        $(".configuring").removeClass("configuring");

        configuring = id;
        
        var $viewObj = $(getModelObj($("#preview")[0], id));
        var type     = $viewObj.attr("data-type");

        var $del = 
            $("<input type='button' id='delete' title='delete' value='' />")
            .bind("mousedown", function() { 
                stopConfiguring();
                var obj = getModelObj(public.model, id);
                obj.parentNode.removeChild(obj);                
                markDirty();
            });
        
        $viewObj.addClass("configuring");
        $viewObj.append($del);

        showBindings($viewObj, "from");
        showBindings($viewObj, "to");

        // if( type == "form" ) {
        //     $("#changetype, #bindto").hide();
        // } else {
        //     $("#changetype, #bindto").show();
        // }

        if( type == "input" || type == "textarea" ) {
            $("#required").show();
        } else {
            $("#required").hide();
        }

        $("#bindfrom").val($viewObj.attr("data-binding-from") || "");
        $("#bindto").val($viewObj.attr("data-binding-to") || "");
        $("#changetype").val($viewObj.attr("data-type") || "");
        
        if( $viewObj.attr("data-required") ) {
            $("#required").attr("checked", "checked"); 
        } else {
            $("#required").removeAttr("checked"); 
        }

        selectTab("configuring");
    };

    function showBindings(obj, binding) { 

        var bounds = obj.attr("data-binding-"+binding);

        if( !bounds ) {
            return;
        }

        var ref  = HN.Util.parseRef(bounds);
        if( ref.path != hn.currentPath() ) {
            return; 
        }

        var range = ( ref.type == "cell" ) 
            ? cellToRange(ref.obj) 
            : ref.obj;

        var tmp = layout.tabUI().order_bounds(range);
        layout.tabUI().highlightRange(HN.Util.id("highlight"+binding), tmp);
    };
    
    function cellToRange(cell) {
        return { "x1": cell.x, "y1": cell.y, 
                 "x2": cell.x, "y2": cell.y };
    };

    function selectTab(name) {

        if( name != "configuring" && configuring ) { 
            stopConfiguring();
        }

        $("#tabwins div").hide();
        $("#tabwins #"+name).show();
        $("#viewtabs a").removeClass("selected");
        $("#viewtabs a[href=#"+name+"]").addClass("selected");
    }

    function parsePath(name) {

        name     = name.split("/");
        var type = (name[0] == "_u") ? "user" : "group";

        return { 
            "type"  : type,
            "group" : name[1],
            "name"  : name[2]
        };
    };

    function pathFromDetails() {
        var pre = (public.details.type == "user") ? "_u" : "_g";        
        return pre + "/" + public.details.group 
            + "/" + public.details.name;
    };

    function inlineCSS($dom) {
        
        $dom.find("link[rel=stylesheet]").each( function() { 
            
            var href = $(this).attr("href");
            var css  = cachedCSS[href] || false;
         
            if( !css ) {
                fetchCSS(href);
            } else { 
                applyCSS($dom, css);
            }

            $(this).remove();
        });     
    };

    function applyCSS($dom, css) {
        for( var i=0, len=css.length; i<len; i++ ) {
            var rule = css[i];
            if( rule.selector.match(":") ) {
                // TODO : apply psuedo selectors
            } else { 
                var $el = $dom.find(rule.selector);
                $.each($el, function() {                                       
                    $(this).attr("style", $(this).attr("style") + rule.styles);
                });
            }
        }
    };

    function fetchCSS(href) { 
        $.get(href, function(data) { 
            cachedCSS[href] = parseCSS(data);
            public.render();
        });        
    };
    
    function parseCSS(strCSS) {
        var css   = strCSS.split("}");
        var rules = [];
        for(var i=0, len=css.length; i < len; i++ ) {
            var tmp = css[i].split("{");
            if( tmp.length == 2 ) {
                rules[i] = {
                    "selector" : trim(tmp[0]),
                    "styles"   : trim(tmp[1])
                };
            }
        }
        return rules;        
    };

    function markDirty() { 
        dirty = true;
        public.render();
    };

    function trim(stringToTrim) {
	    return stringToTrim.replace(/^\s+|\s+$/g,"");
    }

    return public;
}
