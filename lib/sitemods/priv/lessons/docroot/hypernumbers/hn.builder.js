HN.Widgets.text.add = true;

HN.Widgets.text.create =
    function() {
        return "<div class='hn' data-type='text'></div>";
    };

HN.Widgets.form.add = true;

HN.Widgets.form.create =
    function() {
        return "<form class='hn' data-type='form'></form>";
    };

HN.Widgets.form.bindFrom =
    function(self, path, range) {

        var $form = $(self).empty();
        for( var x = range.x1; x <= range.x2; x++ ) {
            
            var to = path +
                HN.Util.refToStr({"type":"col_range", "obj":{"x1":x, "x2":x}});
            var from = path +
                HN.Util.refToStr({"type":"cell", "obj":{"x":x, "y":range.y1}});

            var html = "<label><span><div data-binding-from='"+from+"' "
                +"class='hn' data-type='text'></div></span></label>"
                +"<div class='hn' data-type='input' "
                +"data-binding-to='"+to+"'></div>";

            $form.append(html);
        }
        $form.append("<input type='submit' value='submit' />");
        return true;
    };


HN.Widgets.input.add = true;

HN.Widgets.input.create =
    function() {
        return "<div class='hn' data-type='input'></div>";
    };


HN.Builder = function(data, layout)
{
    var public  = {};
    public.model = null;

    public.details = {};
    
    var globid      = 1;
    var configuring = null;
    var toOrFrom    = null;
    var state       = "EDITING";
    var render      = new HN.HTMLRender(data);

    init();
    loadData();
    bindEvents();
    
    public.loadTplFile = function(path) { 
        public.details = parsePath(path);
        $.get(path, public.loadHtml);
    };
    
    public.loadHtml = function(str) { 
        public.model = public.makeModelObj(str);
        public.render();
    };

    public.makeModelObj = function(string) {
        var base = $(string)[0];
        return addIds(base);
    };

    public.render = function() {
        $("#viewbuilderintro").hide();
        var toRender = $(public.model).clone();
        $("#preview").empty().append(toRender);
        render.render($("#preview"));
        builder.configure();
    };

    public.configure = function() {
        $.each($("#preview").find(".hn"), function() {
            if( $(this).html() == "" ) {
                $(this).html("click me to configure");
            }
        });

        if( configuring && state == "EDITING" ) {
            configureElement(configuring);
        }
    };

    public.viewCode = function() {
        $("#preview")[0].innerHTML =
            "<textarea id='previewcode'>"+renderHtml(public.model)
            +"</textarea>";
    };

    function init() {
        for ( var i in HN.Widgets ) {
            if( typeof HN.Widgets[i].add != "undefined" ) {
                $("#add").append( "<input type='button' value='"+i+"' "
                                  +"class='btn' data-obj='"+i+"' />" );
            }
        }        
    };

    function loadData() {

        var groups = data.getPageData(path).data.groups;
        
        $.getJSON("/?views", function(data) {
            $.each(data, function() {
                var name = this.replace(".tpl", "");
                var $item = $("<li><a name='"+name+"'>"+name+"</a></li>");
                
                $item.click( function(e) {
                    var url = "/views/"+$(this).find("a").attr("name")+".tpl";
                    public.loadTplFile(url);
                    $("#mainviewbuilder").show();
                    layout.state = layout.states.MOST_VIEWBUILDER;
                    layout.gridResize();
                });

                $("#existingviews").append($item);
                var opts = {
                    callback : function(el) {
                        return layout.selection.menuItemPicked(el);
                    }
                };                
                $("#hnmenu").filemenu(opts);
            });
        });

        $.getJSON("/?templates", function(data) {
            $.each(data, function() {
                var li = "<option name='"+this+"'>"+this+"</option>";
                $("#viewtemplates").append(li);
            });
        });

        $.each(groups, function() {
            var li = "<option name='"+this+"'>"+this+"</option>";
            $("#groups").append(li);
        });
    };

    function bindEvents() {

        $("#use-range").click( function() {
            if( configuring ) {
                
                $("#sheetpanel").addClass("disabled");
                var obj = getModelObj(public.model, configuring),
                type    = obj.getAttribute("data-type"),
                bounds  = layout.selection.bounds;
                
                if( typeof HN.Widgets[type][toOrFrom] != "undefined" ) {
                    
                    var path =  $("#togglepathstyle").is(":checked") 
                        ? HN.Util.make_relative(document.location.pathname, 
                                                layout.s.data.data.path)
                        : layout.s.data.data.path;
                    

                    HN.Widgets[type][toOrFrom](obj, path, bounds);
                } else {
                    var attr = ( toOrFrom == "bindTo" )
                        ? "data-binding-to" : "data-binding-from";
                    var ref = HN.Util.range_to_str2(bounds, layout.s);

                    var path =  $("#togglepathstyle").is(":checked") 
                        ? HN.Util.make_relative(document.location.pathname, 
                                                layout.s.data.data.path)
                        : layout.s.data.data.path;

                    obj.setAttribute(attr, path + ref);
                }

                addIds(public.model);
                public.render();
            }
        });
        
        $("#bindto, #bindfrom").focus( function() {
            toOrFrom = ($(this).attr("id") == "bindto")
                ? "bindTo" : "bindFrom" ;
            $("#use-range").show();
        });

        $("#toggleview").bind("change", function() {
            if( this.checked ) {
                public.viewCode();
            } else {
                public.model = public.makeModelObj($("#previewcode")[0].value);
                public.render();
            }
        });

        $("#delete").click( function() {
            if( configuring ) {
                var obj = getModelObj(public.model, configuring);
                obj.parentNode.removeChild(obj);
                configuring = null;
                builder.render();
            }
        });

        $("#closeviewbtn").click( function() {
            layout.state = layout.states.FULL_SPREADSHEET;
            layout.gridResize();
        });

        $("#preview").bind("mousedown", function(e) {
            
            if( state != "EDITING" ) {
                return;
            }
            
            var obj = find_parent_hnobj(e.target);
            
            if( obj ) {
                
                var wrap = $(obj).parents(".container").length > 0
                    ? $(obj).parents(".container")
                    : false;
                
                var id = obj.getAttribute("data-hnid");
                
                $(".configuring").removeClass("configuring");
                
                configuring =
                    $("#preview").find("[data-hnid="+id+"]"
                                      ).addClass("configuring")[0];
                
                
                startDrag(obj, "remove", function(drop) {
                    
                    var id = obj.getAttribute("data-hnid");
                    var targ = getModelObj(public.model, id);

                    if( drop.action == "click" ) {
                        configureElement(targ);
                    } else if( drop.action == "move" ) {
                        addElement(targ, drop.target);
                        configureElement(targ);
                    }
                }, wrap);
                e.preventDefault();
            }
        });
        
        $("#add").bind("mousedown", function(e) {
            
            if( e.target.nodeName == "INPUT") {

                var obj = e.target.getAttribute("data-obj");
                var el = public.makeModelObj(HN.Widgets[obj].create());

                var placer = $("<div id='placer' drop-type='"
                               +e.target.getAttribute("data-obj")
                               +"'> </div>")[0];
                
                startDrag(placer, "clone", function(drop) {
                    var tgt = drop.action == "click" ? null : drop.target;
                    var id = globid++;
                    el.setAttribute("data-hnid", ""+id);
                    addElement(el, tgt);
                    configureElement(el);
                });
            }
        });

        $("#changetype").bind("change", function() {
            var obj = getModelObj(public.model, configuring);
            $(obj).attr("data-type", $(this).val());
            public.render();
        });

        $("#previewbtn").click( function() {
            if( state == "EDITING" ) {
                state = "PREVIEW";
                $(".configuring").removeClass("configuring");
                $("#preview").removeClass("editing");
                $("#panelwrapper").hide();
                $(this).val("edit");
            } else {
                state = "EDITING";
                $("#preview").addClass("editing");
                $("#panelwrapper").show();
                $(this).val("preview");
            }
        });

        $("#createviewbtn").click( function() {
            var username = data.getPageData(path).data.user;
            var name     = $("#newviewname").val() + ".tpl";
            var tpl      = $("#viewtemplates").val();
            var group    = $("#groups").val();
       
            if( tpl == "blank" ) { 
                public.loadHtml("<div></div>");
            } else {
                public.loadTplFile("/templates/"+tpl+"/index.tpl");
            }

            public.details = { 
                "type"  : (group == "me") ? "user" : "group",
                "group" : (group == "me") ? username : group,
                "name"  : name
            };

            $("#mainviewbuilder").show();
        });
        
        var $saveview = $("#saveviewbtn");
        $saveview.click( function() {
            
            $saveview.val("saving...");
            
            var params = {"saveview": {
                "name"   : snipTpl(pathFromDetails()),
                "tpl"    : renderHtml(public.model)
            }};
            
            var url  = pathFromDetails();
            var link = "<a href='?view="+url+"' target='blank'>view</a>";
            
            $.post(document.location.pathname,
                   JSON.stringify(params), function(data) {
                       $saveview.val("saved!");
                       setTimeout( function() { $saveview.val("save"); }, 1000);
                   }, "json");
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

        if( getIdFromObj(model) == id ) {
            return model;
        }

        return $(model).find("[data-hnid="+id+"]")[0];

    };

    function getIdFromObj(obj) {
        return ( typeof obj == "object" )
            ? obj.getAttribute("data-hnid")
            : obj;
    };

    function addElement(el, target) {
        if( !target || target.getAttribute("id") == "preview" ) {
            public.model.appendChild(el);
        } else {
            var targ = getModelObj(public.model, target),
            type     = $(targ).attr("data-type");
            
            if( $(targ).hasClass("hn") && 
                type != "blog" && type != "form" ) {
                targ.parentNode.insertBefore(el, targ);
            } else {
                targ.appendChild(el);
            }
        }
        public.render();
    };

    function renderHtml(obj) {

        if( obj.nodeName == "#text" ) {
            return obj.textContent;
        }

        var tag = obj.nodeName.toLowerCase();

        for( var children="",  i=0; i<obj.childNodes.length; i++ ) {
            children += renderHtml(obj.childNodes[i]);
        }

        for( var attr = "", x = 0; x<obj.attributes.length; x++ ) {
            var tmp = obj.attributes[x];
            if( tmp.nodeName != "data-hnid" ) {
                attr += " "+tmp.nodeName+"='"+tmp.nodeValue+"'";
            }
        }
        return "<"+tag+attr+">" + children + "</"+tag+">";
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

        document.body.style.cursor = "pointer";

        var droptarget = null,
        $placer    = $(orig);//
        $parent    = (!$parent) ? $("#preview") : $parent;

        var is_inside = function(obj, parent)
        {
            return ( obj == parent ) ||
                ( obj.parentNode != null && 
                  is_inside(obj.parentNode, parent) );
        };

        var check_target = function(hover) {

            if( droptarget && hover == droptarget || 
                hover == orig || 
                hover.nodeName != "DIV" || 
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

    function configureElement(obj) {
        
        if( $(obj).attr("data-type") == "form" ) {
            $("#changetype, #bindto").hide();
        } else {
            $("#changetype, #bindto").show();
        }
        
        $(".configuring").removeClass("configuring");
        
        configuring =
            $(getModelObj($("#preview")[0], obj)).addClass("configuring")[0];

        $("#bindfrom").val(configuring.getAttribute("data-binding-from"));
        $("#bindto").val(configuring.getAttribute("data-binding-to"));
        $("#changetype").val(configuring.getAttribute("data-type"));

        // selectTab("configure");
    };

    function parsePath(name) {

        var name = name.split("/");
        var type = (name[2] == "_u") ? "user" : "group";

        return { 
            "type"  : type,
            "group" : name[3],
            "name"  : name[4]
        };
    };

    function pathFromDetails() {
        var pre = (public.details.type == "user") ? "_u" : "_g";        
        return pre + "/" + public.details.group 
            + "/" + public.details.name;
    };

    function snipTpl(file) {
        var ret = file.substring(0, file.length - 4);
        return ret;
    };

    return public;
}

