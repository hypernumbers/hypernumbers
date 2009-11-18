HN.Widgets.text.add =
    "<div><input type='button' value='text' "
    +"class='btn' data-obj='text' /></div>";

HN.Widgets.text.create = 
    function() { 
        return "<div class='hn' data-type='text'></div>";
    };

HN.Widgets.text.configure = 
    function(self) {        
        $("#bind").val(self.getAttribute("data-binding-from"));
    };

HN.Widgets.text.useRange = 
    function(self, path, range) {
        self.setAttribute("data-binding-from", 
                          path + HN.Util.range_to_str2(range));
        return true;
    };


HN.Widgets.form.add = 
    "<div><input type='button' value='form' "
    +"class='btn' data-obj='form' /></div>";

HN.Widgets.form.create = 
    function() { 
        return "<form class='hn' data-type='form'></form>";
    };

HN.Widgets.form.configure = 
    function(self) {
        $("#bind").val(self.getAttribute("data-binding-from"));
    };

HN.Widgets.form.useRange = 
    function(self, path, range) {

        var $form = $(self).empty();
        for( var x = range.x1; x <= range.x2; x++ ) {

            var to = path + 
                HN.Util.refToStr({"type":"colrange", "obj":{"x1":x}});
            var from = path + 
                HN.Util.refToStr({"type":"cell", "obj":{"x":x, "y":range.y1}});
            var html = "<label><span><div data-binding-from='"+from+"' "
                +"class='hn' data-type='text'></div></span>"
                +"<span class='hn' data-type='input' "
                +"data-binding-to='"+to+"'></span></label>";

            $form.append(html);
        }
        $form.append("<input type='submit' value='submit' />");
        return true;
    };


HN.Widgets.input.add = 
    "<div><input type='button' value='input' "
    +"class='btn' data-obj='input' /></div>";

HN.Widgets.input.create =  
    function() { 
        return "<div class='hn' data-type='input'>"
            +"<input type='text' value='Configure Me' />"
            +"</div>";
    };
    
HN.Widgets.input.configure =
    function(self) {
        $("#bind").val(self.getAttribute("data-binding-to"));
        return true;
    };

HN.Widgets.input.useRange = 
    function(self, path, range) {
        self.setAttribute("data-binding-to", 
                          path + HN.Util.range_to_str2(range));
        return true;
    };


var builder;

HN.Builder = function(data, layout)
{
    var public  = {},
    globid      = 1,
    configuring = null,
    state       = "EDITING",
    render      = new HN.HTMLRender(data);
   
    for ( var i in HN.Widgets ) {
        $("#add").append( HN.Widgets[i].add );
    } 
    
    loadViews();
    setupEvents();
    selectTab("add");

    public.model = null;
    
    public.makeModelObj = function(string) {
        var base = $(string)[0];
        return addIds(base);
    };

    public.viewCode = function() { 
        $("#preview")[0].innerHTML =
            "<textarea id='previewcode'>"+renderHtml(public.model)
            +"</textarea>";
    };

    public.render = function() {
        var toRender = $(public.model).clone();
        $($("#preview")).empty().append(toRender);
        render.render($("#preview"));
        builder.configure();
    };

    public.configure = function() { 
        $.each($("#preview").find(".hn"), function() {
            if( $(this).html() == "" ) {
                $(this).html("click me to configure");
            }
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

    function loadViews() {

        var $globalviews = $("#globalviews"),
        $userviews       = $("#userviews");
    
        $.getJSON("/?views", function(data) {
            $.each(data, function() {
                
                var view = this.split("/"), 
                item     = "<li name='"+this+"'>"+view[1]+"</li>";
            
                if( view[0] == "_global" ) {
                    $globalviews.append(item);
                } else {
                    $userviews.append(item);
                }
            });
        });
        
        $.getJSON("/?templates", function(data) {
            $.each(data, function() {
                var li = "<li name='"+this+"' class='btn'>"+this+"</li>";
                $("#templateslist").append(li);
            });
        });        
    };

    function loadHtml(name, path) {
        $.get(path, function(data) {
            public.model = public.makeModelObj(data);
            public.render();
        });
    };

    function setupEvents() {

        var $saveview = $("#saveview"),
        global        = $("#saveglobal").is(":checked");
        
        $("#viewviews").click( function() { $("#views").toggle("medium") });
        $("#viewname").focus( function() { this.select(); });
        
        $(".togglesubpanel").bind("click", function() {        
            $(this).parent("div").toggleClass("disabled");
        });
        
        $("#tabs a").bind("click", function() {
            selectTab($(this).attr("name"));
        });

        $("#bind").focus( function() {
            $("#sheetpanel").removeClass("disabled");
        });        

        $("#use_range").click( function() {
            if( configuring ) {
                $("#sheetpanel").addClass("disabled");
                var obj = getModelObj(public.model, configuring);
                HN.Widgets[obj.getAttribute("data-type")]
                    .useRange(obj, layout.s.data.path, 
                              layout.selection.bounds);   
                addIds(public.model);
                builder.render();
            }
        });

        $("#delete").click( function() {
            if( configuring ) {
                var obj = getModelObj(public.model, configuring);
                obj.parentNode.removeChild(obj);
                configuring = null;
                builder.render();
                selectTab("add");
            }
        });


        $saveview.click( function() {
            
            $saveview.val("saving..");
            
            var params = {"saveview": {
                "global" : global,
                "name"   : $("#viewname").val(),
                "tpl"    : renderHtml(public.model)
            }};
            
            var user = HN.Util.readCookie("auth").split(":")[1],
            name     = params.saveview.name,
            url      = (global ? "_global" : user) + "/" + name,
            link     = "<a href='?view="+url+"' target='blank'>view</a>";
            
            $.post(document.location.pathname, 
                   JSON.stringify(params), function(data) {
                       $saveview.val("saved!");
                       $("#viewlink").html(link);
                       setTimeout( function() { $saveview.val("save"); }, 1000);
                   }, "json");
        });
        
        $("#views").click( function(e) {
            if( e.target.nodeName == "LI" ) {
                var name = e.target.getAttribute("name"), 
                split    = name.split("/");   

                loadHtml(name, "/views/"+name+".tpl");

                $("#views").toggle("medium");
                
                if( split[0] == "_global" ) {
                    $("#saveglobal").attr("checked", "checked");
                } else {
                    $("#saveglobal").removeAttr("checked");
                }
                $("#viewname").val(split[1]);
            }
        });
        
        $("#templates").click( function(e) {
            if( e.target.nodeName == "LI" ) {
                var name = e.target.getAttribute("name"); 
                loadHtml(name, "/templates/"+name+"/index.tpl");
            }
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
                
        $("#toggleview").bind("change", function() {
            if( this.checked ) {
                public.viewCode();
            } else {
                public.model = public.makeModelObj($("#previewcode")[0].value);
                public.render();
            }
        });
        
        $("#add").bind("mousedown", function(e) {
            
            if( e.target.nodeName == "INPUT") {
                
                var obj = e.target.getAttribute("data-obj");
                var el = public.makeModelObj(HN.Widgets[obj].create());
                
                startDrag(el, "clone", function(drop) {
                    var tgt = drop.action == "click" ? null : drop.target;
                    var id = globid++;
                    el.setAttribute("data-hnid", ""+id);
                    addElement(el, tgt);
                    configureElement(el);
                });
            }
        });
        
        $("#preview").bind("mousedown", function(e) {
            
            if( state != "EDITING" ) {
                return;
            }
            
            var obj = HN.Builder.find_parent_hnobj(e.target);

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
    };    

    function startDrag(orig, remove, fun, $parent) {

        document.body.style.cursor = "pointer";
        
        var droptarget = null,
        $placer    = $(orig);//$("<div id='placer'> </div>");
        $parent    = (!$parent) ? $("#preview") : $parent;
        
        var is_inside = function(obj, parent) 
        {
            return ( obj == parent ) || 
                ( obj.parentNode != null && is_inside(obj.parentNode, parent) );
        };
        
        var check_target = function(hover) {
            
            if( droptarget && hover == droptarget 
                || hover == orig 
                || hover.nodeName != "DIV"
                || is_inside(hover, $placer[0])
                || (hover == document.body || hover == document)
                || !is_inside(hover, $parent[0])
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

    function selectTab(id) {
        $("#tabs a").removeClass("selected");
        $("#tabs a[name="+id+"]").addClass("selected");
        $(".configurepanel .tab").hide();
        $("#"+id).show();
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
//            if( tmp.nodeName != "data-hnid" ) {
                attr += " "+tmp.nodeName+"='"+tmp.nodeValue+"'";
//            }
        }
        return "<"+tag+attr+">" + children + "</"+tag+">";
    };


    function addElement(el, target) {    

        if( !target || target.getAttribute("id") == "preview" ) {
            public.model.appendChild(el);
        } else {
            var targ = getModelObj(public.model, target),
            type     = $(targ).attr("data-type");
            
            if( $(targ).hasClass("hn") && type != "blog" && type != "form" ) {
                targ.parentNode.insertBefore(el, targ);
            } else {
                targ.appendChild(el);
            }
        }
        public.render();
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

    function configureElement(obj) {
        
        $(".configuring").removeClass("configuring");

        configuring = 
            $(getModelObj($("#preview")[0], obj)).addClass("configuring")[0];
        
        HN.Widgets[obj.getAttribute("data-type")].configure(obj);
        selectTab("configure");                       
    };
    
    return public;
};

HN.Builder.is_hn_obj = function(obj) 
{
    return (typeof obj.className !== "undefined" 
            && obj.className.match("hn"));
};

HN.Builder.clone_with_new_tag = function(obj, tag) 
{
    var newobj = $("<"+tag+"/>").append($(obj).contents())[0];
    
    for( var x = 0; x<obj.attributes.length; x++ ) {
        var tmp = obj.attributes[x];
        newobj.setAttribute(tmp.nodeName, tmp.nodeValue);
    }
    return newobj;
};

HN.Builder.find_parent_hnobj = function(obj)
{
    if( obj.getAttribute("id") == "preview" ) {
        return false
    }

    if( HN.Builder.is_hn_obj(obj) ) {
        return obj;
    }
    return HN.Builder.find_parent_hnobj(obj.parentNode);
};

var Navigation = function(finder) {

    var public = {};

    function select(path) {
        if( !finder.active ) {
            finder.select(path);
            finder.activate();
        } else { 
            finder.select(path);
        }
    };

    function deselect() {
        finder.deactivate();
    };

    public.updateBar = function() {

        $("#pagebar *:gt(0)").remove();
        
        var npath = ( !finder.path )
            ? document.location.pathname.split("/") 
            : finder.path.splice(1);
        
        var path = ["home"];

        $.each(npath, function() {
            if( this != "" ) {
                path.push(this);
                var url = path.join("/");
                $("#pagebar").append("<div class='crumb'></div>"
                                     +"<a data-href='"+url+"'>"+this+"</a>");
            }
        });        
    };
    
    $("#pagebar").bind("click", function(e) {
        if( e.target.nodeName == "A" ) {
            select(e.target.getAttribute("data-href").split("/"));
        }
    });

    $("#pdclose").bind("click", deselect);
    $("#pdvisit").bind("click", function() { 
        finder.chooseCurrent();
        finder.deactivate();
    });

    public.updateBar();

    return public;
};


HN.BuilderBoot = function() 
{
    var layout, data, nav, 
    sheets = [],
    path   = document.location.pathname;
    
    var load_sheet = function(url) {
        
        path = url;

        if( typeof sheets[path] !== "undefined" ) {
            if( sheets[path] != true ) {
                layout.s = sheets[path];
                sheets[path].calc_size();
                layout.panes.refresh();
                layout.selection.show_selection();
                builder.render();
                nav.updateBar();
            }
        } else {
            data.addPage(path);
        }
    };
    
    var options = {
        
        dataLoaded: function() {
            
            sheets[path] = new HN.Sheet(data.getPage(path));
            
            if( !layout ) {
                
                layout  = new HN.Layout(sheets[path]);

                builder = new HN.Builder(data, layout);
                builder.model = builder.makeModelObj("<div></div>");

                if( $.inArray("admin", data.getPage(path).groups) ) {
                    $(".global").show();
                }                
                
                // This needs taken out, sizing bug with how the spreadsheet
                // gets layed out (intermittent)
                setTimeout( function() {
                    $("#sheetpanel").addClass("disabled");
                }, 200);
                
            } else { 
               load_sheet(path);
            }
        },
        
        dataReloaded: function(data) {
            console.log("reload");
        },

        opaqueLoaded: function(name, data) {
            var opts = {
                "activate"   : function() { $("#finder").show(); },
                "deactivate" : function() { $("#finder").hide(); },
                "itemChosen" : function() {
                    var url = ( this.path.length == 1 ) 
                        ? "/" : "/"+this.path.slice(1).join("/")+"/"
                    load_sheet(url);
                    this.deactivate();
                }
            };
            var finder = new Finder($("#finder").eq(0), data, opts);
            nav = new Navigation(finder);
        },

        update: function() {
            sheets[path].calc_size();
            layout.panes.refresh();
            layout.selection.show_selection();
            builder.render();
        }
    };
    
    data = new HN.Data(options);
    data.loadOpaque(document.location.pathname, "pages");
    data.addPage(path);

}();

