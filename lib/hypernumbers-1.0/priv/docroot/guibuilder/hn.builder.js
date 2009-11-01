HN.Builder = function()
{
    // Need an id to link model objects with dom/display objects
    this.globid = 1;
    this.configuring = null;
    this.state = HN.Builder.EDITING;
    this.model = this.set_model("<div></div>");

    this.dom = {
        "preview":HN.Util.id("preview")
    };
    
    for ( var i in HN.Builder.Widgets ) {
        $("#add").append( HN.Builder.Widgets[i].add );
    } 

    this.setup_events();
    this.select_tab("add");

    setTimeout( function() {
        $("#sheetpanel").addClass("disabled");
    }, 0);

    this.load_views();
};

HN.Builder.EDITING = 1;
HN.Builder.NOT_EDITING = 2;

HN.Builder.prototype.load_views = function()
{
    var that = this;
    
    $.getJSON("/?views", function(data) {
        $.each(data, function() {
            $("#views").append("<li name='"+this+"'>"+this+"</li>");
        });
    });
};

HN.Builder.prototype.load_html = function(name)
{
    var that = this;

    $.get("/views/"+name+".tpl", function(data) {
        $("#viewname").val(name);
        that.model = that.set_model(data);
        that.render_output();
    });
};

// Takes a string of html, gets jquery to build a dom object from it,
// then adds id's to each element so they can be referenced after cloning
HN.Builder.prototype.set_model = function(str)
{
    var base = $(str)[0];
    var that = this;
    
    var addIds = function(obj)
    {
        if( typeof obj.getAttribute != "undefined"
            && obj.getAttribute("data-hnid") == null ) {
            obj.setAttribute("data-hnid", ""+that.globid++);
        }

        for( var i = 0; i < obj.childNodes.length; i++ ) {
            addIds(obj.childNodes[i]);
        }
    };
    
    addIds(base);
    return base;
};

HN.Builder.prototype.select_tab = function(id) {
    $("#tabs a").removeClass("selected");
    $("#tabs a[name="+id+"]").addClass("selected");
    $(".configurepanel .tab").hide();
    $("#"+id).show();
};

HN.Builder.prototype.highlight_element = function(model, id) 
{
    $(".configuring").removeClass("configuring");
    
    this.configuring = 
        $(this.get_model_obj(model, id)).addClass("configuring");
};

HN.Builder.prototype.configure_element = function(obj)
{
    this.highlight_element($("#preview")[0], obj);
    HN.Builder.Widgets[obj.getAttribute("data-type")].configure(obj);
    this.select_tab("configure");               
};

HN.Builder.getId = function(obj) 
{
    return ( typeof obj == "object" ) 
        ? obj.getAttribute("data-hnid") 
        : obj;
};

HN.Builder.prototype.get_model_obj = function(model, id)
{
    id = HN.Builder.getId(id);

    if( HN.Builder.getId(model) == id ) {
        return model;
    }
    return $(model).find("[data-hnid="+id+"]")[0];
};


HN.Builder.prototype.setup_events = function()
{
    var that = this;

    $("#viewviews").click( function() {
        $("#views").toggle("medium")
    });

    var $saveview = $("#saveview");

    $saveview.click( function() {
        $saveview.val("saving..");
        var params = {"saveview": {
            "name": $("#viewname").val(),
            "tpl":  HN.Builder.render_html(that.model)
        }};
        $.post("/", JSON.stringify(params), function(data) {
            $saveview.val("saved!");
            setTimeout( function() { $saveview.val("save"); }, 1000);
        }, "json");
    });

    $("#viewname").focus( function() {
        this.select();
    });

    $("#views").click( function(e) {
        if( e.target.nodeName == "LI" ) {
            that.load_html(e.target.getAttribute("name"));
            $("#views").toggle("medium");
        }
    });

    $("#previewbtn").click( function() {
        if( that.state == HN.Builder.EDITING ) {
            that.state = HN.Builder.NOT_EDITING;
            $(".configuring").removeClass("configuring");
            $("#preview").removeClass("editing");
            $("#panelwrapper").hide();
            $(this).val("edit");
        } else {
            that.state = HN.Builder.EDITING;
            $("#preview").addClass("editing");
            $("#panelwrapper").show();
            $(this).val("preview");
        }
    });
                            
    
    $(".togglesubpanel").bind("click", function() {        
        $(this).parent("div").toggleClass("disabled");
    });
    
    $("#tabs a").bind("click", function() {
        that.select_tab($(this).attr("name"));
    });
    
    HN.Util.addEvent(HN.Util.id("toggleview"), "change", function() {
        if( this.checked ) {
            that.view_code();
        } else {
            that.model = that.set_model($("#previewcode")[0].value);
            that.render_output();
        }
    });

    HN.Util.addEvent(HN.Util.id("add"), "mousedown", function(e) {

        if( e.target.nodeName == "INPUT") {

            var obj = e.target.getAttribute("data-obj");
            var el = that.set_model(HN.Builder.Widgets[obj].create());
            
            that.start_drag(el, "clone", function(drop) {
                var tgt = drop.action == "click" ? null : drop.target;
                var id = that.globid++;
                el.setAttribute("data-hnid", ""+id);
                that.addElement(el, tgt);
                that.configure_element(el);
            });
        }
    });
    
    HN.Util.addEvent(this.dom.preview, "mousedown", function(e) {

        if( that.state != HN.Builder.EDITING ) {
            return;
        }

        var obj = HN.Builder.find_parent_hnobj(e.target);
        
        if( obj ) {

            var wrap = $(obj).parents(".container").length > 0 
                ? $(obj).parents(".container")
                : false;
        

            var id = obj.getAttribute("data-hnid");
            
            $(".configuring").removeClass("configuring");
            
            that.configuring = 
                $("#preview").find("[data-hnid="+id+"]"
                                  ).addClass("configuring");

    
            that.start_drag(obj, "remove", function(drop) { 
                
                var id = obj.getAttribute("data-hnid");
                var targ = that.get_model_obj(that.model, id);
                
                if( drop.action == "click" ) {
                    that.configure_element(targ);
                } else if( drop.action == "move" ) {
                    that.addElement(targ, drop.target);  
                    that.configure_element(targ);
                }
            }, wrap);
            e.preventDefault();
        }
    });
};

HN.Builder.prototype.start_drag = function(orig, remove, fun, $parent)
{
    document.body.style.cursor = "pointer";

    var that       = this;
    var droptarget = null;
    var $placer    = $(orig);//$("<div id='placer'> </div>");
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

HN.Builder.prototype.addElement = function(el, target)
{    
    if( !target || target.getAttribute("id") == "preview" ) {
        this.model.appendChild(el);
    } else {
        var targ = this.get_model_obj(this.model, target);
        var type = $(targ).attr("data-type");

        if( $(targ).hasClass("hn") && type != "blog" && type != "form" ) {
            targ.parentNode.insertBefore(el, targ);
        } else {
            targ.appendChild(el);
        }
    }

    this.render_output();
};

HN.Builder.prototype.view_code = function()
{
    this.dom.preview.innerHTML =
        "<textarea id='previewcode'>"
        +HN.Builder.render_html(this.model)
        +"</textarea>";
};

HN.Builder.prototype.render_output = function()
{
    var toRender = $(this.model).clone()[0];
    var view = HN.Builder.render_model(toRender, "preview");
    $(this.dom.preview).empty().append(view);
    if( this.configuring && this.state == HN.Builder.EDITING ) {
        var id = this.configuring.attr("data-hnid");
        $(view).find("[data-hnid="+id+"]").addClass("configuring");
    }
};

HN.Builder.render_model = function(obj, rendertype)
{
    if( HN.Builder.is_hn_obj(obj) ) {
        return HN.Builder.Widgets[obj.getAttribute("data-type")].render(obj);
    } else {
        for(var i = 0; i < obj.childNodes.length; i++) {
            var nchild = HN.Builder.render_model(obj.childNodes[i], rendertype);
            obj.replaceChild(nchild, obj.childNodes[i]);
        }
        return obj;
    }
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

HN.Builder.render_html = function(obj)
{
    if( obj.nodeName == "#text" ) {
        return obj.textContent;
    }
    
    var tag = obj.nodeName.toLowerCase();
    
    for( var children="",  i=0; i<obj.childNodes.length; i++ ) {
        children += this.render_html(obj.childNodes[i]);
    }
    
    for( var attr = "", x = 0; x<obj.attributes.length; x++ ) {
        var tmp = obj.attributes[x];
        if( tmp.nodeName != "data-hnid" ) {
            attr += " "+tmp.nodeName+"='"+tmp.nodeValue+"'";
        }
    }
    return "<"+tag+attr+">" + children + "</"+tag+">";
};

HN.Builder.Widgets = {};

HN.Builder.Widgets.text = {
    
    "add" : "<div><input type='button' value='text' "
        +"class='btn' data-obj='text' /></div>",
    
    "create":  function() { 
        return "<div class='hn' data-type='text'>text button</div>";
    },

    "configure": function(self) {
        
        var panel = $("#configuretextpanel").contents().clone();
        $("#configure").empty().append(panel);

        if( $(self).hasClass("hnheader") ) {
            panel.find(".header").attr("checked", "checked");
        }

        panel.find(".header").bind("change", function() {
            if( this.checked ) {
                $(self).addClass("hnheader");
            } else {
                $(self).removeClass("hnheader");
            }
            builder.render_output();
        });
        
        $("#use_range").unbind().click( function() {
            var str = HN.Util.range_to_str2(layout.selection.bounds);
            self.setAttribute("data-binding-from", str);
            $("#sheetpanel").addClass("disabled");
            HN.Builder.Widgets[self.getAttribute("data-type")]
                .configure(self);
            builder.render_output();            
        });
        

        var bval = self.getAttribute("data-binding-from");
        panel.find(".bind").val(bval).focus( function() {
            $("#sheetpanel").removeClass("disabled");
        });
        
        panel.find(".delete").click(function() {
            self.parentNode.removeChild(self);
            builder.render_output();
        });
    },

    "render": function(self) {

        var text, from = self.getAttribute("data-binding-from");

        if( from === null ) {
            text = "Click on me to configure";
        } else {
            var ref = HN.Util.parse_ref(from);
            text = sheet.cell(ref.y, ref.x).value || "Blank Cell";
        }

        return $("<div class='"+self.className+"' data-hnid='"
                 +self.getAttribute("data-hnid")
                 +"'>"+text+"</div>")[0];
    }
};

HN.Builder.Widgets.blog = {
    
    "add" : "<input type='button' value='blog' "
        +"class='btn' data-obj='blog' />",
    
    "create":  function() { 
        return "<div class='hn blog' data-type='blog'>"
            +"<div class='hn hnheader' data-type='text'>Example Blog</div>"
            +"<div class='hn by' data-type='text'>posted by joe Blog</div><hr />"
            +"<div class='hn' data-type='text'>bind to range in the "
            +"spreadsheet</div></div>";
    },
    
    "configure": function(self) {
        
        var panel = $("#configuregridpanel").contents().clone();
        $("#configure").empty().append(panel);

        $("#use_range").unbind().click( function() {
            var str = HN.Util.range_to_str2(layout.selection.bounds);
            self.setAttribute("data-binding-from", str);
            $("#sheetpanel").addClass("disabled");
            builder.render_output();            
            HN.Builder.Widgets[self.getAttribute("data-type")]
                .configure(self);
        });

        var bval = self.getAttribute("data-binding-from");
        panel.find(".bind").val(bval).focus( function() {
            $("#sheetpanel").removeClass("disabled");
        });
        
        panel.find(".delete").click(function() {
            self.parentNode.removeChild(self);
            builder.render_output();
        });
    },

    "render": function(self) {

        var id = "data-hnid='"+self.getAttribute("data-hnid")+"'";
        var from = self.getAttribute("data-binding-from");

        if( from === null ) {
            return self;
        } 
        
        var ref = HN.Util.parse_range(from);        
        var div = $("<div class='hn grid' "+id+"></div>")[0];

        var nx, ny = -1;

        for( var y = ref.y1; y <= ref.y2; y++ ) { 
            nx = ref.x1;
            var view = $("<div class='container' "+id+">"
                         +self.innerHTML+"</div>");
            
            $.each(view.children(".hn"), function() {
                $(this).attr("data-binding-from", 
                             HN.Util.coord_to_ref({x:nx++, y:ny}));
            });
            
            div.appendChild(HN.Builder.render_model(view[0]));
            ny -= 1;
        }

        return div;
    }
};

HN.Builder.Widgets.form = {
    
    "add" : "<div><input type='button' value='form' "
        +"class='btn' data-obj='form' /></div>",
    
    "create":  function() { 
        return "<div class='hn' data-type='form'></div>";
    },

    "configure": function(self) {
        
        var panel = $("#configuregridpanel").contents().clone();
        $("#configure").empty().append(panel);

        $("#use_range").unbind().click( function() {
            var str = HN.Util.range_to_str2(layout.selection.bounds);
            self.setAttribute("data-binding-from", str);
            $("#sheetpanel").addClass("disabled");
            builder.render_output();            
            HN.Builder.Widgets[self.getAttribute("data-type")]
                .configure(self);
        });
        

        var bval = self.getAttribute("data-binding-from");
        panel.find(".bind").val(bval).focus( function() {
            $("#sheetpanel").removeClass("disabled");
        });
        
        panel.find(".delete").click(function() {
            self.parentNode.removeChild(self);
            builder.render_output();
        });
    },

    "render": function(self) {

        var id = "data-hnid='"+self.getAttribute("data-hnid")+"'";
        var from = self.getAttribute("data-binding-from");

        var $form = $("<div class='"+self.className+"' "+id
                      +" data-type='form'></div>");

        if( self.childNodes.length == 0 ) {
            return $form.append("please add some inputs to me</div>")[0];
        }

        for( var x = 0; x < self.childNodes.length; x++ ) { 
            var i = self.childNodes[x];
            $form.append(HN.Builder.render_model(i));
        }
        $form.append("<input type='button' value='submit' class='submit' />");

        if( from != null ) {

            $form.find(".submit").click( function() {

                var ref = HN.Util.parse_range(from), vals=[];
                $form.find("input[type=text]").each(function() {
                    var col = HN.Util.to_b26(ref.x1++);
                    vals.push({ref:col+":"+col, formula:$(this).val()});
                });

                var data = {"set":{"list":vals}};
                $.post(document.location.pathname + from, 
                       JSON.stringify(data), null, "json");
            });
        }
 
        return $form[0];
    }
};


HN.Builder.Widgets.input = {
    
    "add" : "<div><input type='button' value='input' "
        +"class='btn' data-obj='input' /></div>",
    
    "create":  function() { 
        return "<div class='hn' data-type='input'>"
            +"<input type='text' value='Configure Me' />"
            +"</div>";
    },
    
    "configure": function(self) {
        
        var panel = $("#configuregridpanel").contents().clone();
        $("#configure").empty().append(panel);
        
        $("#use_range").unbind().click( function() {
            var str = HN.Util.range_to_str2(layout.selection.bounds);
            self.setAttribute("data-binding-from", str);
            $("#sheetpanel").addClass("disabled");
            builder.render_output();            
            HN.Builder.Widgets[self.getAttribute("data-type")]
                .configure(self);
        });
        
        var bval = self.getAttribute("data-binding-from");
        panel.find(".bind").val(bval).focus( function() {
            $("#sheetpanel").removeClass("disabled");
        });
        
        panel.find(".delete").click(function() {
            self.parentNode.removeChild(self);
            builder.render_output();
        });
    },

    "render": function(self) {
        return $("<div class='"+self.className+"' data-hnid='"
                 +self.getAttribute("data-hnid")+"'>"
                 +"<label>text</label><input type='text' />"
                 +"</div>")[0];

        return self;
    }
};

var sheet, layout, builder;
var options = {
    
    dataLoaded: function() {
        sheet   = new HN.Sheet(data.pages[document.location.pathname]);
        builder = new HN.Builder(sheet);
        layout  = new HN.Layout(sheet);
    },
    
    dataReloaded: function(data) {
        console.log("reload");
    },

    update: function() {
        sheet.calc_size();
        layout.panes.refresh();
        layout.selection.show_selection();
        builder.render_output();
    }
};

var data = new HN.Data(options);
data.addPage(document.location.pathname);