HN.Builder = function()
{
    // This is the html that is being built
    this.globid = 1;
    this.model = this.set_model(HN.Builder.DEFAULT);
    this.configuring = null;

    this.dom = {
        "preview":HN.Util.id("preview")
    };
    
    for ( var i in HN.Builder.Widgets ) {
        $("#add").append( HN.Builder.Widgets[i].add );
    } 
    
    this.setup_events();
    this.select_tab("add");
    this.render_output();
};

HN.Builder.DEFAULT = "<div class='ventris'>"
    +"\n\t<div class='header'><div class='title'><h1 class='hn' data-type='text' data-binding-from='A1'></h1></div></div>"
    +"\n\t<div class='container'>"
    +"\n\t\t<div class='content'><p class='hn' data-type='text'></p></div>"
    +"\n\t\t<div class='rightpanel'><p class='hn' data-type='text'></p></div></div>"
    +"\n\t\t<div class='footer'><p class='hn' data-type='text'></p></div>\n</div>";

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

HN.Builder.prototype.configure_element = function(obj)
{
    var id = obj.getAttribute("data-hnid");
    if( this.configuring !== null ) {
        this.configuring.removeClass("configuring");
    }
    
    this.configuring = 
        $("#preview").find("[data-hnid="+id+"]")
        .addClass("configuring");
    
    HN.Builder.Widgets[obj.getAttribute("data-type")]
        .configure(obj);
    this.select_tab("configure");               
};

HN.Builder.prototype.setup_events = function()
{
    var that = this;
    
    $(".togglesubpanel").bind("click", function() {
        
        var parent = $(this).parent("div");
        if( parent.hasClass("disabled") ) {
            parent.removeClass("disabled").children(".subpanel").show();
        } else {
            parent.addClass("disabled").children(".subpanel").hide();
        }
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
            that.start_drag(e.target, "clone", function(drop) {
                var tgt = drop.action == "click" ? null : drop.target;
                var obj = e.target.getAttribute("data-obj");
                var el = HN.Builder.Widgets[obj].create();
                var id = that.globid++;
                el.setAttribute("data-hnid", ""+id);
                that.addElement(el, tgt);
                that.configure_element(el);
            });
        }
    });
    
    HN.Util.addEvent(this.dom.preview, "mousedown", function(e) {

        var obj = HN.Builder.find_parent_hnobj(e.target);
        
        if( obj ) {
            
            that.start_drag(e.target, "remove", function(drop) { 
                
                var id = obj.getAttribute("data-hnid");
                var targ = that.get_model_obj(id);
                
                if( drop.action == "click" ) {
                    that.configure_element(targ);
                } else if( drop.action == "move" ) {
                    that.addElement(targ, drop.target);  
                    that.configure_element(targ);
                }
            });
            e.preventDefault();
        }
    });
};

HN.Builder.prototype.get_model_obj = function(id)
{
    if( typeof id == "object" ) {
        id = id.getAttribute("data-hnid");
    }

    if( this.model.getAttribute("data-hnid") == id ) {
        return this.model;
    }

    return $(this.model).find("[data-hnid="+id+"]")[0];
};

HN.Builder.prototype.start_drag = function(orig, remove, fun)
{
    var that = this;
    var obj = null;
    var droptarget = null;
    var placer = $("<div id='placer'> </div>");
    
    var check_target = function(obj) {
        
        if( droptarget && obj == droptarget 
            || obj == orig 
            || obj == placer[0]) {
            return;
        }
        
        if( obj.nodeName == "DIV") {
            droptarget = $(obj).append(placer.remove())[0];
        } else {
            droptarget = $(obj).before(placer.remove())[0];
        }
    };
    
    var move = function(e) {
        var css = {top:e.clientY+10+"px", "left":e.clientX+10+"px"};
        if( obj === null ) {
            var tmp = remove == "remove" ? $(orig).remove() : $(orig).clone();
            obj = tmp.css({"position":"absolute"})
                .css(css).appendTo("body");
        } else {
            obj.css(css);
            check_target(e.target);
        }
    };

    var up = function(e) {

        HN.Util.removeEvent(document, "mousemove", move);
        HN.Util.removeEvent(document, "mouseup", up);
        
        placer.remove();
        
        if( obj ) {
            obj.remove();
            fun({"action":"move", "target":droptarget});
        } else {
            fun({"action":"click"});
        }
    };
    
    HN.Util.addEvent(document, "mousemove", move);
    HN.Util.addEvent(document, "mouseup", up);
};

HN.Builder.prototype.addElement = function(el, target)
{    
    if( target == null ) {
        this.model.appendChild(el);
    } else {

        var targ = this.get_model_obj(target);
        if( targ.parentNode && targ.nodeName != "DIV" ) {
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
    $(this.dom.preview).empty().append(this.render_model(toRender, "preview"));
    
};

HN.Builder.prototype.render_model = function(obj, rendertype)
{
    if( HN.Builder.is_hn_obj(obj) ) {
        return HN.Builder.Widgets[obj.getAttribute("data-type")].render(obj);
    } else {
        for(var i = 0; i < obj.childNodes.length; i++) {
            var nchild = this.render_model(obj.childNodes[i], rendertype);
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
  //      if( tmp.nodeName != "data-hnid" ) {
            attr += " "+tmp.nodeName+"='"+tmp.nodeValue+"'";
  //      }
    }
    
    return "<"+tag+attr+">" + children + "</"+tag+">";
};

HN.Builder.Widgets = {};
HN.Builder.Widgets.text = {
    
    "add" : "<input type='button' value='text' "
        +"class='btn' data-obj='text' />",
    
    "create":  function() { 
        return $("<p class='hn' data-type='text'></p>")[0];
    },

    "configure": function(self) {
        
        var type = $("<select><option value='P'>Paragraph</option>"
                     +"<option value='H1'>Header 1</option></select>");
        
        type.val(self.nodeName).bind("change", function() {
            var nobj = HN.Builder.clone_with_new_tag(self, $(this).val());
            self.parentNode.replaceChild(nobj, self);
            builder.render_output();
        });
        
        var del = $("<input type='button' value='delete' />").click(function() {
            self.parentNode.removeChild(self);
            builder.render_output();
        });
        
        //var form = $("<br /><label>Binding <input type='text' /></label>");
        $("#configure").empty().append(type).append("<br />").append(del);
    },

    "render": function(self) {

        var text, from = self.getAttribute("data-binding-from");
        
        if( from === null ) {
            text = "Lorum Ipsum";
        } else {
            var ref = HN.Util.parse_ref("A1");
            text = sheet.cell(ref.y, ref.x).value;
        }
        var tag = self.nodeName.toLowerCase();
        return $("<"+tag+" class='hn' data-hnid='"
                 +self.getAttribute("data-hnid")
                 +"'>"+text+"</"+tag+">")[0];
    }
};

var sheet, layout, builder;
var options = {
    
    dataLoaded: function() {
        sheet  = new HN.Sheet(data);
        builder = new HN.Builder(sheet);
        layout = new HN.Layout(sheet);
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

var data = new HN.Data(document.location.pathname, options);
