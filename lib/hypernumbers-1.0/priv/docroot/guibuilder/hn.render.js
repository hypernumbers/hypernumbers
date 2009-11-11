HN.namespace("Builder.Widgets");

HN.Builder.Widgets.text = {

    // TODO: 
    //   add rendering for row / columns
    //   add table rendering
    
    "render": function(self, data) {

        var from = self.getAttribute("data-binding-from");

        if( from == null ) { 
            return null;
        }

        var ref = HN.Util.parseRef(from),
        read    = function(y, x) { 
            return data.read(ref.path, y, x).value || "" 
        };
        
        if( ref.type == "cell" ) { 
            return $("<span>"+read(ref.obj.y, ref.obj.x)+"</span>")[0];       
        } else {

            var base = $("<div></div>"),
            inc      = (ref.obj.y1 > ref.obj.y2-1) ? -1 : 1,
            y        = ref.obj.y1,
            tpl      = ( $(self).find(".row").length == 0 ) 
                ? HN.Render.buildRow(ref.obj.x2 - ref.obj.x1 + 1)
                : $(self).find(".row").eq(0).clone();
            
            while(y != (ref.obj.y2-1) ) {

                var row = tpl.clone();

                for( var x = 0; x < ref.obj.x2 - ref.obj.x1 + 1; x++ ) {
                    var val = read(y, x+ref.obj.x1);
                    row.find("[data-offset="+(x+1)+"]").html(val);
                }

                base.append(row);
                y += inc;
            }
            return base[0];
        }
    }
};

HN.Builder.Widgets.form = {
    
    "render": function(self) {
        $(self).unbind().bind("submit", function(e) {

            e.preventDefault();

            var values = [], 
            items      = $(this).find("[data-binding-to]"),
            path       = "";

            items.each(function() {

                var ref = HN.Util.parseRef($(this).attr("data-binding-to")),
                type    = $(this).attr("data-type");
                
                values.push({"ref"     : HN.Util.refToStr(ref), 
                             "formula" : HN.Builder.Widgets[type].value(this)});
                             
                path = ref.path;

            });

            var data = {"set":{"list": values}},
            submit   = function() { HN.Builder.Widgets.form.submitted(self); };

            $.post(path, JSON.stringify(data), submit, "json");
           
            return false;
        });
        return self;
    },
    "submitted": function(self) {
        $(self).empty()
            .append("<h2>Thanks for filling out our form!</h2>"
                    +"<p>Your response has entered our spreadsheet.</p>");
    }
};

HN.Builder.Widgets.radio = {
    
    "render": function(self) {
        return $("<input type='radio'>")[0];
    }
};

HN.Builder.Widgets.textarea = {
    
    "render": function(self) {
        var value = "", 
        from = self.getAttribute("data-binding-from");

        if( from !== null ) {
            var ref = HN.Util.parseRef(from);
            value = data.read(ref.path, y, x).value || "";
        }
        
        return $("<textarea>"+value+"</textarea>")[0];
    },
    
    "value": function(self) { 
        return $(self).find("textarea").val();
    }
};

HN.Builder.Widgets.input = {
    
    "render": function(self) {
        var value = "", 
        from = self.getAttribute("data-binding-from");

        if( from !== null ) {
            var ref = HN.Util.parseRef(from);
            value = data.read(ref.path, y, x).value || "";
        }
        
        return $("<input type='text' value='"+value+"' />")[0];
    },
    "value": function(self) { 
        return $(self).find("input").val();
    }
};


HN.namespace("Render");
HN.Render.buildRow = function(len) {
    for( var html = "<div class='row'>", x = 0; x < len; x++ ) {
        html += "<div data-offset='"+x+"'></div>";
    }
    return $(html+"</div>");
};

HN.HTMLRender = function(data)
{
    var public = {};
    
    public.render = function(doc) {
        
        $(doc).find(".hn").each( function() {
            if( this.nodeName == "FORM") { 
                HN.Builder.Widgets.form.render(this, data);
            } else {
                var type = $(this).attr("data-type"), 
                rendered = HN.Builder.Widgets[type].render(this, data);
                $(this).empty().append(rendered);
            }
        });
    };

    return public;
};

HN.RenderBoot = function()
{
    var options = {
        "stopUpdate"   : true,
        "dataLoaded"   : function() { htmlRender.render(document); },
        "dataReloaded" : function(data) {  },
        "update"       : function() { htmlRender.render(document); }
    };

    var data   = new HN.Data(options),
    htmlRender = new HN.HTMLRender(data);

    htmlRender.render(document);
}();