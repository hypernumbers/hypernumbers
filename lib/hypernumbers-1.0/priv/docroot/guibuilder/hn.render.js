HN.namespace("Widgets");

HN.Widgets.text = {

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
            ref.obj = {"x1":ref.obj.x, "y1":ref.obj.y, 
                       "x2":ref.obj.x, "y2":ref.obj.y};
        }

        var x,y,
        base  = $("<div></div>"),
        range = HN.Util.refIterator(ref.obj),
        tpl   = ( $(self).find(".row").length == 0 ) 
            ? HN.Render.buildRow(ref.obj.x2 - ref.obj.x1 + 1)
            : $(self).find(".row").eq(0).clone();

        while( y = range.nextRow() ) {
            var row = tpl.clone(), 
            index = 0;
            while( x = range.nextCol() ) {
                var val = read(y, x);
                row.find("[data-offset="+(++index)+"]").html(val);
            }
            base.append(row);
        }
        return base[0];
    }
};

HN.Widgets.form = {
    
    "render": function(self, data) {

        $(self).unbind().bind("submit", function(e) {

            e.preventDefault();

            var values = [], 
            items      = $(this).find("[data-binding-to]"),
            path       = "";

            items.each(function() {

                var ref = HN.Util.parseRef($(this).attr("data-binding-to")),
                type    = $(this).attr("data-type");
                
                values.push({"ref"     : HN.Util.refToStr(ref), 
                             "formula" : HN.Widgets[type].value(this)});
                             
                path = ref.path;

            });

            var data = {"set":{"list": values}},
            submit   = function() { HN.Widgets.form.submitted(self); };

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

HN.Widgets.radio = {    
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
            ref.obj = {"x1":ref.obj.x, "y1":ref.obj.y, 
                       "x2":ref.obj.x, "y2":ref.obj.y};
        }
        
        var $div = $("<div></div>"),
        range = HN.Util.refIterator(ref.obj);
        
        range.map(function(y, x) { 
            $div.append("<input type='radio'>"+read(y, x)+"&nbsp;&nbsp;");
        });
        return $div[0];

    }
};

HN.Widgets.select = {
    
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
            ref.obj = {"x1":ref.obj.x, "y1":ref.obj.y, 
                       "x2":ref.obj.x, "y2":ref.obj.y};
        }
        
        var $select = $("<select></select>"),
        range = HN.Util.refIterator(ref.obj);
        
        range.map(function(y, x) { 
            $select.append("<option>"+read(y, x)+"</option>");
        });
        return $select[0];
    },
    "value": function(self) { 
        return $(self).find("select").val();
    }

};

HN.Widgets.checkbox = {
    
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
            ref.obj = {"x1":ref.obj.x, "y1":ref.obj.y, 
                       "x2":ref.obj.x, "y2":ref.obj.y};
        }
        
        var $div = $("<div></div>"),
        range = HN.Util.refIterator(ref.obj);
        
        range.map(function(y, x) { 
            var val = read(y, x);
            $div.append("<input type='checkbox' value='"+val+"'>"
                        +val+"&nbsp;&nbsp;");
        });
        return $div[0];
    },
    "value": function(self) {
        var results = [];
        $.each($(self).find(":checked"), function() {
            results.push($(this).val());
        });
        return results.join(",");
    }
};


HN.Widgets.textarea = {
    
    "render": function(self, data) {
        var value = "", 
        from = self.getAttribute("data-binding-from");

        if( from !== null ) {
            var ref = HN.Util.parseRef(from);
            value = data.read(ref.path, ref.obj.y, ref.obj.x).value || "";
        }
        
        return $("<textarea>"+value+"</textarea>")[0];
    },
    
    "value": function(self) { 
        return $(self).find("textarea").val();
    }
};

HN.Widgets.input = {
    
    "render": function(self, data) {
        var value = "", 
        from = self.getAttribute("data-binding-from");

        if( from !== null ) {
            var ref = HN.Util.parseRef(from);
            value = data.read(ref.path, ref.obj.y, ref.obj.x).value || "";
        }
        
        return $("<input type='text' value='"+value+"' />")[0];
    },
    "value": function(self) { 
        return $(self).find("input").val();
    }
};


HN.namespace("Render");
HN.Render.buildRow = function(len) {
    for( var html = "<div class='row'>", x = 1; x < len+1; x++ ) {
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
                HN.Widgets.form.render(this, data);
            } else {
                var type = $(this).attr("data-type");
                var rendered = HN.Widgets[type].render(this, data);
                $(this).empty().append(rendered);
            }
        });
    };

    return public;
};