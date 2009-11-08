HN.namespace("Builder.Widgets");

HN.Builder.Widgets.text = {
    
    "render": function(self, data) {
        
        var from = self.getAttribute("data-binding-from"),
        ref      = HN.Util.parseRef(from),
        read     = function(y, x) { 
            return data.read(ref.path, y, x).value || "" 
        };
        
        if( ref.type == "cell" ) { 
            return $("<span>"+read(ref.obj.y, ref.obj.x)+"</span>")[0];       
        }    
        // Single Row
        else if ( ref.type == "range" && ref.obj.y1 == ref.obj.y2 ) { 

            for( var html = "<div>", x = ref.obj.x1; x < ref.obj.x2; x++ ) {
                html += "<div>"+read(ref.obj.y1, x)+"</div>";
            }
            return $(html+"</div>")[0];       
        } 
        else {
            
            var base = $("<div></div>"),
            tpl      = $(self).find(".row").eq(0).clone(),
            inc = (ref.obj.y1 > ref.obj.y2) ? -1 : 1;
            y = ref.obj.y1;
            
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

HN.Builder.Widgets.blog = {
    
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
    
    "render": function(self) {
        return $("<div class='"+self.className+"' data-hnid='"
                 +self.getAttribute("data-hnid")+"'>"
                 +"<label>text</label><input type='text' />"
                 +"</div>")[0];

        return self;
    }
};

HN.HTMLRender = function(data)
{
    var public = {};
    
    public.render = function(doc) {
        
        $(doc).find(".hn").each( function() {
            var type = $(this).attr("data-type"), 
            rendered = HN.Builder.Widgets[type].render(this, data);
            $(this).empty().append(rendered);
        });
    };

    return public;
};

HN.RenderBoot = function()
{
    var options = {
        dataLoaded   : function() { htmlRender.render(document); },
        dataReloaded : function(data) {  },
        update       : function() { htmlRender.render(document); }
    };

    var data   = new HN.Data(options),
    htmlRender = new HN.HTMLRender(data);

    htmlRender.render(document);
}();