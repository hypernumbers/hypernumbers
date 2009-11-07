HN.namespace("Builder.Widgets");

HN.Builder.Widgets.text = {
    
    "render": function(self, data) {
        
        
        var text, from = self.getAttribute("data-binding-from");
        
        if( from === null ) {
            text = "Click on me to configure";
        } else {
            var ref = HN.Util.parseRef(from);
            text = data.read(ref.path, ref.obj.y, ref.obj.x).value
                || "Blank Cell";
        }
        
        return $("<div class='"+self.className+"' data-hnid='"
                 +self.getAttribute("data-hnid")
                 +"'>"+text+"</div>")[0];
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

// var sheet, layout, render;
// var options = {
    
//     dataLoaded: function() {
//         sheet   = new HN.Sheet(data.pages[document.location.pathname]);
//         builder = new HN.Builder(sheet);       
//     },
    
//     dataReloaded: function(data) {
//         console.log("reload");
//     },

//     update: function() {
//         sheet.calc_size();
//         layout.panes.refresh();
//         layout.selection.show_selection();
//         builder.render_output();
//     }
// };

// var data = new HN.Data(options);
// data.addPage(document.location.pathname);

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
        
        dataLoaded: function() {
            console.log("loaded");
        },
        
        dataReloaded: function(data) {
            console.log("reload");
        },
        
        update: function() {
            console.log("update");
        }
    };


    var data = new HN.Data(options),
    render = new HN.HTMLRender(data);
   
    render.render(document);
}();