HN.namespace("Widgets");

HN.Widgets.text = {

    "render": function(self, data) {

        var from = self.getAttribute("data-binding-from");
        var to = self.getAttribute("data-binding-to");

        if( from == null ) {
            return null;
        }

        var ref  = HN.Util.parseRef(from);

        if( ref.type == "cell" ) {
            var val = "";
            if ( to == null ) {
                val = data.readCell(ref.path, ref.obj.y, ref.obj.x).value || "";
            } else {
                val = data.readCell(ref.path, ref.obj.y, ref.obj.x).formula || "";
            }
            var css = data.lookupCSS(ref.path, ref.obj.y, ref.obj.x) || "";
            return $("<span style='"+css+"'>"+val+"</span>")[0];
        }

        var hasInnerTpl = $(self).find(".tpl").length != 0; 
        var base        = ( hasInnerTpl ) 
            ? $("<div></div>") 
            : $("<table></table>");
        var range       = HN.Util.refIterator(ref.obj);
        var tpl         = ( hasInnerTpl )
            ? $(self).find(".tpl").eq(0).clone()         
            : HN.Render.buildRow(ref.obj.x2 - ref.obj.x1 + 1);

        var x,y;
        while( y = range.nextRow() ) {
            var $row  = tpl.clone();
            var index = 0;
            while( x = range.nextCol() ) {
                var val  = data.readCell(ref.path, y, x).value || "";
                var css  = data.lookupCSS(ref.path, y, x) || "";
                var cell = $row.find("[data-offset="+(++index)+"]");
                cell.html(val).attr("style", css + cell.attr("style"));
            }
            base.append($row);
        }
        
        return base[0];
    },
    "value": function(self) {
        return $(self).find("span").text();
    }
};

HN.Widgets.form = {

    "render": function(self, data, renderer) {

        // mark all bits on the form as being form elements
        var controls = $(self).find(".hn");
        var path     = document.location.pathname;
        
        controls.each(function() {
            this.setAttribute("data-in-form", "true");
        });
        
        $(self).unbind().bind("submit", function(e) {

            e.preventDefault();
            
            var invalid = false;
            var values = [];
            var items  = $(this).find("[data-binding-to]");

            items.each(function() {

                var type = $(this).attr("data-type");
                var value = HN.Widgets[type].value(this);
                
                if( !value && $(this).attr("data-required") ) { 
                    invalid = this;
                }

                var ref  = HN.Util.parseRef($(this).attr("data-binding-to"));
                var p    = HN.Util.relToAbsPath(path, ref.path);
                
                values.push({ 
                    "ref"     : p + HN.Util.refToStr(ref),
                    "formula" : value 
                });
            });

            if( invalid ) { 
                $(invalid).addClass("error")
                    .append("<div class='inputrequired'>"
                           +"* this field is required</div>");
                return;
            }
            
            $.ajax({
                "type"     : "POST",
                "url"      : path+"?view="+$("body").attr("data-view"),
                "dataType" : "json",
                "data"     : JSON.stringify({"set":{"list": values}}),
                "success"  : function(urm) { 
                    HN.Widgets.form.submitted(self, data); 
                }, 
                "error"    : function() {
                    $(self).prepend("<div class='error'>Sorry, There was an "+
                                    "error Processing this form, did you "+
                                    "fill everything in?</div>");
                }
            });
            
            return false;
        });

        if( $(self).find("input[type=submit]").length == 0 &&
            !$(self).attr("data-submitted") ) {
            $(self).append("<input type='submit' value='Submit Form' />");
        }

        return self;
    },
    "submitted": function(self, data) {

        var to = self.getAttribute("data-binding-to");

        if( !to ) { 
            var msg = "<p>Thanks for filling out our form! "
                + "Your response has been entered into our spreadsheet</p>";
        } else { 
            var ref = HN.Util.parseRef(to);
            var $tmp = $("<div data-binding-from='"+to+"' "
                         +"class='hn' data-type='text'></div>");
            var msg = $tmp.append(HN.Widgets.text.render($tmp[0], data));
        }
        $(self).attr("data-submitted", "true").empty().append(msg);
    }
};

HN.Widgets.radio = {

    "render": function(self, data) {

        var from = self.getAttribute("data-binding-from");
        if( from == null ) {
            return null;
        }

        var ref = HN.Util.parseRef(from);
        var read    = function(y, x) {
          return data.readCell(ref.path, y, x).formula || "";
        };
        if( ref.type == "cell" ) {
            ref.obj = {"x1":ref.obj.x, "y1":ref.obj.y,
                       "x2":ref.obj.x, "y2":ref.obj.y};
        }

        var $div  = $("<div class='radio'></div>");
        var range = HN.Util.refIterator(ref.obj);
        var name = "rand_"+String((new Date()).getTime()).replace(/\D/gi,'')
        
        range.map(function(y, x) {

            var val    = read(y, x);
            var randId = name + "-"+x+"-"+y;

            $div.append("<label for="+randId+">&nbsp;&nbsp;"
                        +"<input type='radio' value='"+val+"' id='"
                        +randId+"' name='"+name+"'>"+val+"</label><br/>");
        });

        var to = self.getAttribute("data-binding-to");
        if( to ) { 
            to = HN.Util.parseRef(to);
            if( to.type == "cell" ) { 
                $div.find("[value="+read(to.obj.y, to.obj.x)+"]").attr("checked", "checked");
            } else { 
                $div.find("input:first").attr("checked", "checked");
            }   
        } else { 
            $div.find("input:first").attr("checked", "checked");
        }            

        // Nastiest code ever written, ie wont set checked dynamically
        // so need to reparse http://dev.jquery.com/ticket/6377
        if( $.browser.msie ) { 
            var html = $('<div>').append($div.clone()).remove().html();
            return $(html)[0];            
        } else {
            return $div[0];
        }
    },
    "value": function(self) {
        return $(self).find(":checked").val();
    }

};

HN.Widgets.select = {

    "render": function(self, data) {

        var from = self.getAttribute("data-binding-from");

        if( from == null ) {
            return null;
        }

        var ref  = HN.Util.parseRef(from);
        var read = function(y, x) {
          return data.readCell(ref.path, y, x).formula || "";
        };
        if( ref.type == "cell" ) {
            ref.obj = {"x1":ref.obj.x, "y1":ref.obj.y,
                       "x2":ref.obj.x, "y2":ref.obj.y};
        }

        var $select = $("<select></select>");
        var range   = HN.Util.refIterator(ref.obj);

        range.map(function(y, x) {
            $select.append("<option>"+read(y, x)+"</option>");
        });

        var to   = self.getAttribute("data-binding-to");
        if( to ) { 
            to = HN.Util.parseRef(to);
            if( to.type == "cell" ) { 
                $select.val(read(to.obj.y, to.obj.x));
            }
        }

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
          return data.readCell(ref.path, y, x).formula || "";
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
            value = data.readCell(ref.path, ref.obj.y, ref.obj.x).value || "";
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
            value = data.readCell(ref.path, ref.obj.y, ref.obj.x).formula || "";
        }
            return $("<input type='text' value='"+value+"' />")[0];
    },
    "value": function(self) {
        return $(self).find("input").val();
    }
};


HN.namespace("Render");
HN.Render.buildRow = function(len) {
    var width = parseInt(100 / len) + "%";
    for( var html = "<tr class='row'>", x = 1; x < len+1; x++ ) {
        html += "<td data-offset='"+x+"' style='width:"+width+"'> </td>";
    }
    return $(html+"</tr>");
};

var hn = {};
hn.currentPath = function() {
    return document.location.pathname;
};

HN.HTMLRender = function(data)
{
    var public = {};
    
    // Renders dom as valid xhtml (we need to parse as xml)
    public.renderHtml = function(obj) { 

        if( !obj.nodeName ) { 
            return renderHtmlNodeArray(obj);
        }
        
        if( obj.nodeName == "#text" ) {
            return htmlEntities(obj.textContent);
        }
        
        var tag = obj.nodeName.toLowerCase();
                
        for( var attr = "", x = 0; x<obj.attributes.length; x++ ) {
            var tmp = obj.attributes[x];
            if( tmp.nodeName != "data-hnid" ) {
                attr += " "+tmp.nodeName+"='"+htmlEntities(tmp.nodeValue)+"'";
            }
        }
        
        return "<"+tag+attr+">" 
            + renderHtmlNodeArray(obj.childNodes)
            + "</"+tag+">";
    };
    
    public.render = function(doc) {

        $(doc).find(".hn").each( function() {

            if( this.nodeName == "FORM") {
                HN.Widgets.form.render(this, data);
            } else {

                var type       = $(this).attr("data-type");
                var rendered   = HN.Widgets[type].render(this, data);
                var is_in_form = $(this).attr("data-in-form");
                var data_to    = $(this).attr("data-binding-to");

                $(this).empty().append(rendered);
                //$(this)[0].innerHTML = $(rendered).html();

                if (!is_in_form && (data_to && (
                    type === "input"
                        || type === "textarea"
                        || type === "select"
                        || type === "radio"
                        || type === "checkbox"))) {

                    var change = function(e) {

                        e.preventDefault();
                        
                        var ref = HN.Util.parseRef(
                            $(this).closest(".hn").attr("data-binding-to"));
                        var data = {"set": {"ref"     : HN.Util.refToStr(ref),
                                            "formula" : e.currentTarget.value}};
                        var path = ref.path + HN.Util.refToStr(ref) 
                            + "?view=" + $("body").attr("data-view");

                        $.post(path, JSON.stringify(data), null, "json");
                        return false;
                    }

                    if( type == "select" ) {                         
                        $(this).find("select").unbind().bind("change", change);
                    } else {
                        $(this).find("input").unbind().bind("blur", change);
                    }
                } else {
                    $(this);
                }
            }
        });
    };

    function htmlEntities(str) { 
        return str.replace(/&/g, "&amp;");
    };

    function renderHtmlNodeArray(arr) {
        for( var html="",  i=0; i<arr.length; i++ ) {
            html += public.renderHtml(arr[i]);
        }
        return html;
    }

    return public;
};