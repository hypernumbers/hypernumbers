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

var builder;

HN.Builder = function(data, layout)
{
    var public  = {},
    globid      = 1,
    configuring = null,
    toOrFrom    = null,
    state       = "EDITING",
    render      = new HN.HTMLRender(data);

    for ( var i in HN.Widgets ) {
        if( typeof HN.Widgets[i].add != "undefined" ) {
            $("#add").append( "<input type='button' value='"+i+"' "
                              +"class='btn' data-obj='"+i+"' />" );
        }
    }

    loadViews();
    setupEvents();
    selectTab("templates");

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

        if( configuring ) {
            configureElement(configuring);
        }
    };

      public.sanityCheck = function() {
        var elements = $(document).find(".hn");
        var ret =  HN.Sanity.check(elements);
        var text = "";
        for (var i = 0; i < ret.length; i++) {
          switch (ret[i].status) {
            case "ok":
              "ok";
              break;
            default:
              text += ret[i].element + "\n";
              if (!(ret[i].errors == "")) {
                text += ret[i].errors + "\n";
              };
              if (!(ret[i].warnings == "")) {
                text += ret[i].warnings + "\n";
              };
              if (!(ret[i].notes == "")) {
                text += ret[i].notes + "\n";
              };
            text += "---------------------------------------------------------\n";
          };
        }
        console.log(text);
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

        var $saveview = $("#saveview");

        $("#viewviews").click( function() { $("#views").toggle("medium"); });
        $("#viewname").focus( function() { this.select(); });

        $(".togglesubpanel").bind("click", function() {
            $(this).parent("div").toggleClass("disabled");
        });

        $("#changetype").bind("change", function() {
            var obj = getModelObj(public.model, configuring);
            $(obj).attr("data-type", $(this).val());
            public.render();
        });

        $("#tabs a").bind("click", function() {
            selectTab($(this).attr("name"));
        });

        $("#bindto, #bindfrom").focus( function() {
            toOrFrom = ($(this).attr("id") == "bindto")
                ? "bindTo" : "bindFrom" ;
            $("#sheetpanel").removeClass("disabled");
        });

        $("#use_range").click( function() {
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

            $saveview.val("saving...");

            var global = $("#saveglobal").is(":checked");

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
                selectTab("add");
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

        $("#sanitycheckbtn").click( function() {
              public.sanityCheck();
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
        $placer    = $(orig);//
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
        $("#tabs a[name="+id+"]").addClass("s  elected");
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
            if( tmp.nodeName != "data-hnid" ) {
                attr += " "+tmp.nodeName+"='"+tmp.nodeValue+"'";
            }
        }
        return "\n<"+tag+attr+">" + children + "</"+tag+">";
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
        return false;
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

            sheets[path] = new HN.Sheet(data.getPageData(path));

            if( !layout ) {

                layout  = new HN.Layout(sheets[path]);

                builder = new HN.Builder(data, layout);
                builder.model = builder.makeModelObj("<div></div>");

                if( $.inArray("admin", data.getPage(path).groups) != -1 ) {
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
                                ? "/" : "/"+this.path.slice(1).join("/")+"/";
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

/**
 * * @class HN.Sanity
 *
 */

HN.namespace("Sanity");
HN.Sanity.in_form = {};
HN.Sanity.not_in_form = {};

HN.Sanity.check = function(elements) {
  var results = [];
  var ret;
  for (var i = 0; i < elements.length; i++) {
    var type = $(elements[i]).attr("data-type");
    var to      = HN.Sanity.parse($(elements[i]).attr("data-binding-to"));
    var from    = HN.Sanity.parse($(elements[i]).attr("data-binding-from"));
    var in_form = HN.Sanity.in_form($(elements[i]).attr("data-in-form"));
    var element = "element: " + type + " - from: " + from.path + " : " + from.ref
      + " - to: " + to.path + " : " + to.ref;
    ret = HN.Sanity.check_element(element, type, in_form, to.path, to.type, to.ref,
                                  from.path, from.type, from.ref);
    results[results.length] = ret;
  };
  return results;
};

HN.Sanity.check_element = function(element, type, in_form, to_path, to_type, to_ref,
                                   from_path, from_type, from_ref) {
  var status = "ok";
  var notes = "";
  var error = "";
  var warning = "";
  //
  // First check elements when they are not in forms
  if (in_form === "not_in_form") {
    if (type === "text") {
      // lables really need to have a from...
      if (!from_type) {
        status = "not ok";
        error = "ERROR: element of type text (which is not in a form) has no 'from' data";
      } else if (from_type === "row_range"
                 || from_type === "row_from_last"
                 || from_type === "col_range"
                 || from_type === "col_from_last") {
        status = "not ok";
        notes = "NOTE: What do we mean when we bind a text lable to a row or column?";
      } else if (to_type  && from_type
                 && (to_type !== from_type)) {
        status = "not ok";
        error = "ERROR: from_type: "  + from_type + " and to_type" + " are different";
      } else if (((from_type === "range" && to_type === "range")
                  || (from_type === "row_range_from_last" && to_type === "row_range_from_last")
                  || (from_type === "col_range_from_last" && to_type === "col_range_from_last"))
                 && !HN.Util.isCongruent(from_ref, to_ref)) {
        status = "not ok";
                   error = "ERROR: ranges " + from_ref + " and " + to_ref
                     + " are not the same size";
      };
    } else if ((type === "input") || (type === "textarea")) {
      if (!to_type) {
        status = "not ok";
        error = "ERROR: " + type + "s must go somewhere";
      } else if (!from_type) {
        status = "not ok";
        warning = "WARNING: " + type + " should come from somewhere when it's not in a form";
      } else if ((to_type === "cell") && (from_type === "cell") && (to_ref !== from_ref)) {
        status = "not ok";
        warning = "WARNING: " + type + " taking data from somewhere and putting it "
        + "somewhere else is likely to confuse your users";
      } else if ((to_type !== "cell") ||  (from_type !== "cell")) {
        status = "not ok";
        error = "ERROR: " + type + " must be getting data from and putting data to cells only "
        + "when not in a form";
      };
    } else if ((type === "select") || (type === "radio")) {
      if (!to_type) {
        status = "not ok";
        error = "ERROR: " + type + "s must go somewhere";
      } else if (!from_type) {
        status = "not ok";
        error = "ERROR: " + type + " must come from somewhere";
      } else if (to_type !== "cell") {
        status = "not ok";
        error = "ERORR: " + type + " can only send data to a cell when not in a form";
      } else if ((to_type === "cell") && HN.Sanity.overlaps(from_path, from_type, from_ref,
                                                            to_path, to_ref)) {
        status = "not ok";
        error = "ERROR: " + type + " cannot send data back to where it gets its options from";
      }
    } else if (type === "checkbox") {
      if (!to_type) {
        status = "not ok";
        error = "ERROR: " + type + "s must go somewhere";
      } else if (!from_type) {
        status = "not ok";
        error = "ERROR: " + type + " must come from somewhere";
      } else if ((to_type !== "cell") || (from_type !== "cell")) {
        status = "not ok";
        error = "ERORR: " + type + " can only send data to a cell when not in a form";
      }
    };
    //
    // Now check elements in forms
    //
  } else if (in_form === "in_form") {
    if (type === "text") {
      // lables really need to have a from...
      if (!from_type) {
        status = "not ok";
        error = "ERROR: element of type text is getting data from anywhere";
      } else if (!to_type) {
        status = "not ok";
        warning = "WARNING: element of type text in not being posted anywhere despite "
                + "being in a form";
      } else if (from_type === "row_range"
                 || from_type === "row_from_last"
                 || from_type === "col_range"
                 || from_type === "col_from_last") {
        status = "not ok";
        notes = "NOTE: What do we mean when we bind a text lable to a row or column?";
      } else if (to_type && from_type
                 && (to_type !== from_type)) {
        status = "not ok";
        error = "ERROR: from_type: "  + from_type + " and to_type" + " are different";
      } else if (((from_type === "range" && to_type === "range")
                  || (from_type === "row_range_from_last" && to_type === "row_range_from_last")
                  || (from_type === "col_range_from_last" && to_type === "col_range_from_last"))
                 && !HN.Util.isCongruent(from_ref, to_ref)) {
        status = "not ok";
                   error = "ERROR: ranges " + from_ref + " and " + to_ref
                     + " are not the same size";
      };
    } else if ((type === "input") || (type === "textarea")) {
      if (!to_type) {
        status = "not ok";
        error = "ERROR: " + type + "s must go somewhere";
      } else if (!((to_type === "cell")
                   || (to_type === "row_range")
                   || (to_type === "col_range"))
                   && (from_type !== "cell")) {
        status = "not ok";
        error = "ERROR: " + type + " must be getting data from a cell and is "
          + "trying to put it into a " + type;
      };
    } else if ((type === "select") || (type === "radio")) {
      if (!to_type) {
        status = "not ok";
        error = "ERROR: " + type + "s must go somewhere";
      } else if (!from_type) {
        status = "not ok";
        error = "ERROR: " + type + " must come from somewhere";
      } else if (!((to_type === "cell")
                   || (to_type === "row_range")
                   || (to_type === "col_range"))) {
        status = "not ok";
        error = "ERORR: " + type + " can only send data to a cell, row or column "
                                 + " when in a form";
      } else if (HN.Sanity.overlaps(from_path, from_type, from_ref,
                                    to_path, to_ref)) {
        status = "not ok";
        error = "ERROR: " + type + " cannot send data back to where it gets its options from";
      }
    } else if (type === "checkbox") {
      if (!to_type) {
        status = "not ok";
        error = "ERROR: " + type + "s must go somewhere";
      } else if (!from_type) {
        status = "not ok";
        error = "ERROR: " + type + " must come from somewhere";
      } else if (!((to_type === "cell")
                || (to_type === "row_range")
                 || (to_type === "col_range"))
                 || (from_type !== "cell")) {
        status = "not ok";
        error = "ERORR: " + type + " can only send data to a cell, row or column "
                                 + " when in a form";
      }
    };
  };
  return {element: element,
    status: status,
    notes: notes,
    errors: error,
    warnings: warning};
};

// checks if a CELL passed in as to_path and to_ref overlaps with whatever
// on the from side, be it a cell, range, row, col, blah-blah...
HN.Sanity.overlaps = function(from_path, from_type, from_ref, to_path, to_ref) {
  if (from_path !== to_path) {
    return false;
  } else {
    switch (from_type) {
      case "cell":
        if (from_ref === to_ref) {
          return true;
        } else {
          return false;
        };
      case "range":
        return HN.Sanity.inRange("range", HN.Util.parseRef(from_ref).obj,
                                 HN.Util.parseRef(to_ref).obj);
      case "row_range":
        return HN.Sanity.inRange("row", HN.Util.parseRef(from_ref).obj,
                                 HN.Util.parseRef(to_ref).obj);
      case "col_range":
        return HN.Sanity.inRange("col", HN.Util.parseRef(from_ref).obj,
                                 HN.Util.parseRef(to_ref).obj);
      default:
        return false;
    };
  };
};

HN.Sanity.inRange = function(type, from_ref, to_ref) {
  var brow = true;
  var bcol = true;
  if (((type === "range") || (type === "row"))
    && ((to_ref.y < from_ref.y1)
    || (to_ref.y > from_ref.y2))) {
    brow = false;
  } else if
    (((type === "range") || (type === "row"))
    && ((to_ref.x < from_ref.x1)
    || (to_ref.x > from_ref.x2))) {
    bcol = false;
  };
  if ((brow === false) || (bcol === false)) {
    return false;
  } else {
    return true;
  };
};

HN.Sanity.parse = function(string) {
  if (string) {
    var ret = HN.Util.parseRef(string);
    return {path: ret.path,
            type: ret.type,
            ref:  HN.Util.refToStr({type: ret.type, obj: ret.obj})};
  } else {
    return {path: null,
            type: null,
            ref:  null};
  }
};

HN.Sanity.in_form = function(bool) {
  if (bool) {
    return "in_form";
  } else {
    return "not_in_form";
  }
};

