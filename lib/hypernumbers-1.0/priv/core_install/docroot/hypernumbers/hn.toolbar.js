/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, Finder: false, path: false */

/**
 * @class HN.ToolBar
 * provides functionality for the styles toolbar
 */
HN.ToolBar = function (layout) {

    var api       = {},
        editor    = HN.Util.id("editor"),
        formula   = "",
        newPath,
        wizcats   = {}, // Function Wizard Categories
        catids    = []; // array of category ids

    api.loadSite = function () {
        api.loadFunctions();
        api.loadPages();
        api.loadViews();
    };

    api.pokeGroup = function (group) {
        hn.groups.push(group);
    };

    api.loadPages = function () {
        if (hn.pages.length === 0) {
            // not loaded yet
            return;
        }
        var opts = {
            "menuPicked"   : function () {
                hn.ctrlPanel.open();
            },
            "activate"     : function () { },
            "deactivate"   : function () { },
            "itemSelected" : function () {
                hn.ctrlPanel.pageSelected(this.asList());
            },
            "itemChosen"   : function () {
                newPath = this.asString();
                window.location.hash = hn.hashUrl.setParam("path", newPath);
                hn.ctrlPanel.close();
            }
        };

        hn.finder = new Finder($("#finder").eq(0), hn.pages, opts);
        hn.finder.select(HN.Util.pathToList(path));
/*
        $("#pdvisit").bind("click", function () {
            hn.finder.chooseCurrent();
        });
*/
    };

    api.userNavigation = function (user) {

        if (user !== "anonymous") {

            HN.Util.id("loggedin").style.display = "block";
            HN.Util.id("home").innerHTML = user;

            HN.Util.addEvent(HN.Util.id("logout"), "mousedown", function (e) {
                HN.Util.logout();
            });

            HN.Util.addEvent(HN.Util.id("lang"), "mousedown", function (e) {
                var el = (e.target.nodeName === "A") ?
                    e.target : e.target.parentNode;
                HN.Callbacks.setLanguage(el.getAttribute("name"));
            });

        } else {
            HN.Util.id("anonymous").style.display = "block";
        }
    };

    // this function should be refactored
    // the problem is that we use hn.data.key to reach into the bowels of the
    // hn.data object and then push stuff onto groups when we should use the api to
    // do selective operations on stuff
    // caused by me being not so hot at javascript :(

    api.loadViews = function () {
        var path = layout.currentSheet().path(),
        perms = hn.data.readPermissions(path),
        table = HN.Util.makePermsTable(perms, path),
        user  = HN.Util.parseEmail(HN.Util.readCookie("auth")),
        clickPublic, advperms, advusers, p, g, grp, id;

        // Fire the bits in
        $("#permstablelayout").html(table);
        // Now the groups menu
        $("#addgroupmenu").html(HN.Util.makeGroupMenu());

        // Set up the permissions
        if (perms.champion) {
            $("#newviewas input[value=" + perms.champion + "]")
                .attr("checked", "checked");
        } else {
            $("#newviewas input[type=radio]").removeAttr("checked");
        }
        // first clear all attributes
        $(".groupinput").removeAttr("checked");
        for (p in perms.views) {
            if (perms.views.hasOwnProperty(p)) {
                perms.views[p].everyone ?
                    $("#newpublic" + p).attr("checked", "checked") :
                    $("#newpublic" + p).removeAttr("checked");
                for (g in perms.views[p].groups) {
                    if (perms.views[p].groups.hasOwnProperty(g)) {
                        grp = perms.views[p].groups[g];
                        if (grp !== "admin") {
                            id = p + "_" + grp;
                            $("#" + id).attr("checked", "checked");
                        }
                    }
                }
            }
        }

        // Now set up the functions and stuff
        // First up the default options
        $("#newviewas input[name=newdefaultpage]").bind("change", function () {

            hn.data.pokeChampion(path, $(this).val());
            HN.Callbacks.setChampion(path, $(this).val(), null);

        });
        // Now define the handling functions for the 'everyone' stuff
        clickPublic = function () {
            var view = $(this).attr("data-value"),
                everyone = $(this).is(":checked");
            hn.data.pokePublic(path, view, everyone);
            HN.Callbacks.setView(path, view, everyone, perms.views[view].groups, null);
        };
        for (p in perms.views) {
            if (perms.views.hasOwnProperty(p)) {
                $("#newpublic" + p).bind("change", clickPublic);
            }
        }
        // and for the groups/views stuff
        $(".groupinput").bind("change", function () {
            var a = $(this).attr("id").split("_"),
            checked = $(this).is(":checked"),
            view = a[0],
            group = a[1],
            newperms = {};
            if (checked) {
                hn.data.pokeGroup(path, view, group);
            } else {
                hn.data.unpokeGroup(path, view, group);
            }
            // need to reread the permission
            newperms = hn.data.readPermissions(path);
            HN.Callbacks.setView(path, view, newperms.views[view].everyone,
                                 newperms.views[view].groups, null);
        });
        // We need to set up the showbasicgroups function
        // because we trigger in on setting the view up
        $("#showbasicgroups").bind("click", function () {
            $("#advperms").hide();
            $("#advusers").hide();
            $("#invitecolleague").val("workmate@example.com");
            $("#newcolleaguefeedback").html("&nbsp;");
            layout.grabFocus();
            $("#invitecolleague").focus();
            $("#permstable").show();
        });
        // Set up the basic button
        // If the user is "anonymous" we need to disable this lot
        // because they need to see it, but they
        // can't do hee-haw

        if (user === "anonymous") {
            $("#newviewas #permstable input").attr("disabled", true);
            $("#addgroup").hide();
            $("#invite").hide();
        }
        // Now (if necessary) make the advanced options panel
        //
        if (hn.is_admin) {
            advperms = HN.Util.makeAdvPerms(perms);
            advusers = HN.Util.makeAdvUsers(perms);
            path     = layout.currentSheet().path();
            perms    = hn.data.readPermissions(path);

            // Set up the options panels
            $("#advpermstable").html(advperms);
            $("#advuserstable").html(advusers);

            // Now set up the options bindings
            $("#showadvperms").bind("click", function () {
                $("#permstable").hide();
                $("#advusers").hide();
                $("#newgroup").val("");
                $("#newgroupfeedback").html("&nbsp;");
                // clean up the defaults
                $("#advperms input").removeAttr("checked");
                if (perms.champion) {
                    $("#advperms input[value=" + perms.champion + "]")
                        .attr("checked", "checked");
                }
                layout.grabFocus();
                $("#newgroup").focus();
                $("#advperms").show();
            });
            $("#showadvusers").bind("click", function () {
                $("#permstable").hide();
                $("#advperms").hide();
                $("#emailforgroup").val("workmate@example.com");
                $("#newusermsg").val("Dear workmate, please take a look at this page...");
                $("#newuserfeedback").html("&nbsp;");
                $("#administrator").removeAttr("checked");
                $("#groups option").removeAttr("selected");
                layout.grabFocus();
                $("#emailforgroup").focus();
                $("#advusers").show();
            });
        } else {
            $("#showbasicgroups").hide();
            $("#showadvperms").hide();
            $("#showadvusers").hide();
            $(".miniseparator").hide();
            $("#permstable").show();
        }
    };


    api.stringify = function(x) { // convert parsed expression to string
      var f = x.fn;
      if (typeof(f) != "undefined") {
          // TODO check for infix in
	  if (f.type == "infix") {
	      return "(" + x.args.map(api.stringify).join(f.name) + ")";
	  } else {
	      if (x.args) {
		  return f.name.toUpperCase()
		      + "(" + x.args.map(api.stringify).join(",") + ")";
	      }
	      else {
		  return f.name.toUpperCase() + "()";
	      }
	  }
      } else {
         if (x.error) {
	     return null;
	 } else if (typeof(x.constant) != 'undefined') {
            switch (typeof(x.constant)) {
		case 'boolean':
		  return x.constant ? "true" : "false";
		  break;
		case 'string':
                  return "\"" + x.constant + "\"";
		  break;
                default:
		  return x.constant;
	    }
	 } else {
             for (var y in x) {
		 return x[y];
	     }
	 }
      }
    };

    api.parseExpression = function(exp, func) {
          var json = JSON.stringify({ "expression": exp });
          $.post("/_parse_expression/", json, func, "json");
    };

    api.parseSubExpression = function(exp, func) {
      if (exp != "") {
          api.parseExpression("=" + exp, func);
      }
    };

    api.getArgValues = function(a) {
	var args = [],
	    n = api.getIdPrefix(a),
	    c = 0;

	$('input[id^="' + n + '"]').each(
           function(idx,el) {
	       args.push(el.value);
	       ++c;
           });

	if (a.type != "finite") {
	    while ((c > 0) && (args[args.length-1] == "")) {
		args.pop();
	    }
	}

	return args;
    };

    api.getFunctionText = function(f) {
      var args = [];
      for(var i=0; i<f.args.length; i++) {
	  if (f.args[i].type == "variable") {
	      // push the actual number of arguents, ignoring blanks at the end
	      var vs = api.getArgValues(f.args[i].attr);
	      args.push(vs.length, vs);
	  }
	  else {
	      var vs = api.getArgValues(f.args[i]);
	      if (vs.length) {
		  args.push(vs);
	      }
	  }

      }
      if (f.infix) {
	  return args.join(f.fn);
      } else {

	  var coerce = function(v) {
              if (v.match(/\D/) || (parseInt(v) < 1)) {
		  return 1;
	      }
	      return v;
	  };

	  var fn = f.fn.toUpperCase();
	  if (f.resize) {
	    switch(f.resize) {
		case "row":
		  var r = coerce($("#resizeRows").val());
		  fn = fn + "." + r;
		  break;
		case "column":
		  var c = coerce($("#resizeColumns").val());
		  fn = fn + "." + c;
		  break;
		case "range":
		  var r = coerce($("#resizeRows").val()),
		      c = coerce($("#resizeColumns").val());
		  fn = fn + "." + r + "x" + c;
		  break;
	    }


	  }
	  return fn + "(" + args.join(",") + ")";
      }
    };

    api.setWizText = function(t) {
      $("#wiztext").val("=" + t);
    };

    api.parseArgument = function(el,f) {
	var g = function(x) {
	    if (x==null) {
	    }
	    else {
		var v = (api.stringify(x.expression));
		if (v != null) {
		    el.value = v; // normalise
		    api.setWizText(api.getFunctionText(f));
		} else {
		    // BUG focus not returned to element on error
		    alert("Invalid argument.");
		    api.setWizText(x.expression.error);
		    el.focus();
		}
	    }
	};
	if (el.value == "") {
	    api.setWizText(api.getFunctionText(f));
	} else {
	    api.parseSubExpression(el.value, g);
	}
    };

    api.getIdPrefix = function(a) {
	return "wa_" + a.name.replace(/[\W]/, "_"); // prefix for id
    };

    api._idCount = 0;                     // counter for unique ids

    api.getNextArgumentId = function(a) { // get id of arg to be added
	var n = api.getIdPrefix(a);
	switch(a.type) {
          case "infinite":
	    return [ n, ++api._idCount ];
            break;
        case "variable":
        case "finite":
        case "optional":
        default:
            return [ n, '1' ]; // there is only one...
	}
    };

    api.createArgument = function(a,f) {
        var nc = api.getNextArgumentId(a),
            n  = nc.join("");

	var l = $( document.createElement('label') ).text(a.name).attr("for", n).attr("id", "l" + n);

	var z = $( document.createElement('input') ).attr("id", n);
	if (typeof(a.default) != "undefined") { z.attr("value", a.default); }

	z.focus(function(ev) {
          $("#wizarghelp").text(a.desc);
        });

	z.blur(function(ev) {
          $("#wizarghelp").text("");
        });

	// We need to stop propagation so keystrokes are not also
	// picked up by the spreadsheet cells.

	z.bind("keydown", function(ev) {
		   ev.stopPropagation();
               });

	z.bind("keyup", function(ev) {
		   ev.stopPropagation();
               });

	// TODO Sometimes a change to final optional arguments is not fired?

        z.bind("change", function(f) {
		   return function(ev) {
		       api.parseArgument(ev.target,f);
		   };
	       }(f));

	var e  = $( document.createElement('div') ).attr("id", e + n).append(l).append(z),
            es = [ e ];

	switch (a.type) {

 	    case "variable":

	      z.attr("readonly", "readonly").attr("value", 0);

	      a.attr.variable = z;

	      var xs = api.createArgument(a.attr,f);
	      for(var j=0;j<xs.length;j++) {
		  es.push(xs[j]);
	      }

	      break;

 	    case "finite":

	      l.addClass("req");
 	      break;

 	    case "infinite":

	      // TODO maintain a separate (hidden) counter of fields,
	      // which is more efficient than using jQuery to count
	      // them.

	      var cnt = 1 + $('input[id^="' + nc[0] + '"]').length;
	      if (cnt == 1) {
	        l.addClass("req");
	      }

	      // if argument is associated with a variable, then update that
	      // with the count of arguments

	      if (a.variable) {
		a.variable.val(cnt);
	      }


	      // Add a new argument as soon as a key is pressed inside
	      // the last. We bind to keydown instead of change
	      // because it allows tab to next field to go to the new
	      // fields instead of the Cancel/Done buttons.

	      var more = $( document.createElement('span') ).attr("id", "m" + n).attr("class", "wizrep").attr("title", "more").text("...")
		  .click( function(a,f,d,z) {
			 return function(ev) {
			     var xs = api.createArgument(a, f);
			     for(var j=0;j<xs.length;j++) {
				 d.after(xs[j]);
			     }
			     z.unbind("keydown.infinite");

			     // Replace "More" button with delete button.

			     more.unbind("click").empty().attr("title", "delete").text("X").click( function(a,f,d,z) {
					     return function(ev) {
						 d.remove();
						 $('label[id^="l' + nc[0] + '"]:first').addClass("req");
						 if (a.variable) {
						     a.variable.val($('input[id^="' + nc[0] + '"]').length);						     
						 }
					     };
					 }(a,f,e,z) );

		       };
		   }(a,f,e,z) );
	      e.append(more);

              z.bind("keydown.infinite", function(m) {
			 return function(ev) {
			     ev.stopPropagation();
			     m.click();
			 };
		     }(more));


	      break;

 	    case "optional":
	    default:

	}

	return es;
    };

    api.createResizer = function(name,f,def) {

	var n = "resize" + name;

	var l = $( document.createElement('label') ).text(name).attr("for", n).addClass("req").attr("id", "l" + n);

	// TODO setting this up to use jQuery UI numeric plugin would
	// be nicer, but it ay require a lot of work to use jQuery-UI

	if (typeof(def) == "undefined") {
	    def = 1;
	}

	var z = $( document.createElement('input') ).attr("id", n).attr("size", 5).attr("value", def).attr("type", "number");

	// We need to stop propagation so keystrokes are not also
	// picked up by the spreadsheet cells.

	z.bind("keydown", function(e) {
		   e.stopPropagation();
               });

	z.bind("keyup", function(e) {
		   e.stopPropagation();
               });

        z.bind("change", function(ev) {
          var v = ev.target.value;
          if (v.match(/\D/) || (parseInt(v) < 1)) {
	      alert("Please enter a number >= 1.");
	      ev.target.focus(); // TODO buggy
	  } else {
	      api.parseArgument(ev.target,f);
	  }
        });

	z.bind("Decrease", function(ev) {
          var v = parseInt(ev.target.value);
          if (v > 1) {
              ev.target.value = v - 1;
	      $(this).change();
	  }
	});

	z.bind("Increase", function(ev) {
          ev.target.value = 1 + parseInt(ev.target.value);
          $(this).change();
	});

	var zd = $( document.createElement('span') ).attr("title", "decrease").text(" -").click(function (ev) { z.trigger("Decrease"); });

	var zi = $( document.createElement('span') ).attr("title", "increase").text(" +").click(function (ev) { z.trigger("Increase"); });

	var e = $( document.createElement('div') ).append(l).append(z).append(zd).append(zi);

        return e;
    };

    api.wizardAddArguments = function(f) {
	var d = $("#wizfunargs");

	if (f.resize) {
	    if ((f.resize == "row") || (f.resize == "range")) {
		d.append(api.createResizer("Rows",f, f.resize_rows_default));
	    }
	    if ((f.resize == "column") || (f.resize == "range")) {
		d.append(api.createResizer("Columns",f, f.resize_cols_default));
	    }
	}

        for(var i=0;i<f.args.length;i++) {
	    var xs = api.createArgument(f.args[i], f);
	    for(var j=0;j<xs.length;j++) {
		d.append(xs[j]);
	    }
	}
	api.setWizText(api.getFunctionText(f));
    };

    //

    api.wizardSelectFunction = function(f) {
	$("#wizfunname").html(f.fn.toUpperCase());

	var desc = f.desc;
	if (f.inexcel) {
	    desc += " Identical to Excel.";
	}
	if (f.experimental) {
	    desc += '<span class="warn">This is experimental.</span>';
	}

	$("#wizfundesc").html(desc);
	if (typeof(f.warning) != "undefined") {
	    $("#wizfundesc").append($( document.createElement('span') ).html(f.warning).attr("class", "warning"));
	}
        $("#wizfunargs").empty();
	api.wizardAddArguments(f);
        api.setWizText(api.getFunctionText(f));
	$("#wizdone").removeAttr("disabled");
	api.adjust_arglist();
    };

    //

    api.wizardPopulateArgs = function(x,f) {
	if (f.args) {
	    var j=0, // index in f.args[]
                c=1; // count of infinite/variable args

	    if (typeof(x.expression.args) == "undefined") {
		x.expression.args = [];
	    }

            for(var i=0;i<x.expression.args.length;i++) {
		var a  = f.args[j],
		    id = api.getIdPrefix(a) + (c++);

		$("#" + id).val(api.stringify(x.expression.args[i]));

		switch (a.type) {
		    case "infinite":
		      $("#" + id).trigger("keydown");
		      break;
                    case "finite":
                    case "optional":
		    default:
		      j++;
		      c=1;
		}


	    }
	}
	api.setWizText(api.getFunctionText(f));
    };

    api.wizardPopulateFunctionName = function(n,x) {
	// Select All Functions

	$("#wizcatlist option").removeAttr('selected');
	$("#wizcatlist option:first").attr('selected', 'selected');
	$("#wizcatlist").trigger("change");

	$("#wizfunlist option").removeAttr('selected').
	    each(function(i) {
		     if ($(this).text() == n.toUpperCase()) { // TODO
			 $(this).attr('selected', 'selected');
			 $("#wizfunlist").trigger("click");

			 var f = catids[0][$(this).val()];
			 api.wizardPopulateArgs(x, f);

			 return false;
		     }
		     return true;
		 });

    };

    api.wizardPopulateFunction = function(x) {
	if (x == null) {
	    alert("Invalid formulae.");
	}
	else if (typeof(x.expression) != "undefined") {

	    if (typeof(x.expression.fn) != "undefined") {
		api.wizardPopulateFunctionName(x.expression.fn.name,x);
	    }
	    else if (typeof(x.expression.constant) != "undefined") {
		switch (typeof(x.expression.constant)) {
		  case 'boolean':
		    var n = x.expression.constant ? "true" : "false";
		    api.wizardPopulateFunctionName(n,x);
		    break;
		}

	    }
	}
	else {
	    // TODO should wizard handle values, constants, ranges, etc.?
	}
    };

    //

    api.displayFunctionsByCategory = function(catids) {
	var fs = catids[$("#wizcatlist option:selected").val()];
	$("#wizdone").attr("disabled", "disabled");
	$("#wizfunlist")[0].options.length = 0;
        for(var i=0;i<fs.length;i++) {
	  $("#wizfunlist").append(new Option(fs[i].fn.toUpperCase(), fs[i].index));
	}
    };


    api.submitWizard = function() {
	var g = function(x) {
            if (x==null) return; // TODO?
	    var v = (api.stringify(x.expression));
	    if (v != null) {
		var path  = layout.currentSheet().path(),
  		    cell  = layout.tabUI().currentSelectedCell(),
		    cmd   = { "set" : { "formula" : "=" + v } };
		HN.Util.postCell(path, cell, cmd);
                $("#funwiz").trigger("CloseDialog");
	    }
	    else {
		alert("Invalid or missing argument.");
		api.setWizText(x.expression.error);
	    }
	};
	api.parseExpression($("#wiztext").val(), g);
    };

    api.loadFunctions = function () {
        if (hn.functions.length === 0) {
            // not loaded yet
            return;
        }
        var html    = "",
            cat     = {},
            cathtml = "",
            funs    = {},
            catdom  = HN.Util.id("catlist"),
            fundom  = HN.Util.id("funlist"),
            wcatdom = HN.Util.id("wizcatlist"),
            i, len, fun, str, x, category, functionName, enter, sel;


	var DefCat = "All Functions";
	wizcats[DefCat] = catids.length;
        catids[wizcats[DefCat]] = [];
	// TODO is .append(new Option(...)) MSIE compatible?
	$("#wizcatlist").append(new Option(DefCat, wizcats[DefCat]));

	// TODO change filename based language

	$.get("/hypernumbers/fns_en-GB.json", function(d) {
          var fs = JSON.parse(d);
          if (fs) {
	    for(var i=0;i<fs.length;i++) {
		var f = fs[i];
		if (f.wizardready) {
		    var c = f.category;
		    if (typeof(wizcats[c]) == "undefined") {
			wizcats[c] = catids.length;
			catids[wizcats[c]] = [];
			// TODO is .append(new Option(...)) MSIE compatible?
			$("#wizcatlist").append(new Option(c, wizcats[c]));
		    }
                    f.index = catids[wizcats[DefCat]].length;
		    catids[wizcats[DefCat]].push(f);
		    catids[wizcats[c]].push(f);
		}
	    }

	   $("#wizcatlist").bind("change", function(e) {
	      api.displayFunctionsByCategory(catids);
           });

	   api.displayFunctionsByCategory(catids);

	   $("#wizfunlist").bind("click", function(e) {
	     // catids[wizcats[DefCat]][ ... ]
             var f = catids[0][$("#wizfunlist option:selected").val()];
	     api.wizardSelectFunction(f);
           });

	   $("#wizfunlist option:first").attr('selected','selected');
	   $("#wizfunlist").trigger("click");

	  }
        });

	$("#wizdone").click(function(e) {
	  api.submitWizard();
        });

        cat["All Functions"] = [];

        for (i = 0, len = hn.functions.length; i < len; i += 1) {

            fun = hn.functions[i];
            functionName = fun[1];
            category = fun[0];

            if  (category !== "category") {
                funs[functionName] = fun;

                if (typeof cat[category] === "undefined") {
                    cat[category] = [];
                }
                cat["All Functions"].push(fun);
                cat[category].push(fun);
            }
        }

        for (x in cat) {
            if (cat.hasOwnProperty(x)) {
                cathtml += "<option value=\"" + x + "\">" + x + "</option>";
            }
        }
        catdom.innerHTML = cathtml;

        HN.Util.addEvent(catdom, "change", function (e) {
            var index    = catdom.selectedIndex,
                category = catdom.childNodes[index].getAttribute("value");
            api.filterCategory(fundom, category, cat[category]);
        });

        enter = function (e) {
            e.preventDefault();
            sel = layout.tabUI();
            //if (!sel.is_editing()) {
            //} else {
                //sel.editor.formula.value += formula+"(";
                //sel.editor.input.value   += formula+"(";
            //}
            sel.editor.calculateWidth();
        };

        HN.Util.addEvent(HN.Util.id("enterformula"), "mousedown", enter);

        api.filterCategory(fundom, "All Functions", cat["All Functions"]);
        api.selectFun(cat["All Functions"][0]);
        HN.Util.addEvent(fundom, "mousedown", function (e) {
            e.preventDefault();
            if (e.target.nodeName === "A") {
                api.selectFun(funs[e.target.getAttribute("name")]);
            }
        });
    };

    api.selectFun = function (fun) {
        formula = fun[1];
        HN.Util.id("funname").innerHTML = fun[1];
        HN.Util.id("fundesc").innerHTML = fun[2];

        var funlist = HN.Util.id("funlist").childNodes[0],
            len = funlist.childNodes.length, a, i;
        for (i = 0; i < len; i += 1) {
            a = funlist.childNodes[i].childNodes[0];
            a.style.backgroundColor = (a.getAttribute("name") === fun[1]) ?
                "#FFF" : "";
        }
    };

    api.filterCategory = function (fundom, name, funs) {
        var html = "<ul>", x;
        for (x in funs) {
            if (funs.hasOwnProperty(x)) {
                html += "<li><a name=\"" + funs[x][1] + "\">" +
                    funs[x][1] + "</a></li>";
            }
        }
        fundom.innerHTML = html + "</ul>";
    };

    /**
     * Given format key, set the format for current selection
     */
    api.setFormat = function (value) {
        var formats = {
            "fmt_0"   : "General",
            "fmt_1"   : "#,0",
            "fmt_1"   : "#,00",
            "fmt_2"   : "#,0.00",
            "fmt_2a"  : "#,0.000",
            "fmt_3"   : "###,0;[Red](#,##0,0)",
            "fmt_4"   : "###,0;(#,##0,0)",
            "fmt_4a"  : "###,0.0;(#,##0,0.0)",
            "fmt_5"   : "###,0.00;(#,##0,0.00)",
            "fmt_5a"  : "###,0.000;(#,##0,0.000)",
            "fmt_6"   : "\"$\"###,0",
            "fmt_7"   : "\"$\"###,0.00",
            "fmt_8"   : "\"$\"###,0;[Red]\"$\"#,##0,0",
            "fmt_9"   : "0%",
            "fmt_9a"  : "0.0%",
            "fmt_10"  : "0.00%",
            "fmt_10a" : "0.000%",
            "fmt_11"  : "d/m/yyyy",
            "fmt_11a" : "dd-mmm-yy",
            "fmt_11b" : "d-mmmm-yyyy",
            "fmt_12"  : "hh:mm:ss",
            "fmt_13"  : "d/m/yyyy hh:mm:ss"
        },
            sheet  = layout.currentSheet(),
            tab    = layout.tabUI(),
            format = formats[value];

        HN.Callbacks.format(layout.currentSheet().path(),
                            tab.currentSelectedBounds(), format);
    };

    function enableSiteMenu() {

        $("#sitemenulist a").bind("mousedown", function (e) {
            e.preventDefault();
            hn.ctrlPanel.selectTab($(e.currentTarget).attr("data-tab"));
            hn.ctrlPanel.open();
        });
    }

    function enablePaste() {
        if (HN.Util.localStorage() && localStorage.copied) {
            $(".paste").removeClass("disabled");
        }
    }

    function toggleButton(id) {

        HN.Util.addEvent(HN.Util.id(id), "mousedown", function (e) {
            e.preventDefault();
            var cmd, param, toggle, style, value, sheet, cell, css, def;
            if (e.target.nodeName === "A") {
                if (layout.tabUI().is_editing()) {
                    cmd   = e.target.getAttribute("data-command");
                    param = e.target.getAttribute("data-param") || "";
                    document.execCommand(cmd, false, param);
                } else {
                    toggle = e.target.getAttribute("data-toggle");
                    style = e.target.getAttribute("data-style");
                    value = e.target.getAttribute("data-value") ||
                        e.target.getAttribute("data-param");
                    sheet = layout.currentSheet();

                    if (typeof(toggle) != "undefined") {
                        cell = layout.tabUI().currentSelectedCell();
                        css  = sheet.lookupCSS(cell.y, cell.x);
                        def  = style + ":" + value;
                        if (css && css.match(def)) {
                            value = toggle;
                        }
                    }

                    HN.Callbacks.style(sheet.path(),
                                       layout.tabUI().currentSelectedBounds(),
                                       style, value);
                }
            }
        });
    }

    function dialogButton(button, dialog) {

        $("#" + dialog + " .cancel").bind("mousedown", function () {
            HN.UI.close_dialog(layout, dialog);
            layout.tabUI().editor.restoreSelection();
        });

        $("#" + button).bind("mousedown", function () {

            layout.tabUI().editor.saveSelection();
            HN.UI.open_dialog(layout, dialog);

            if (dialog === "insertlinkdialog") {
                var text = layout.tabUI().is_editing() ?
                    layout.tabUI().editor.selectionText() :
                    layout.tabUI().currentCellValue();
                $("#insertlinktext").val(text);
            }
            setTimeout(function () {
                $("#" + dialog).find("input")[0].focus();
            }, 0);
        });
    }

    /**
     * Setup the drop down menus (colors / alignment / font etc)
     */
    function initViews() {

        $("#addcolleaguebutton").bind("click", function () {
            var path  = layout.currentSheet().path(),
                perms  = hn.data.readPermissions(path),
                email  = $("#invitecolleague").val(),
                bits   = [],
                groups = [],
                msg    = "Please take a look at this page";
            if (email === "") {
                return;
            } else if (email === "workmate@example.com") {
                $("#newcolleaguefeedback").html("please enter a real email");
                return;
            }
            bits = email.split("@");
            groups[0] = email;
            $("#newcolleaguefeedback").html("user " + bits[0] + " invited");
            $("#invitecolleague").val("");
            // the group is the e-mail address
            HN.Callbacks.inviteUser(path, email, groups, perms.champion, msg);
        });
        // setting up the menu click
        $("#addgroup").bind("click", function (data) {
            var path  = layout.currentSheet().path(),
                perms = hn.data.readPermissions(path),
                pos = $(data.currentTarget).offset(),
                hide = function (e) {
                    if (HN.Util.is_inside(e.target, $("#addgroupmenu")[0])) {
                        var Groups   = [],
                            group    = $(e.target).text(),
                            everyone = false,
                            newperms = {},
                            view = perms.champion;
                        hn.data.pokePublic(path, view, everyone);
                        hn.data.pokeGroup(path, view, group);
                        // reread the permissons
                        newperms = hn.data.readPermissions(path);
                        HN.Callbacks.setView(path, view, everyone,
                                             newperms.views[view].groups, null);
                    }
                    HN.Util.removeEvent(document, "mousedown", hide);
                    $("#addgroupmenu").hide();
                };
            $("#addgroupmenu").css({"display" : "block",
                                    "top"     : pos.top + 12,
                                    "left"    : pos.left - 5});
            HN.Util.addEvent(document, "mousedown", hide);
        });
        // now bind the buttons
        $("#addnewgroupbutton").bind("click", function () {
            var path  = layout.currentSheet().path(),
                perms = hn.data.readPermissions(path),
                divarray = [],
                views    = [],
                name, i;
            name = $("#newgroup").val();
            if (name === "") {
                return;
            }
            divarray = $("#advperms :checked");
            for (i = 0; i < divarray.length; i = i + 1) {
                views.push($(divarray[i]).val());
            }
            $("#newgroupfeedback").html("group " + name + " added");
            $("#newgroup").val("");
            HN.Callbacks.createGroups(path, views, name);
        });
        $("#addnewuserbutton").bind("click", function  () {
            var path  = layout.currentSheet().path(),
                perms = hn.data.readPermissions(path),
                email   = "",
                groups  = [],
                gparray = [],
                msg     = "",
                admin   = false,
                i;
            email = $("#emailforgroup").val();
            if (email === "") {
                return;
            } else if (email === "workmate@example.com") {
                $("#newuserfeedback").html("please enter a real email");
                return;
            }
            gparray = $("#groups :selected");
            for (i = 0; i < gparray.length; i = i + 1) {
                groups.push($(gparray[i]).val());
            }
            if ($("#administrator").is(":checked")) {
                groups.push("admin");
            }
            $("#newuserfeedback").html("user " + email + " added");
            msg = $("#newusermsg").val();
            HN.Callbacks.createUser(path, email, groups, msg);
        });
    }

    function dropMenus() {
        var i, el = $(".expand"),
            len   = el.length,
            click = function (e) {
                e.preventDefault();
                var parent = e.currentTarget,
                    menu   = $("#" + parent.id + " .expandinner")[0],
                    name = $("#" + parent.id + " .expandinner").attr("id"),
                    hide;

                HN.Util.removeEvent(parent, "mousedown", click);

                hide = function (e) {

                    if (e.target === menu || name === "newviewas" &&
                        HN.Util.is_inside(e.target, menu)) {
                        return;
                    }
                    if (name !== "sitemenulist") {
                        layout.resumeSelection();
                    }
                    HN.Util.addEvent(parent, "mousedown", click);
                    HN.Util.removeEvent(document, "mousedown", hide);
                    $(parent).removeClass("active");
                    menu.style.display = "none";
                };

                // need to initialise the newviewas menu (get focus etc)
                if (name === "newviewas") {
                    $("#showbasicgroups").trigger("click", []);
                }

                $(parent).addClass("active");
                menu.style.display = "block";
                window.setTimeout(function () {
                    HN.Util.addEvent(document, "mousedown", hide);
                }, 0);
            };

        for (i = 0; i < len; i += 1) {
            HN.Util.addEvent(el[i], "mousedown", click);
        }
    }

    function addButtons() {

        var i, buttonIds = ["bold", "italic", "strike", "fontsizelist",
                            "ffamily", "aligntext"];

        for (i = 0; i < buttonIds.length; i += 1) {
            toggleButton(buttonIds[i]);
        }
    }

    function cssValue(css, value) {

        var i, tmp, styles = css && css.split(";") || [];

        for (i = 0; i < styles.length; i += 1) {
            tmp = styles[i].split(":");
            if (tmp[0] === value) {
                return tmp[1];
            }
        }
        return false;
    }

    function savePickedColour(color, background, css) {

        var colVal = cssValue(css, "color"),
            bgVal = cssValue(css, "background-color"),
            obj    = {},
            recent = HN.Util.localStorage() &&
            localStorage.recentColours &&
            JSON.parse(localStorage.recentColours) ||
            HN.DEFAULT_COLOUR;

        obj.color = color.substr(1);
        obj["background-color"] = background.substr(1); // need to cos of the name

        recent.combos.unshift(obj);
        recent.combos.pop();

        if (HN.Util.localStorage()) {
            localStorage.recentColours = JSON.stringify(recent);
        }
    }

    function colorPickers() {

        var opts, colors, hide, actualHide, click, el, i;

        opts = {
            "pickerOnfocus"   : true,
            "pickerPosition"  : "bottom",
            "pickerZIndex"    : 999999999,
            "pickerFace"      : 5,
            "pickerFaceColor" : "white",
            "pickerBorder"    : 1,
            "hash"            : true
        };

        colors = {
            "forecolor" : new jscolor.color(HN.Util.id("forecolor"), "foreground", opts),
            "bgcolor"   : new jscolor.color(HN.Util.id("bgcolor"), "background", opts)
        };

        $(".colormenu form").bind("submit", function (e) {

            e.preventDefault();

            var wrapper = $(this).parent().parent(),
                input   = $(this).find("input"),
                jscolor = colors[input.attr("id")],
                col     = input[0].value,
                style   = $(this).attr("data-style"),
                sheet   = layout.currentSheet(),
                cell    = layout.tabUI().currentSelectedCell(),
                css     = sheet.lookupCSS(cell.y, cell.x),
                color   = "",
                backgroundcolor = "",
                rgb = [];
            actualHide(wrapper[0]);
            if (col.length === 7) {
                rgb[0] = parseInt(col.substr(1, 2), 16) / 255;
                rgb[1] = parseInt(col.substr(3, 2), 16) / 255;
                rgb[2] = parseInt(col.substr(5, 2), 16) / 255;
            } else if (col.length === 4) {
                rgb[0] = parseInt(col.charAt(1) + col.charAt(1), 16) / 255;
				        rgb[1] = parseInt(col.charAt(2) + col.charAt(2), 16) / 255;
				        rgb[2] = parseInt(col.charAt(3) + col.charAt(3), 16) / 255;
            }
            switch (style) {
            case "color":
                color = input[0].value;
                backgroundcolor = HN.Util.invertColor(rgb[0], rgb[1], rgb[2]);
                savePickedColour(color, backgroundcolor, css);
                break;
            case "background-color" :
                backgroundcolor = input[0].value;
                color = HN.Util.invertColor(rgb[0], rgb[1], rgb[2]);
                savePickedColour(color, backgroundcolor, css);
                break;
            }

            if (!layout.tabUI().is_editing()) {

                // var cmd   = e.target.getAttribute("data-command");
                // var param = e.target.getAttribute("data-param") || "";
                // document.execCommand(cmd, false, param);
                // } else {
                HN.Callbacks.colours(sheet.path(),
                                     layout.tabUI().currentSelectedBounds(),
                                     color, backgroundcolor);
            }
        });

        actualHide = function (menu) {
            colors.forecolor.hidePicker();
            colors.bgcolor.hidePicker();
            $(menu).removeClass("active");
            $(menu).find(".hiddenmenu").hide();
            HN.Util.addEvent(menu, "mousedown", click);
            $(document).unbind("mousedown.colorpicker");
            layout.resumeSelection();
            HN.UI.createComboList("#combocolorsbg", HN.THEMES);
        };

        click = function (e) {

            var current = e.currentTarget,
                menu    = $(current).find(".hiddenmenu");

            e.preventDefault();
            HN.Util.removeEvent(current, "mousedown", click);

            hide = function (e) {

                if (HN.Util.is_inside(e.target, menu[0]) ||
                    HN.Util.is_inside(e.target, $("#jscolor")[0])) {

                    if (e.target.nodeName === "A") {

                        var path   = layout.currentSheet().path(),
                            bounds = layout.tabUI().currentSelectedBounds(),
                            obj    = e.target;

                        HN.Callbacks.colours(path, bounds,
                                             "#" + obj.getAttribute("data-textcolor"),
                                             "#" + obj.getAttribute("data-bgcolor"));
                    } else {
                        return;
                    }
                }

                actualHide(current);
            };

            layout.grabFocus();
            $(current).addClass("active");
            menu.show();

            window.setTimeout(function () {
                $(document).bind("mousedown.colorpicker", hide);
            }, 0);

        };

        el = $(".colormenu");
        for (i = 0; i < el.length; i += 1) {
            HN.Util.addEvent(el[i], "mousedown", click);
        }
    }

    function initEvents() {

        enableSiteMenu();

        // Enable paste menu if something has been copied in other window
        HN.Util.addEvent(window, "focus", function (e) {
            enablePaste();
        });
        enablePaste();

        $("#imageform").bind("submit", function (e) {

            e.preventDefault();

            var imgsrc = $("#insertimageinput").val();

            if (!layout.tabUI().is_editing()) {
                layout.tabUI().startEditing("");
            } else {
                layout.tabUI().editor.restoreSelection();
                layout.tabUI().editor.deleteSelection();
            }

            layout.tabUI().editor.execCommand("insertimage", false, imgsrc);

            HN.UI.close_dialog(layout, "insertimagedialog", function () {
                $("#insertimageinput").val("http://");
            });
        });

        $("#linkform").bind("submit", function (e) {

            e.preventDefault();

            var linkhref = $("#insertlinkinput").val(),
                linktext = $("#insertlinktext").val(),
                html     = "<a href='" + linkhref + "'>" + linktext + "</a>";

            if (!layout.tabUI().is_editing()) {
                layout.tabUI().startEditing("");
            } else {
                layout.tabUI().editor.restoreSelection();
                layout.tabUI().editor.deleteSelection();
            }
            layout.tabUI().editor.execCommand("inserthtml", false, html);

            HN.UI.close_dialog(layout, "insertlinkdialog", function () {
                $("#insertlinkinput").val("http://");
                $("#insertlinktext").val("");
            });
        });

        // Formats menu
        $("#formats").bind("mousedown", function (e) {
            if (e.target.nodeName === "A") {
                api.setFormat(e.target.getAttribute("id"));
            }
        });

        // Functions Dialog
        $("#functionsbtn").bind("mousedown", function (e) {
            $("#functions").show();
        });
        $("#funsclose").bind("mousedown", function (e) {
            $("#functions").hide();
        });

	// Function Wizard

	var funwiz   = $("#funwiz"),
            argslist = $("#wizfunargs"),
            bottom   = $("#wizstatus"),
	    footer   = funwiz.find(".footer");

	// Function to Resize the Function Wizard's Argument List
	// TODO adjust width as well

	api.adjust_arglist = function(dialog, content, bottom, footer) {
            return function() {
		var top = Math.round(content.offset().top - dialog.offset().top),
                    bot = bottom.height() + footer.height();
		content.height(dialog.height() - top - bot);
		content.css({ "height": content.height() + "px"});
                bottom.css({ "bottom": (footer.height()) + "px" });
            };
	}(funwiz, argslist, bottom, footer );

        $("#wizbtn").bind("mousedown", function (e) {
            if (funwiz.is(":visible")) {
		funwiz.trigger("CloseDialog");
	    } else {
		var g = function(x) {
		    api.wizardPopulateFunction(x);
		    open_dialog("funwiz", api.adjust_arglist);
		};
		var formula = layout.tabUI().currentCellFormula();
		if ((formula[0] == "=") || (formula[0] == "+") || (formula[0] == "-")) {
		    api.parseExpression(formula, g);
		} else {
		    open_dialog("funwiz", api.adjust_arglist);
		}
	    }
        });

        $("#wizcanc").bind("mousedown", function (e) {
	    funwiz.trigger("CloseDialog");
        });

        // bind keypresses to stop propagation to spreadsheet

	// TODO on change, parse and replace with normalised text in
	// the same way that arguments are.

        $("#wiztext").bind("keydown", function(e) {
          e.stopPropagation();
        });

        $("#wiztext").bind("keyup", function(e) {
          e.stopPropagation();
        });

        $("#forminput, #formtextarea, #formselect, #formradio")
            .bind("mousedown", function (e) {
                // Dale was being too fancy trying to make the input insertion smart
                // this is a bug - TODO let formula have trailing blanks...
                //var regex = /^=(input|textarea|radio|select)\(([\w\W]+)\)$/;
                //    current = layout.tabUI().is_editing()
                //      ? layout.tabUI().editor.selectionText()
                //      : layout.tabUI().currentCellFormula(),
                //    match = current.match(regex);

                //if (match) {
                //    params = match[2];
                //} else {
                //    params = $(this).attr("data-default")
                //        .replace(/'/g, "\"");
                //}
                var params = $(this).attr("data-default").replace(/'/g, "\""),
                    formula = "=" + $(this).attr("data-input") +
                    "(" + params + ")";

                HN.Callbacks.set_cell(layout.currentSheet().path(),
                                      layout.tabUI().currentSelectedCell(),
                                      formula);
            });

        $("#formbutton").bind("mousedown", function (e) {

            var button = "=button(\"Submit Form\", " +
                "\"Thanks for submitting my form\")";

            HN.Callbacks.set_cell(layout.currentSheet().path(),
                                  layout.tabUI().currentSelectedCell(),
                                  button);
        });

        $("#mergecells").bind("mousedown", function (e) {
            var bounds = layout.tabUI().order_bounds(
                layout.tabUI().currentSelectedBounds());
            HN.Callbacks.mergeCells(layout.currentSheet().path(),
                                    bounds);
        });

        $("#inlineinput").bind("mousedown", function (e) {
            var path, cell, range, input,
                status = "";
            path  = layout.currentSheet().path();
            cell  = layout.tabUI().currentSelectedCell();
            range = layout.tabUI().currentSelectedBounds();
            input = layout.currentSheet().cell(cell.y, cell.x).input;
            status = "";
            if (input === "inline") {
                status = "none";
            } else {
                status = "inline";
            }
            HN.Callbacks.set_input(path, range, status);
        });
    }

    dropMenus();
    initEvents();
    addButtons();
    colorPickers();
    dialogButton("insertimage", "insertimagedialog");
    dialogButton("insertlink", "insertlinkdialog");
    initViews();

    return api;
};
