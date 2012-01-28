/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 20, white: false */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false, Finder: false, path: false */

/**
 * @class HN.ToolBar
 * provides functionality for the styles toolbar
 */

HN.ToolBar = function (layout) {

    var api       = {},
        editor    = HN.Util.id("editor"),
        formula   = "",
        newath,
        wizcats   = {}, // Function Wizard Categories
        catids    = [], // array of category ids
        newPath;

    //api.loadSite = function () {
    //        api.loadFunctions();
    //        api.loadViews();
    //    };

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
        //
        //$("#pdvisit").bind("click", function () {
        //    hn.finder.chooseCurrent();
        //});

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
    // hn.data object and then push stuff onto groups when we should use
    // the api to do selective operations on stuff
    // caused by me being not so hot at javascript :(

    api.loadViews = function () {
        var path = layout.currentSheet().path(),
        perms = hn.data.readPermissions(path),
        table = HN.Util.makePermsTable(perms, path),
        user  = HN.Util.parseEmail(HN.Util.readCookie("auth")),
        clickPublic, advperms, advusers, p, g, grp, id, dataPerm,
        groupinputchangefn, showbasicgrpsfn, showadvpermsfn,
        showadvusersfn;

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
                            dataPerm = "[data-perm='" + p + "_" + grp + "']";
                            $(dataPerm).attr("checked", "checked");
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
            HN.Callbacks.setView(path, view, everyone,
                                 perms.views[view].groups, null);
        };
        for (p in perms.views) {
            if (perms.views.hasOwnProperty(p)) {
                $("#newpublic" + p).bind("change", clickPublic);
            }
        }

        groupinputchangefn = function () {
            var a = $(this).attr("data-perm").split("_"),
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
        };

        // and for the groups/views stuff
        $(".groupinput").bind("change", groupinputchangefn);

        // We need to set up the showbasicgroups function
        // because we trigger in on setting the view up
        
        showbasicgrpsfn = function () {
            $("#advperms").hide();
            $("#advusers").hide();
            $("#invitecolleague").val("workmate@example.com");
            $("#newcolleaguefeedback").html("&nbsp;");
            layout.grabFocus();
            $("#invitecolleague").focus();
            $("#permstable").show();
        };

        $("#showbasicgroups").bind("click", showbasicgrpsfn);
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

            showadvpermsfn = function () {
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
            };

            $("#showadvperms").bind("click", showadvpermsfn);

            showadvusersfn = function () {
                $("#permstable").hide();
                $("#advperms").hide();
                $("#emailforgroup").val("workmate@example.com");
                $("#newusermsg").val("Dear workmate, please take " +
                                     "a look at this page...");
                $("#newuserfeedback").html("&nbsp;");
                $("#administrator").removeAttr("checked");
                $("#groups option").removeAttr("selected");
                layout.grabFocus();
                $("#emailforgroup").focus();
                $("#advusers").show();
            };

            $("#showadvusers").bind("click", showadvusersfn);
        } else {
            $("#showbasicgroups").hide();
            $("#showadvperms").hide();
            $("#showadvusers").hide();
            $(".miniseparator").hide();
            $("#permstable").show();
        }
    };

    // Convert parsed expression to string

    api.stringify = function(x) {
        var f = x.fn,
        y;
        if (typeof(f) !== "undefined") {
            // TODO check for infix in
	          if (f.type === "infix") {
	              return "(" + x.args.map(api.stringify).join(f.name) + ")";
	          } else {
	              if (x.args) {
		                return f.name.toUpperCase() +
		                    "(" + x.args.map(api.stringify).join(",") + ")";
	              }
	              else {
		                return f.name.toUpperCase() + "()";
	              }
	          }
        } else {
            if (x.error) {
	              return null;
	          } else if (typeof(x.constant) !== 'undefined') {
                switch (typeof(x.constant)) {
		              case 'boolean':
		                return x.constant ? "true" : "false";
		              case 'string':
                    return "\"" + x.constant + "\"";
                default:
		                return x.constant;
	              }
	          } else {
                for (y in x) {
                    if (x.hasOwnProperty(y)) {
		                    return x[y];
                    }
	              }
	          }
        }
    };

    // Parse an expression for the wizard.

    api.parseExpression = function(exp, func) {
        var json = JSON.stringify({ "expression": exp });
        $.post("/_parse_expression/", json, func, "json");
    };

    // Parse sub-expression. Basically a wrapper that adds initial "="
    // so that the parser accepts it.

    api.parseSubExpression = function(exp, func) {
        if (exp !== "") {
            api.parseExpression("=" + exp, func);
        }
    };

    // Returns a list of values of function wizard arguments.

    // TODO This is called when wizard dialog is first set up, and not
    // when submitted.

    api.getArgValues = function(a, req) {
	      var args = [],
	      n = api.getClass(a),
	      c = 0,
        isValid;
	      $("." + n).each(
            function(idx,el) {
                args.push(el.value);
	              ++c;
            });

	      if (a.type !== "finite") {

	          // Remove optional arguments from the tail of the list, if
	          // they are blank

	          while ((c > 0) && (args[args.length-1] === "")) {
		            args.pop();
	          }
	      }

	      if (req) {
	          if ((c === 0) || (args[0] === "")) {
                isValid = false;
                //alert(a.name + " is required. banjo");
		            //$('input[id^="' + n + '"]:first').focus(); // FIXME
	          } else {
                isValid = true;
            }
	      }
	      return {"args"    : args, 
                "isValid" : isValid};
        };

    api.getFunctionText = function(f, req) {
        var args = [],
        isValid = true,
        i, vs, coerce, fn, r, c;
        for( i = 0; i < f.args.length; i = i + 1) {
	          if (f.args[i].type === "variable") {
                // push the actual number of arguments
                // ignoring blanks at the end
	              vs = api.getArgValues(f.args[i].attr, req);
	              args.push(vs.args.length, vs.args);
                if (isValid && !vs.isValid) {
                    isValid = false;
                }
	          }
	          else {
	              vs = api.getArgValues(f.args[i], req);
	              if (vs.args.length) {
	                  args.push(vs.args);
                    if (isValid && !vs.isValid) {
                        isValid = false;
                    }
	              }
	          }
        }
        if (f.infix) {
	          return args.join(f.fn);
        } else {
	          coerce = function(v) {
                if (v.match(/\D/) || (parseInt(v, 10) < 1)) {
		                return 1;
	              }
	              return v;
	          };
            
	          fn = f.fn.toUpperCase();
	          if (f.resize) {
	              switch(f.resize) {
		              case "row":
		                r = coerce($("#resizeRows").val());
		                fn = fn + "." + r;
		                break;
		              case "column":
		                c = coerce($("#resizeColumns").val());
		                fn = fn + "." + c;
		                break;
		              case "range":
		                r = coerce($("#resizeRows").val());
		                c = coerce($("#resizeColumns").val());
		                fn = fn + "." + r + "x" + c;
		                break;
	              }
	          }
	          if (isValid) {
                $("#wizdone").attr("disabled", false);
            } else {
	              $("#wizdone").attr("disabled", "disabled");
            }
            return fn + "(" + args.join(",") + ")";
        }
    };

    api.setWizText = function(t) {
        $("#wiztext").val("=" + t);
    };

    api.parseArgument = function(el,f, req) {
	      var g = function(x) {
            var v;
	          if (x !== null) {
		            v = (api.stringify(x.expression));
		            if (v !== null) {
		                el.value = v; // normalise
		                api.setWizText(api.getFunctionText(f, req));
		            } else {
		                // BUG focus not returned to element on error
                    api.setWizText(x.expression.error);
		                el.focus();
		            }
	          }
	      };
	      if (el.value === "") {
	          api.setWizText(api.getFunctionText(f, req));
	      } else {
	          api.parseSubExpression(el.value, g);
	      }
    };

    // Get a prefix for argument id

    api.getClass = function(a) {
        return "wa_" + a.name.replace(/[\W]/, "_"); 
    };
    api.createArgument = function(a, f) {
 
        var n = api.getClass(a),
	      l = $(document.createElement('label'))
            .text(a.name)
            .attr("for", n)
            .attr("class", "l" + n),
	      z = $(document.createElement('input')).attr("class", n),
        e, es, xs, j, cnt, more, desc;
	      
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
		            api.parseArgument(ev.target,f, true);
		        };
	      }(f));

	      e  = $( document.createElement('div') )
            .attr("id", "e" + n)
            .append(l).append(z);
        es = [ e ];

	      switch (a.type) {

        case "variable":

	          z.attr("readonly", "readonly").attr("value", 0);

	          a.attr.variable = z;

	          xs = api.createArgument(a.attr,f);
	          for (j = 0; j < xs.length; j = j + 1) {
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

	          cnt = 1 + $("." + n).length;
	          if (cnt === 1) {
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
	          more = $(document.createElement('img')).attr("id", "m" + n)
                .attr("class", "wizrep")
                .attr("alt", "Add")
                .attr("src","img/add.png")
                .click( function(a, f, d, z) {
			                      return function(ev) {
			                          var xs = api.createArgument(a, f),
                                j;
			                          for (j = 0; j < xs.length; j = j + 1) {
				                            d.after(xs[j]);
			                          }
			                          z.unbind("keydown.infinite");
                                
			                          // Replace "More" button with delete button.
                                
			                          more.unbind("click").empty()
                                    .attr("alt", "Delete")
				                            .attr("src", "img/delete.png").click(
					                              function(a, f, d, z) {
					                                  return function(ev) {
                                                var fl, ff;
						                                    d.remove();
						                                    fl = $('label[id^="l' +
                                                       nc[0] +
                                                       '"]:first').addClass("req");
                                                
						                                    // Update the tooltip to say
                                                // that argument is required
                                                
						                                    ff = $( "#" + fl.attr("for"));
						                                    if (ff.attr("x-title").indexOf(" (Required)") === -1) {
						                                        ff.attr("title", ff.attr("x-title") + " (Required)");
						                                        ff.attr("x-title", ff.attr("title")).tooltip();
						                                    }
                                                
						                                    if (a.variable) {
						                                        a.variable.val($('input[id^="' +
								                                                     nc[0] + '"]').length);
						                                    }
					                                  };
					                              }(a, f, e, z) );
				                        api.adjust_arglist();
		                        };
		                    }(a, f, e, z) );
	          e.append(more);
            
            z.bind("keydown.infinite", function(m) {
		            return function(ev) {
			              ev.stopPropagation();
			              m.click();
		            };
		        }(more));

	          break;

        case "optional":
            break;
	      default:

	      }

	      // Set up tooltips for arguments. We save them to x-title as
	      // well as title so that we can update the tooltip as needed.

	      desc = a.desc;
	      if (l.hasClass("req")) {
	          desc += " (Required)";
	      }
        z.attr("x-title", desc).attr("title", desc).tooltip();

	      return es;
    };

    api.createResizer = function(name,f,def) {

	      var n = "resize" + name,
	      l = $(document.createElement('label')).text(name).attr("for", n)
            .addClass("req").attr("id", "l" + n),
        z, zd, zi, e;
        
	      // TODO setting this up to use jQuery UI numeric plugin would
	      // be nicer, but it ay require a lot of work to use jQuery-UI

	      if (typeof(def) === "undefined") {
	          def = 1;
	      }

	      z = $( document.createElement('input') ).attr("id", n).attr("size", 5)
            .attr("value", def).attr("type", "number");

	      z.attr("title", "Number of " + name + " (Required)").tooltip();

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
		        if (v.match(/\D/) || (parseInt(v, 10) < 1)) {
	              ev.target.focus(); // TODO buggy
	          } else {
	              api.parseArgument(ev.target,f, true);
	          }
        });

	      z.bind("Decrease", function(ev) {
		        var v = parseInt(ev.target.value, 10);
		        if (v > 1) {
                ev.target.value = v - 1;
	              $(this).change();
	          }
	      });

	      z.bind("Increase", function(ev) {
		        ev.target.value = 1 + parseInt(ev.target.value, 10);
		        $(this).change();
	      });

	      zd = $(document.createElement('img'))
            .attr("class","wizrep")
            .attr("alt", "Decrease")
            .attr("src","/img/delete_bw.png")
            .click(function (ev) { 
                       z.trigger("Decrease"); 
                   });

	      zi = $(document.createElement('img'))
            .attr("class","wizrep")
            .attr("alt", "Increase")
            .attr("src","/img/add_bw.png")
            .click(function (ev) { 
                       z.trigger("Increase"); 
                   });

	      e = $(document.createElement('div'))
            .append(l)
            .append(zd)
            .append(z)
            .append(zi);

        return e;
    };

    //

    api.wizardAddArguments = function(f) {
	      var d = $("#wizfunargs"),
        r, i, xs, j;

	      if (f.resize) {
	          r = $(document.createElement('div')).attr("class", "resizer");

	          if ((f.resize === "row") || (f.resize === "range")) {
		            r.append(api.createResizer("Rows",f, f.resize_rows_default));
	          }
	          if ((f.resize === "column") || (f.resize === "range")) {
		            r.append(api.createResizer("Columns",f, f.resize_cols_default));
	          }
	          d.append(r);
	      }

        for (i = 0; i < f.args.length; i = i + 1) {
	          xs = api.createArgument(f.args[i], f);
	          for (j = 0; j < xs.length; j = j + 1) {
		            d.append(xs[j]);
	          }
	      }
	      api.setWizText(api.getFunctionText(f));
    };

    //

    api.wizardSelectFunction = function(f) {
	      var desc = f.desc,
            link;
        HN.Callbacks.setMark("function " + f.fn + " selected in fn wizard");
        if (f.link) {
            link = f.fn.toUpperCase() + "<br /><a class='hn_documentation' " +
                "href=" + f.link + " target='_blank'>" +
                "(help and examples)</a>";
            $("#wizfunname").html(link);
        } else {
            $("#wizfunname").html(f.fn.toUpperCase());
        }
	      if (f.inexcel) {
	          desc += " <span class='hn_wiz_emphasis'>" +
                "(Identical to Excel)</span>";
	      }
	      if (f.experimental) {
	          desc += " <span class='hn_warn'>This is experimental</span>";
	      }

	      $("#wizfundesc").html(desc);
	      if (typeof(f.warning) !== "undefined") {
	          $("#wizfundesc").append($(document.createElement('span'))
                                    .html(f.warning).attr("class", "warning"));
	      }
        $("#wizfunargs").empty();
        api.wizardAddArguments(f);
        api.setWizText(api.getFunctionText(f));
	      api.adjust_arglist();
    };

    // Populate the arguments in function wizard with values from
    // parsed expressions.

    api.wizardPopulateArgs = function(x,f) {
        var j, i, a, c, klass;

	      if (f.args) {
	          j = 0; // index in f.args[]
            c = 1; // count of infinite/variable args

	          if (typeof(x.expression.args) === "undefined") {
		            x.expression.args = [];
	          }

            for (i = 0; i < x.expression.args.length; i = i + 1) {
		            a  = f.args[j];
		            klass = api.getClass(a) + (c);
                c = c + 1;
		            $("." + klass).val(api.stringify(x.expression.args[i]));

		            switch (a.type) {
		              case "infinite":
		                $("." + klass).trigger("keydown");
		                break;
                  case "finite":
                    j = j + 1;
                    c = 1;
                    break;
                  case "optional":
                    j = j + 1;
                    c = 1;
                    break;
		            default:
                    j = j + 1;
                    c = 1;
		            }
            }
	      }
	      api.setWizText(api.getFunctionText(f));
    };

    // Selected function name in drop-down box for function wizard

    api.wizardPopulateFunctionName = function(n,x) {
	      // Select All Functions

	      $("#wizcatlist option").removeAttr('selected');
	      $("#wizcatlist option:first").attr('selected', 'selected');
	      $("#wizcatlist").trigger("change");

	      $("#wizfunlist option").removeAttr('selected')
	          .each(function(i) {
		            if ($(this).text() === n.toUpperCase()) { // TODO
			              $(this).attr('selected', 'selected');
			              $("#wizfunlist").trigger("click");

			              var f = catids[0][$(this).val()];
			              api.wizardPopulateArgs(x, f);

			              return false;
		            }
		            return true;
		        });

    };

    // Populate the function and arguments of a function wizard with
    // values from parsed expressions.

    api.wizardPopulateFunction = function(x) {
        if (typeof(x.expression) !== "undefined") {
	          if (typeof(x.expression.fn) !== "undefined") {
		            api.wizardPopulateFunctionName(x.expression.fn.name,x);
	          }
	          else if (typeof(x.expression.constant) !== "undefined") {
		            if (typeof(x.expression.constant) === 'boolean') {
		                var n = x.expression.constant ? "true" : "false";
		                api.wizardPopulateFunctionName(n,x);
		            }
	          }
	      }
	      // TODO should wizard handle values, constants, ranges, etc.?
    };

    // Display functions in the wizard by selected category

    api.displayFunctionsByCategory = function(catids) {
	      var fs = catids[$("#wizcatlist option:selected").val()],
        i;
	      
        $("#wizdone").attr("disabled", "disabled");
	      $("#wizfunlist")[0].options.length = 0;
        for (i = 0; i < fs.length; i = i + 1) {
	          $("#wizfunlist").append(new Option(fs[i].fn.toUpperCase(),
                                               fs[i].index));
	          }
    };

    // Function to parse the value of the wizard text field and set
    // the bound cell.

    api.submitWizard = function() {
	      var g = function(x) {
            var v, path, cell, cmd;

            if (x === null) {
                return; // TODO?
            }
	          v = (api.stringify(x.expression));
	          if (v !== null) {
		            path  = layout.currentSheet().path();
                cell  = {
		                "y": parseInt($("#celly").text(), 10),
		                "x": parseInt($("#cellx").text(), 10)
		            };
		            cmd   = { "set" : { "formula" : "=" + v } };
		            HN.Util.postCell(path, cell, cmd);
                $("#funwiz").trigger("CloseDialog");
	          }
	          else {
		            api.setWizText(x.expression.error);
	          }
	      };
	      api.parseExpression($("#wiztext").val(), g);
    };

    api.loadFunctions = function () {
        var html    = "",
            cat     = {},
            cathtml = "",
            funs    = {},
            catdom  = HN.Util.id("catlist"),
            fundom  = HN.Util.id("funlist"),
            wcatdom = HN.Util.id("wizcatlist"),
            i, len, fun, str, x, category, functionName, enter, sel, DefCat;
        
        // don't reload funs
        if (hn.areFunsLoaded) {
            return;
        }

        hn.areFunsLoaded = true;
	      DefCat = "All Functions";
	      wizcats[DefCat] = catids.length;
        catids[wizcats[DefCat]] = [];
	      // TODO is .append(new Option(...)) MSIE compatible?
	      $("#wizcatlist").append(new Option(DefCat, wizcats[DefCat]));

	      // TODO change filename based language

	      $.get("/hypernumbers/fns_en-GB.json", function(d) {
            var fs = JSON.parse(d),
                  changeFun, clickFun, i, f, c;
                  if (fs) {
	                    for (i = 0; i < fs.length; i = i + 1) {
		                      f = fs[i];
		                      if (f.wizardready) {
		                          c = f.category;
		                          if (typeof(wizcats[c]) === "undefined") {
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
                      changeFun = function(e) {
	                        api.displayFunctionsByCategory(catids);
                      };

	                    $("#wizcatlist").bind("change", changeFun);
                      
	                    api.displayFunctionsByCategory(catids);
                      clickFun = function(e) {
	                        // catids[wizcats[DefCat]][ ... ]
                          var f = catids[0][$("#wizfunlist option:selected").val()];
	                        api.wizardSelectFunction(f);
                      };
	                    $("#wizfunlist").bind("click", clickFun);
                      
	                    //$("#wizfunlist option:first").attr('selected','selected');
	                    //$("#wizfunlist").trigger("click");
                      
	                }
              });
        
	      $("#wizdone").click(function(e) {
	                              api.submitWizard();
                            });
        
    };

    //
    // Given format key, set the format for current selection
    //
    api.setFormat = function (value) {
        var formats = {
            "fmt_0"   : "General",
            "fmt_1"   : "#,0",
            "fmt_1a"  : "#,0.0",
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

                    if (typeof(toggle) !== "undefined") {
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

    //
    // Setup the drop down menus (colors / alignment / font etc)
    //

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
                inside, hidden, offset,
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
            // work out the menu width by using the hidden input
            inside = $("#addgroupmenu").html();
            hidden = HN.Util.id("hidden_input");
            hidden.setAttribute("style", "overflow:visible;");
            hidden.innerHTML = inside;
            offset = hidden.offsetWidth;
            $("#addgroupmenu").css({"display" : "block",
                                    "top"     : pos.top + 12,
                                    "left"    : pos.left + (63 - offset - 12)});
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
            HN.DEFAULT_COLOUR,
            localstorage;

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
            "forecolor" : new jscolor.color(HN.Util.id("forecolor"),
                                            "foreground", opts),
            "bgcolor"   : new jscolor.color(HN.Util.id("bgcolor"),
                                            "background", opts)
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

    function initWikiSelect() {

        var onfocusfun, onsubmit, get_status;

        $("#hn_makewikiselectclose").bind("click", function (e) {
            $("#cover").fadeOut("fast");
            layout.resumeSelection();
            $("#hn_makewikiselect").hide();
        });

        get_status = function() {
            // this is a bit shite
            // if it starts with a / or a . then it is dynamic
            // selection (ie it is ../some/path/to/a/ra1:nge3)
            var selection = $("#hn_insertwikiselect").val(),
            addr, vals, obj;
            if (selection === "") {
                return "none";
            } else if ((selection[0] === ".") ||
                       (selection[0] === "/")) {
                return {"dynamic_select": selection};
            } else {
                vals = HN.Util.CVSToArray(selection);
                // if it is a single entry and that is a valid cell or range
                // then it is interpreted as a dynamic select
                // otherwise it is interpreted as a list of values
                if (vals[0].length === 1) {
                    obj = HN.Util.parseRef(vals[0][0]);
                    switch (obj.type) {
                    case "cell":
                        return {"dynamic_select": vals[0][0]};
                    case "range":
                        return {"dynamic_select": vals[0][0]};
                    default:
                        return {"select": vals[0][0]};
                    }               
                } else {
                    return {"select": vals[0]};                    
                }
            }
        };

        onfocusfun = function() {
            var cell = layout.tabUI().currentSelectedCell(),
                ref = HN.Util.coord_to_ref(cell),
                options = layout.tabUI().currentWikiSelection();
            $("#hn_wikiselectcell").text(ref);
            // only set the selected value if one has been selected
        };

        $("#hn_makewikiselect").bind("focus", onfocusfun);

        onsubmit = function () {
            var status = get_status(),
            path, cell, range;
            path  = layout.currentSheet().path();
            cell  = layout.tabUI().currentSelectedCell();
            range = {"x1": cell.x, "x2": cell.x, "y1": cell.y, "y2": cell.y};
            HN.Callbacks.set_input(path, range, status);
            $("#cover").fadeOut("fast");
            layout.resumeSelection();
            $("#hn_makewikiselect").hide();
        };
        $("#hn_wikiselectsubmit").bind("click", onsubmit);
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
	      footer   = funwiz.find(".footer"),
	      funlist  = $("#wizfunlist"),
        wizbtnfn, formfn, formbtnfn, mergecellsfn, inlinefn,
        dropdownfn, userdeffnsfn, userdeflayoutfn;

	      // Function to Resize the Function Wizard's Argument List
	      // TODO adjust width as well

	      api.adjust_arglist = function(dialog, content, bottom, footer, funlist) {
            return function() {
		            var top = Math.round(content.offset().top - 
                                     dialog.offset().top),
                btn, ofs, lsz, hgt;
                // now make the wizfunargs invisible
                // otherwise you get an irritating flash

                content.height(dialog.height() - top - bottom.height());
                content.width(dialog.width() - 154);
		            bottom.width(content.width());

		            btn = content.find(".wizrep:first");
		            ofs = 6 + (btn.length > 0 ? btn.width() : 0) +
                    $.scrollbarWidth();

		            bottom.find("#wiztext").width(bottom.width() - 4 -
                                              footer.find("a.resize:first").width());

		            content.find('label[id^="lwa"]').each(
		                function (width, ofs) {
			                  return function(idx,el) {
			                      var l = $(el),
                            i = $("#" + l.attr("for"));
                            // Note: leave room for "+" and "-" buttons 
                            // and scrollbar
			                      i.width(width - l.width() - ofs);
			                  };
		                }(content.width(), ofs)
                );

		            lsz = parseInt(funlist.css("line-height"), 10);
		            hgt = (dialog.height() - funlist.position().top);
		            funlist.attr("size", Math.floor(hgt / lsz));

		            // Hack for Google Chrome
		            while (funlist.height() >= (hgt-4)) {
		                funlist.attr("size", funlist.attr("size") - 1);
		            }

            };
	      }(funwiz, argslist, bottom, footer, funlist);
        
        wizbtnfn = function (e) {
            var cell, g, formula;
            if (funwiz.is(":visible")) {
		            funwiz.trigger("CloseDialog");
	          } else {
                HN.Callbacks.setMark("opening function wizard box");
		            // Save currently bound cell
		            cell = layout.tabUI().currentSelectedCell();
		            $("#cellx").text(cell.x);
		            $("#celly").text(cell.y);
		            $("#cellid").text(HN.Util.refToStr({"type": "cell", 
                                                    "obj": cell}));
                
		            g = function(x) {
		                api.wizardPopulateFunction(x);
		                HN.Dialog.open_dialog("funwiz", api.adjust_arglist);
		            };
		            formula = layout.tabUI().currentCellFormula();
		            if ((formula[0] === "=") || (formula[0] === "+") ||
			              (formula[0] === "-")) {
		                api.parseExpression(formula, g);
		            } else {
                    $("#wizfunlist option:first").attr('selected','selected');
	                  $("#wizfunlist").trigger("click");
		                HN.Dialog.open_dialog("funwiz", api.adjust_arglist);
		            }
	          }
        };

        $("#wizbtn").bind("mousedown", wizbtnfn);
        
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

        formfn = function (e) {
            // Dale was being too fancy trying to make the input 
            // insertion smart this is a bug 
            //TODO let formula have trailing blanks...
            var params = $(this).attr("data-default").replace(/'/g, "\""),
            formula = "=" + $(this).attr("data-input") + "(" + params + ")";
            
            HN.Callbacks.set_cell(layout.currentSheet().path(),
                                  layout.tabUI().currentSelectedCell(), 
                                  formula);
        };

        $("#forminput, #formtextarea, #formselect, #formradio, #formfixedval")
            .bind("mousedown", formfn);

        formbtnfn = function (e) {
            var button = "=button(\"Submit Form\", " +
                "\"Thanks for submitting my form\")";
                                                              HN.Callbacks.set_cell(layout.currentSheet().path(),
                                  layout.tabUI().currentSelectedCell(),
                                  button);
        };

        $("#formbutton").bind("mousedown", formbtnfn);

        mergecellsfn = function (e) {
            var bounds = layout.tabUI()
                .order_bounds(layout.tabUI().currentSelectedBounds());
            HN.Callbacks.mergeCells(layout.currentSheet().path(),bounds);
        };

        $("#mergecells").bind("mousedown", mergecellsfn);
        
        dropdownfn = function (e) {
            var dropdownref = layout.tabUI().currentInlineDropdown();
            $("#hn_insertwikiselect").val(dropdownref);
            HN.UI.open_dialog(layout, "hn_makewikiselect");
        };
                                                              
        $("#inlinedropdown").bind("mousedown", dropdownfn);

        inlinefn = function (e) {
            var path, cell, range, input, status = "";
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
        };

        $("#inlineinput").bind("mousedown", inlinefn);
        
        userdeffnsfn = function () {
            HN.UserDefFns.configure();
            HN.Dialog.open_dialog("hn_makeuserdeffns");
        };

        $("#hn_userdeffns").bind("click", userdeffnsfn);
    }

    dropMenus();
    initEvents();
    addButtons();
    colorPickers();
    dialogButton("insertimage", "insertimagedialog");
    dialogButton("insertlink", "insertlinkdialog");
    initViews();
    initWikiSelect();

    return api;
};
