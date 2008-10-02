var formulae = new Array();
var parents  = new Array();
var children = new Array();
var css_attrs = ["font-style","font-weight","text-align","text-decoration","color"];

var loc = document.location;
var url = loc.protocol+"//"+loc.host+loc.pathname;
var auth = false;

var handle_attr = function(refxml)
{
    var el = $(refxml);
    
    var ref   = el.attr("ref");
    var name  = ((el.children()[0]).nodeName).toLowerCase();
    var value = el.text();
    
    if( jQuery.inArray(name, css_attrs) != -1 )
        $("#hn").spreadsheet("setStyle",ref,name,value);
    
    else if(name == "formula") 
    {
        formulae[ref] = value;  
    }
    
    else if(name == "height")  
        $("#hn").spreadsheet("setHeight",ref,parseInt(value));

    else if(name == "width")
    {
        $("#hn").spreadsheet("setWidth",ref,parseInt(value));
    }

    else if(name == "name")    
        $("#hn").spreadsheet("addName",ref,value);

    else if(name == "value")
    {
        $("#hn").spreadsheet("setValue",ref,value);
    }
    else if(name == "parents" || name == "children")
    {
        var arr = new Array();
        
        var find = function()
        {
            if($(this).attr("type") == "remote")
            {
                arr.push($(this).text());
            }
        };

        el.find("url").each(find);
        
        if(name == "parents")       
            parents[ref] = arr;
        
        else if(name == "children") 
            children[ref] = arr;
    }
}
    
$(function()
{
    init();
    
    setTimeout(function()
               {
                   create_spreadsheet();
                   load_data();
                   $(".spreadsheet > .cover").fadeOut("normal");
                   
               },100);
    
    //create_spreadsheet();
});

var init = function()
{
    var so = new SWFObject("/swf/JsSocket.swf", 
			   "mymovie", 
			   "470", 
			   "200", 
			   "8", 
			   "#FFFFFF");
    so.addParam("allowScriptAccess", "always");
    so.write("flashcontent");
    
    // Import SpreadSheet
    var importfun = function()
    {
        var onchange = function()
	{
	    $.ajaxFileUpload({url:'?import', 
			      secureuri:false, datatype: "json", 
			      fileElementId:'Filedata'});
	    
	    $("#import").hide();
	};
        
        $("#filemenu ul").hide();
        $("#import").show();
        $("#Filedata").change(onchange);
        
        return false;
    }; 
    $("#m_import").click(importfun);

    var dynfun = function()
    {
	if(!$(this).is(":checked"))
	    $("#templates .dynamic").attr("disabled","true");
	else
	    $("#templates .dynamic").removeAttr("disabled");
    };
    $("#templates input[type=checkbox]").change(dynfun);

    var createformfun = function()
    {
        setup_form($("#create_form_input").val()+"/");
        $("#form_dialog").hide();
        $("#builder").show();
    };
    $("#create_form").click(createformfun);

    var tplfun = function()
    {
	$("#templates").show();
	$("#menu ul ul").hide();
    };
    $("#create_tpl").click(tplfun);

    var formfun = function()
    {
	$("#form_dialog").show();
	$("#menu ul ul").hide();
    };
    $("#form_menu").click(formfun);
    
    var savefun = function()
    {
	var poststr = "<template><name>"+$("#tplname").val()+"</name>"
	    + "<url>" + $("#tplurl").val() + "</url><gui>"
	    + $("#tplgui").val() + "</gui><formurl>"+$("#formurl").val()
            + "</formurl></template>";
	$.post(document.location.pathname,poststr,function(){
	    document.location.href="/@"+$("#tplname").val()+"/";
	},"xml");
    };
    $("#templates input.submit").click(savefun);

    var setguifun = function()
    {
	$("#guidialog").show();
	$("#menu ul ul").hide();
    };
    $("#set_gui").click(setguifun);
    

    var setguisavefun = function()
    {
	var poststr = "<create><gui>"+$("#dogui").val()+"</gui></create>";
	$.post(document.location.pathname,poststr,function(){
	    alert("New GUI Set");
	},"xml");
    };
    $("#guidialog input.submit").click(setguisavefun);


    var auth = readCookie("auth");
    if(auth != null)
    {   
	// TODO : Check auth here
	var logout = function()
	{
	    eraseCookie("auth");
	    document.location.reload(true);
	};
	var user = auth.split(":");
	$("#loginmsg").show().find("#user").text(user[0]);
	$("#logoutbutton").show().children("a").click(logout);
    }
    else
    {
	var loginshow = function()
	{
	    $("#loginform").toggle();
	};
	$("#loginbutton").show().children("a").click(loginshow);
	
	var login = function()
	{
	    var email = $("#email").val();
	    var pass = $("#pass").val();
	    
	    if(pass!="" && email!="")
	    {
		var xml = "<login>"
		    +"<email>"+ $("#email").val() +"</email>"
		    +"<password>"+ $("#pass").val() +"</password>"
		    +"</login>";
		
		var callback = function(data)
		{
		    if($(data).find("unauthorised").length == 1)
		    {
			$("#feedback").text("Invalid Details");
		    }
		    else 
		    {
			var token = $(data).find("token").text();
			createCookie("auth",token,30);
			window.location.reload( true );
		    }
		}
		$.post("/",xml,callback,xml);
	    }
	    else
	    {
		$("#feedback").text("Enter full details");
	    }
	    return false;
	};
	$("#login").submit(login);
    }
};

var create_spreadsheet = function(user)
{
    var range = "a1:z50";
    
    var defaults = {
        fullscreen : true,
        range      : range,
        fmargin    : 93,
        cellChange  : function(x,y,val)
        {
            var fullurl = document.location.pathname
		+ $.fn.to_b26(x+1)+y+"?attr";
            
            if(val == "")
            {
                $.post(fullurl,
                       "<delete><formula/><rawvalue/><value/></delete>",null,"xml");
            }
            else
            {
                $.post(fullurl,
                       "<create><formula><![CDATA["+val+"]]></formula></create>",null,"xml");
            }
        },
        formatChange  : function(range,val)
        {
            $.post(document.location.pathname+range,
                   "<create><format><![CDATA["+val+"]]></format></create>",null,"xml");
        },
        cellSelect : function(x,y,el)
        {
            var x = $.fn.to_b26(x+1).toUpperCase();
            
            var f = formulae[(x+y)];
            el.val( (typeof f != "undefined") ? f : "");
        },
        colResize : function(col,width)
        {
            $.post(document.location.pathname+$.fn.to_b26(col)+"?attr",
                   "<create><width>"+width+"</width></create>",null,"xml");
        },
        rowResize : function(row,height)
        {
            $.post(document.location.pathname+row+"?attr",
                   "<create><height>"+height+"</height></create>",null,"xml");
        },
        cssChange : function(el,attr,val)
        {
            var ref = $.fn.cell_index(el);
            $.post(document.location.pathname
		   +$.fn.to_b26((ref[0]+1))+(ref[1]+1)+"?attr",
                   "<create><"+attr+">"+val+"</"+attr+"></create>",null,"xml");
        },
        setName : function(range,name)
        {
            $.post(document.location.pathname+range+"?attr",
                   "<create><name>"+name+"</name></create>",null,"xml");
        }
    };
    
    $("#hn").spreadsheet(defaults);
    
    $.get("/funs.xml", function(data)
	  {
	    $(data).find("functions > category").each(function()
	    {
	        var category = $(this).attr("name");
	        $(this).children("function").each(function()
	        {
                var label = $(this).attr("label");
                var data  = $(this).attr("data");
                
	             $("#hn").spreadsheet("addFunction",
	                category,label,data);
	        }); 
	    });
	});
           
    $.get("/?templates", function(data)
	  {
	    $(data).find("templates > template").each(function()
	    {
		var name = $(this).children("name").text();
		var path = $(this).children("path").text();
		var li = $("<li><a class=\"x\">"+name+"<ul>"
		    + "<li><a href=\""+path+"?new="+name+"\">Create New</a></li>"
		    + "<li><a href=\""+name+"/\">Edit Template</a></li>"
		    + "</ul></a></li>");
		$("#tpl").append(li);
	    });
	});
           

 
    $.clipboardReady(function()
    {
	var no_context = function(e) { return false; };
        $("#context").bind("contextmenu",no_context);
        
        $('#hn .data').bind("contextmenu", function(e)
        {
            if($(e.target).is("div.cell"))
            {
		var ref = $.fn.cell_index(e.target);
		var refstr = $.fn.to_b26(ref[0]+1)+(ref[1]+1);
                $("#texttocopy").text("=hn(\""+url+refstr+"?hypernumber\")");
                
                $("#contextlinks").empty();
 
                var get_base_url = function(url) 
                {
                    return url.substring(0,url.lastIndexOf("/"));
                };
                
                if(typeof parents[refstr] != "undefined")
                {
                    var par = $("<div id='parents'><strong>Parents"
                        +"</strong><div></div></div>").appendTo($("#contextlinks"));

                    $.each(parents[refstr],function()
                    {
                        par.find("div").append($("<a href='"
                            +get_base_url(this)+"'>"+this+"</a><br/>"));
                    }); 
                }
                
                if(typeof children[refstr] != "undefined")
                {
                    var par = $("<div id='children'><strong>Children"
                        +"</strong><div></div></div>").appendTo($("#contextlinks"));
                                  
                    $.each(children[refstr],function()
                    {
                        par.find("div").append($("<a href='"
                            +get_base_url(this)+"'>"+this+"</a><br/>"));
                    }); 
                }
                        
                var menu = $("#context");
                menu.css("top",e.clientY+"px").css("left",e.clientX);
                menu.show();   
                
                $("body").one('click', function()
                {
                    $("#context").hide();    
                });
            }
            
            return false;
        });
        
        $("#copyhypernumber").click(function()
        {
            $.clipboard($("#texttocopy").text());    
        });
        
        $("#formatcell").click(function()
        {
            $.clipboard($("#texttocopy").text());    
        });
                             
    },{swfpath: "/swf/jquery.clipboard.swf"}); 
}

load_data = function()
{
    var get_pages = function(data)
    {	 
        var add = function(root,dir,path)
	{
	    var list = $("<ul />").appendTo(root);

	    var fun = function()
	    {
		var npath = $(this).attr("path");
		npath = (npath == "/") ? "" : npath;
		var parent = $("<li><a href=\""+path+npath+"/\">/"
		    + npath+"</a></li>").appendTo(list);
		if($(this).children().size() > 0)
		{
		    add(parent,this,path+npath+"/");
		}
	    };
	    $(dir).children().each(fun);   
	}
	
	add($("#browse"),$(data),"");
        $("#menu").filemenu();
    }
    
    var get_attr = function(data) 
    {
	var fun = function(y) { handle_attr(this); };
	$(data).find("ref").each(fun);
    }    
    
    $.ajax({
	type: "GET",
	url: document.location.pathname+"?pages",
	dataType: "xml",
	success: get_pages
    });
 
    $.ajax({
	type: "GET",
	url: document.location.pathname+"?attr",
	dataType: "xml",
	success: get_attr
    });
}

function createCookie(name,value,days) 
{
    if (days) 
    {
	var date = new Date();
	date.setTime(date.getTime()+(days*24*60*60*1000));
	var expires = "; expires="+date.toGMTString();
    }
    else var expires = "";
    document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name) 
{
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
	var c = ca[i];
	while (c.charAt(0)==' ') c = c.substring(1,c.length);
	if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
}

function eraseCookie(name) 
{
    createCookie(name,"",-1);
}

