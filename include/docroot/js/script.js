var formulae = new Array();
var parents  = new Array();
var children = new Array();
var css_attrs = ["font-style","font-weight","text-align","text-decoration","color"];
        
var handle_attr = function(refxml)
{
    var el = $(refxml);
       
    var ref   = el.attr("ref");
    var name  = ((el.children()[0]).nodeName).toLowerCase();
    var value = el.text();

    if( jQuery.inArray(name, css_attrs) != -1 )
        $("#hn").spreadsheet("setStyle",ref,name,value);
            
    else if(name == "formula") formulae[ref] = value;  
    else if(name == "height")  $("#hn").spreadsheet("setHeight",ref,parseInt(value));
    else if(name == "width")   $("#hn").spreadsheet("setWidth",ref,parseInt(value));
    else if(name == "name")    $("#hn").spreadsheet("addName",ref,value);
    else if(name == "value")
    {
        $("#hn").spreadsheet("setValue",ref,value);
    }
    else if(name == "parents" || name == "children")
    {
        var arr = new Array();

        el.find("url").each(function()
        {
            if($(this).attr("type") == "remote")
            {
                arr.push($(this).text());
            }
        });
        
        if(name == "parents")       parents[ref] = arr;
        else if(name == "children") children[ref] = arr;
    }
}
    
$(function()
{
    var range = "a1:z50";
            
	var url = document.location.href;
	var so = new SWFObject("/swf/JsSocket.swf", "mymovie", "470", "200", "8", "#FFFFFF");
            
	so.addParam("allowScriptAccess", "always");
    so.write("flashcontent");
            
	// Import SpreadSheet
	$("#m_import").click( function()
	{
		$("#filemenu ul").hide();
        $("#import").show();
		$("#Filedata").change(function()
		{
			$.ajaxFileUpload
			({
				url:'?import',
				secureuri:false,
				datatype: "json",
				fileElementId:'Filedata'
			})
			$("#import").hide();
		});
		return false;
    });
	
	$.get(url+"?pages&format=xml", function(data)
	{
        var add = function(root,dir,path)
		{
			var list = $("<ul />").appendTo(root);
					
			$(dir).children().each(function()
			{
				var npath = $(this).attr("path");
				npath = (npath == "/") ? "" : npath;
				var parent = $("<li><a href=\""+path+npath+"/\">/"+npath+"</a></li>").appendTo(list);
				if($(this).children().size() > 0)
				{
				    parent.children("a").addClass("x");
					add(parent,this,path+npath+"/");
				}
			});
		}
		
		var tshow = function(el)
		{
		    var x = $(el);
		    var list = x.next("ul");
		    var hide = function()
		    {
		        list.hide();
		        $(this).unbind();
		        x.one('mouseup',function() 
		        {
		            x.one('click',function() { tshow(x); });
		        });
		    };
		
		    list.show();
		    x.one('mouseup',function() 
		    {
		        x.one('click',function() { hide(); });
		    });
		}
		
		$("#menu > ul li h2").one('click',function()
		{
		    tshow(this);
		});
		
		add($("#browse"),$(data),"");

		$("#menu li").css("display","block");
	});
            
	var defaults =
    {
        fullscreen : true,
        range      : range,
        fmargin    : 24,
        cellChange  : function(x,y,val)
        {
            var fullurl = url+$.fn.to_b26(x+1)+y+"?attr";
            
            if(val == "")
            {
                $.post(fullurl,
                   "<delete><formula /><value/></delete>",null,"xml");
            }
            else
            {
                $.post(fullurl,
                   "<create><formula><![CDATA["+val+"]]></formula></create>",null,"xml");
            }
        },
        formatChange  : function(range,val)
        {
            $.post(url+range,
                "<create><format><![CDATA["+val+"]]></format></create>",null,"xml");
        },
        cellSelect : function(x,y,el)
        {
            var f = formulae[($.fn.to_b26(x+1)+y)];
            el.val( (typeof f != "undefined") ? f : "");
        },
        colResize : function(col,width)
        {
            $.post(url+$.fn.to_b26(col)+"?attr",
                "<create><width>"+width+"</width></create>",null,"xml");
        },
        rowResize : function(row,height)
        {
            $.post(url+row+"?attr",
                "<create><height>"+height+"</height></create>",null,"xml");
        },
        cssChange : function(el,attr,val)
        {
            var ref = $.fn.cell_index(el);
            $.post(url+$.fn.to_b26((ref[0]+1))+(ref[1]+1)+"?attr",
                "<create><"+attr+">"+val+"</"+attr+"></create>",null,"xml");
        },
        setName : function(range,name)
        {
            $.post(url+range+"?attr",
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
            
	$.get(url+"?attr", function(data) 
	{
		$(data).find("ref").each(function(y)
		{
		    handle_attr(this);
		});
	});
            
    $.clipboardReady(function()
    {
        $("#context").bind("contextmenu", function(e) 
        { 
            return false; 
        });
        
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
});
