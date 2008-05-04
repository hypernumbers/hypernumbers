var formulae = new Array();
var css_attrs = ["font-style","font-weight","text-align","text-decoration"];
        
var handle_attr = function(refxml)
{
    var el = $(refxml);
       
    var ref   = el.attr("ref");
    var name  = ((el.children()[0]).nodeName).toLowerCase();
    var value = el.text();
    
    console.log(ref + ":" + name + ":" + value);

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
		$("#filemenu ul").toggle();
		var x = $("#import").modal();
		$("#Filedata").change(function()
		{
			$.ajaxFileUpload
			({
				url:'?import',
				secureuri:false,
				datatype: "json",
				fileElementId:'Filedata'
			})
			x.close();
		});
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
					add(parent,this,path+npath+"/");
			});
		}
		
		add($("#browse"),$(data),"");
		$.fn.clickMenu.setDefaults({arrowSrc:"/img/arrow.png"});
		$("#menu").clickMenu();
		$("#menu li").css("display","block");
	});
            
	var defaults =
    {
        fullscreen : true,
        range      : range,
        fmargin    : 24,
        cellChange  : function(x,y,val)
        {
            $.post(url+$.fn.to_b26(x+1)+y+"?attr",
                "<create><formula><![CDATA["+val+"]]></formula></create>",null,"xml");
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
            
	$.get(url+"?attr", function(data) 
	{
		$(data).find("ref").each(function(y)
		{
		    handle_attr(this);
		});
	});
            
    $.clipboardReady(function()
    {
        $('#hn .data').contextMenu('myMenu1',
        {
			bindings:
            {
				'hnumber': function(t)
                {
                    $.clipboard($("#texttocopy").text());
				}
			},
			menuStyle:
            {
				width: '150px'
			},
			itemStyle:
            {
				font: '8px',
				border: 'none',
				padding: '3px'
			},
			itemHoverStyle:
            {
				background: '#EEE',
				border: 'none'
			},
			onContextMenu: function(e) 
			{
				if($(e.target).is("div.cell"))
				{
				    var ref = $.fn.cell_index(e.target);
				    var refstr = $.fn.to_b26(ref[0]+1)+(ref[1]+1);
                    $("#texttocopy").text("=hn(\""+url+refstr+"?hypernumber\")");
                    return true;
                }
                return false;
            }
		});                     
    },{swfpath: "/swf/jquery.clipboard.swf"}); 
});
