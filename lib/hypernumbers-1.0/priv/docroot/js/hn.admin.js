var loc = document.location;
var url = loc.protocol+"//"+loc.host+loc.pathname;
var auth = false;

var sheets = new Array();
var templates = new Array();
var selected = null;

var refresh = function()
{
  document.location.href = "/_admin/";
};

$(function()
  {
    $("#menu").filemenu();

    var loaded = function()
    {
      display_menu(sheets,$("#sheets"),"sheet");
      display_menu(templates,$("#templates"),"template");
    };

    $.ajax({
      type: "GET",
	  url: "/?pages",
	  dataType: "xml",
	  success: function(data) { get_pages(data,loaded); }
    });

    var f = function()
    {
      var name = $("#new_name").val();
      if(name == "")
      {
        alert("need to enter a name");
        return;
      }

      if(name[0] != "/")
        name = "/"+name;
      if(name[name.length-1] != "/")
        name += "/";

      var path  = ($("#new_type").val() == "spreadsheet")
        ? name : "/templates"+name;

      var post = "<create><formula>\"\"</formula></create>";
      $.post(path+"a100?attr",post,refresh,"text/xml");
    };
    $("#new_submit").click(f);

    setup_buttons();
  });

var select_sheet = function(el,type)
{
  $("#instance").hide();
  var f = function(data)
  {
    if($(data).find("dynamic").length > 0)
      {
        $("#instance").show();
      }
  };
  $.get("/"+selected.path+"/?attr",f,"text/xml");

  $("#dyntpl_details").hide();
  $(".selected").removeClass("selected");
  $(el).addClass("selected");
  $("#sel_name").text("/"+selected.path);

  $("#panel .ss_btn").css("display","block");
  $("#panel .tpl_btn").css("display",(type=="sheet")?"none":"block");
};

var display_menu = function(arr,dom,type)
{
  if(arr.length == 0)
    dom.append("<div>empty</div");
  else
    buildtree(arr,dom,type);
};

var buildtree = function(arr,root,type)
{
  var ul = $("<ul />").appendTo(root);
  var f = function()
  {
    var that = this;
    var li = $("<li><div class=\"row\"><a>"+that.name
      + "</a></div></li>").appendTo(ul);

    li.find("div").click(function(){
      selected = that; select_sheet(this,type); });

    if(typeof that.children != "undefined")
    {
      buildtree(that.children,li,type);
    }
  };
  $.each(arr,f);
};

var get_pages = function(data,f)
{
  var add = function(root,dir,path)
  {
	var fun = function()
	{
      var name = $(this).attr("path");
      var obj = new Object();
      obj.path = path+name;
      obj.name = name;

      root.push(obj);

      if($(this).children().size() > 0)
	  {
        obj.children = new Array();
		add(obj.children,this,obj.path+"/");
	  }
	};

	$(dir).children().each(fun);
  };

  var tmp = new Array();
  add(tmp,$(data).find("> dir"),"");

  for each(var x in tmp)
  {
    if(x.path=="templates")
      templates = x.children;
    else
      sheets.push(x);
  }

  f();
};

var setup_buttons = function()
{
  var instance = function()
  {
    window.open("/"+selected.path+"/?new") ;
  };
  $("#instance").click(instance);

  var dynamic_cancel = function()
  {
    $("#dyntpl_details").hide();
  };
  $("#dyntpl_cancel").click(dynamic_cancel);

  var dynamic_save = function()
  {
    var post = "<template><url>"+$("#dyntpl_url").val()+"</url></template>";
    $.post("/"+selected.path+"/?attr",post,refresh,"text/xml");
  };
  $("#dyntpl_save").click(dynamic_save);


  var dynamic_template = function()
  {
    $("#dyntpl_details").show();
  };
  $("#sel_dyntpl").click(dynamic_template);

  var del = function()
  {
    $.post("/"+selected.path+"/?attr","<delete/>",refresh,"text/xml");
  };
  $("#sel_delete").click(del);

  var edit = function()
  {
    window.open("/"+selected.path+"/?gui=index") ;
  };
  $("#sel_edit").click(edit);
};
