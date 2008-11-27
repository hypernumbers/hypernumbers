var loc = document.location;
var url = loc.protocol+"//"+loc.host+loc.pathname;
var auth = false;

var sheets = new Array();
var selected = null;

var btns = "<a class=\"delete\" alt=\"delete spreadsheet\">&nbsp;</a>";

var refresh = function()
{
  document.location.href = "/";
};

$(function()
  {
    $("#menu").filemenu();

    var loaded = function()
    {
      display_menu(sheets,$("#sheets"));

      var expand = function()
      {
        var parent = $(this).parent().parent();
        parent.toggleClass("expand");
        parent.toggleClass("collapse");
      };

      $(".expand_btn").click(expand);
      setup_buttons();
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

      var post = "<create><formula>\"\"</formula></create>";
      $.post(name+"a100?attr",post,refresh,"text/xml");
    };
    $("#create_go").click(f);
  });

var select_sheet = function(el)
{
  $("#instance").hide();
  var f = function(data)
  {
    if($(data).find("instance").length == 0)
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
};

var display_menu = function(arr,dom)
{
    buildtree(arr,dom);
};

var buildtree = function(arr,root)
{
  var ul = $("<ul />").appendTo(root);
  var f = function()
  {
    var that = this;
    var haschildren = typeof that.children != "undefined";
    var btn = "<a class=\""+((haschildren)?"expand":"leaf")+"_btn\">&nbsp;</a>";
    var li = $("<li class=\"collapse clearfix\"><div class=\"row clearfix\">"
               +btn+btns+"<a class=\"edit\" href=\""+that.path
               +"/\" target=\"blank\" \">"+that.name
               +"</a></div></li>").appendTo(ul);

    li.data("path",that.path);

    li.find("div").click(function(){
      selected = that; select_sheet(this); });

    if(haschildren)
      buildtree(that.children,li);

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

  sheets = tmp;

  f();
};

var setup_buttons = function()
{
  var instance = function()
  {
    window.open("/"+selected.path+"/?new") ;
    refresh();
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
    var path = $(this).parent().parent().data("path");
    $.post("/"+path+"/?attr","<delete/>",refresh,"text/xml");
    return false;
  };
  $(".delete").click(del);

  var create = function()
  {
    $("#create").hide();
    $("#create_details").show();
  };
  $("#create").click(create);

  var create_cancel = function()
  {
    $("#create_details").hide();
    $("#create").show();
  };
  $("#create_cancel").click(create_cancel);

  var backup = function()
  {
    document.location.href = "/?save";
  };
  $("#backup").click(backup);

  var restore = function()
  {
    $("#import,.cover").show();
  };
  $("#restore").click(restore);

  var cancel_restore = function()
  {
    $("#import,.cover").hide();
  };
  $("#cancel_restore").click(cancel_restore);

  var do_restore = function()
  {
    var f = function(data,status)
    {
      refresh();
    };
    $("#pick,#loading").toggle();
    $.ajaxFileUpload({
      url:'/?import',
      secureuri:false,
      datatype: "json",
	  fileElementId:'restorefile',
      success:f});
  };
  $("#do_restore").click(do_restore);
};
