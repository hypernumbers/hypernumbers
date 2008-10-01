var ctl_panel = "<div class=\"ctl-panel\">"
    + "<input type=\"submit\" value=\"delete\" class=\"delete_button\" />"
    + "<input type=\"submit\" value=\"edit\" class=\"edit_button\"/></div>";

var default_item = "<li class=\"form-item\">"
    + "<div class=\"rendered\">New Form Item</div></li>";

var edit_panel = "<div class=\"edit\"><h3>Edit</h3>"
    + "<div class=\"row\"><label>Type of element</label><select class='type'>"
    + "<option value='text'>Text</option>"
    + "<option value='header'>Header</option>"
    + "<option value='button'>Button</option>"
    + "<option value='input'>Single Line Input</option>"    
    + "<option value='textarea'>Multi Line Input</option>"    
    + "<option value='select'>Select Item</option>"        
    + "<option value='livetable'>Live Table</option>"    
    + "</select></div><div class=\"attributes\">test</div></div>";

var defaultval = new Array();
defaultval['input'] = "Input Label";
defaultval['text']  = "Sample Text";
defaultval['textarea']  = "Sample Text";
defaultval['header']  = "Header";
defaultval['select']  = "Pick an option";
defaultval['button']  = "Submit";
defaultval['livetable']  = "";

var text_child = new Array();
text_child['header'] = "h1";
text_child['text']   = "p";
text_child['button'] = "button";
text_child["input"]  = "label";
text_child["textarea"]  = "label";
text_child["select"] = "label";
text_child["livetable"] = "made_up";

var editform = new Array();
var rendered = new Array();
rendered['input'] = "<div><label>{value}</label><br /><input type=\"text\" /></div>";
editform['input'] = "<div class=\"row\"><label>Input Label</label><input type=\"text\" "
    + " value=\"{value}\" class=\"labeltext\"/></div><div class=\"row\"><label>"
    + "Destination</label><input type=\"text\" class=\"dest\"/></div>";

rendered['textarea'] = "<div><label>{value}</label><br /><textarea></textarea></div>";
editform['textarea'] = "<div class=\"row\"><label>Input Label</label><input type=\"text\" "
    + " value=\"{value}\" class=\"labeltext\"/></div><div class=\"row\"><label>"
    + "Destination</label><input type=\"text\" class=\"dest\"/></div>";

rendered['text'] = "<p>{value}</p>";
editform['text'] = "<div><textarea class=\"labeltext\">{value}</textarea></div>";

rendered['header'] = "<h1>{value}</h1>";
editform['header'] = "<div><textarea class=\"labeltext\">{value}</textarea></div>";

rendered['select'] = "<div><label>{value}</label><br />"
    +"<select></select>";
editform['select'] = "<div class=\"row\"><label>Input Label</label><input type=\"text\" "
    + " value=\"{value}\" class=\"labeltext\"/></div><div class=\"row\"><label>"
    + "Data Source</label><input type=\"text\" class=\"source\"></div><div class=\"row\"><label>"
    + "Destination</label><input type=\"text\" class=\"dest\"></div>";

rendered['livetable'] = "<table><tr><td>Live Table</td></tr></table>";
editform['livetable'] = "<div class=\"row\"><label>Data Source</label>"
    + "<input type=\"text\" class=\"source\"></div><div class=\row\"><label>"
    + "Editable</label><input type=\"checkbox\" class=\"editable\" /></div>";

rendered['button'] = "<button>{value}</button>";
editform['button'] = "<div class=\"row\"><label>"
    + "Button Text</label><input type=\"text\" class=\"labeltext\"></div>";

var edit_item = function(element)
{
    if(element.hasClass("editing"))
    {
        element.removeClass("editing");
    }
    else
    {
        $("#form li.editing").removeClass("editing");
        element.addClass("editing");
        build_edit_form(element,element.data("type"));
    }   
};

var build_edit_form = function(element,type)
{
    var form = element.find(".edit .attributes")

    var val = element.data("value") 
    if(val == undefined)
        val = defaultval[type];
    
    form.empty().append($(editform[type].replace("{value}",val)));
    element.find(".rendered").empty().append($(rendered[type].replace("{value}",val)));

    if(type == "input" || type == "header" || type == "text" 
       || type == "select" || type == "textarea" || type == "button")
    {
        var change_text = function(e)
        {
            var val = $(this).val();
            element.find(".rendered "+text_child[type]).text(val);
            element.data("value",val);
        };
        form.find(".labeltext").keyup(change_text);
    }
    
    if(type == "input" || type == "textarea" || type == "select")
    {
        var change_text = function(e)
        {
            element.data("dest",$(this).val());
        };
        form.find(".dest").keyup(change_text);
        if(typeof(element.data("dest") != undefined))
            form.find(".dest").val(element.data("dest"));
    }

    if(type == "select" || type == "livetable")
    {
        var change_text = function(e)
        {
            element.data("source",$(this).val());
        };
        form.find(".source").keyup(change_text);
        if(typeof(element.data("source") != undefined))
            form.find(".source").val(element.data("source"));
    }

    if(type == "livetable")
    {
        var change_editable = function(e)
        {
            element.data("editable",$(this).is(":checked")?"editable":"static");
        };
        form.find(".editable").click(change_editable);
        if(typeof(element.data("editable") != undefined))
        {
            if(element.data("editable") == "editable")
                form.find(".editable").attr("checked","checked");
        }
    }
}

var create_form_item = function(element)
{
    var enter = function()
    {
        element.addClass("hover");
    };
    var leave = function()
    {
        element.removeClass("hover");
    };
    var select = function()
    {
        edit_item($(this).parents("li.form-item"));
    };
    var remove = function()
    {
        $(this).parents("li.form-item").remove();
    };
    var changetype = function()
    {
        element.data("type",$(this).val());
        build_edit_form(element,$(this).val());
    };

    element.append($(ctl_panel));
    element.append($(edit_panel));
    element.bind("mouseenter",enter);
    element.bind("mouseleave",leave);
    element.find("input.delete_button").bind("click",remove);
    element.find("input.edit_button").bind("click",select);
    element.find(".edit .type").change(changetype);
    element.data("type","text");
};

var handleElement = function(params)
{
    var new_item = $(default_item);
    create_form_item(new_item);
    new_item.data("type",params[0]);
    new_item.data("value",params[2]);
    
    if(params[0] == "textarea" || params[0] == "input" 
       || params[0] == "select" || params[0] == "button")
        new_item.data("dest",params[4]);

    if(params[0] == "select" || params[0] == "livetable")
        new_item.data("source",params[3]);

    if(params[0] == "livetable")
        new_item.data("editable",params[2]);

    edit_item(new_item);
    $("#form").append(new_item);
}

function setup_form(path)
{
    var root = "http://"+document.location.host+"/forms/"+path;

    var init = function()
      {
          create_form_item($(this));
      };

      var add = function()
      {
          var new_item = $(default_item);
          create_form_item(new_item);
          $("#form").append(new_item);
          edit_item(new_item);
      };

      var save = function()
      {     
          if($("li.form-item").size() > 0)
          {           
              var data = new Array();     
              var each = function()
              {
                  var type = $(this).data("type");
                  var val = $(this).find(".rendered "+text_child[type]).text();
                  
                  if(type == "text" || type== "header" || type == "button")
                      data.push([type,"#form",val,"null","null"]); 
                  else if(type == "input" || type == "textarea")
                      data.push([type,"#form",val,"null",$(this).data("dest")]); 
                  else if(type == "select")
                      data.push([type,"#form",val,$(this).data("source"),$(this).data("dest")]); 
                  else if(type == "livetable")
                  {
                      data.push([type,"#form",$(this).data("editable"),
                                 $(this).data("source"),"null"]); 
                  }
              };

              $("#save").attr("disabled","disabled").val("saving...");
              
              $.post(root+"?attr","<delete/>",null,"xml");

              $("li.form-item").each(each);
              
              var range = "a3:e"+(data.length+2);
              $.post(root+"a1?attr","<create><formula>"+range+"</formula></create>",null,"xml");
              $.post(root+"b1?attr","<create><formula>"+$("#action").val()
                     + "</formula></create>",null,"xml");
              
              var post = "<create>";
              for(var x = 0; x < data.length; x++)
              {
                  for(var y = 0; y < data[x].length; y++)
                  {
                      post += "<formula>"+data[x][y]+"</formula>";
                  }
              }
              var success = function()
              {
                  $("#save").removeAttr("disabled").val("save");
              }
              
              $.post(root+range+"?attr",post+"</create>",success,"xml");
          }
      };

      var load = function(data)
      {
          var range = $(data).find("value").text();
          if(range != "")
          {
              var end = function()
              {
                  $("li.form-item.editing").removeClass("editing");
              };
              get_sorted_attr(root,range,handleElement,end);
          }
      };

      $.get(root+"/a1?attr",load);
      
      $("#add_item").click(add);
      $("#save").click(save);
      $(".form-item").each(init);
      $("#form").sortable({});
};