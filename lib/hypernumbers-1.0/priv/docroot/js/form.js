var post_vars = new Array();
var root = "http://"+document.location.host+document.location.pathname;

$.fn.parse_cell = function(cell) 
{
    var x = $.fn.from_b26((cell.match(/[a-z]+/i)[0]).toLowerCase());
    var y = parseInt(cell.match(/[0-9]+/)[0]);
   
    return [x,y];
};

$.fn.to_b26 = function(cell) 
{
    return String.fromCharCode(cell+96);
};

$.fn.from_b26 = function(cell) 
{
    return cell.charCodeAt(0)-96;
};

var handle_attr = function(refxml)
{
    var el = $(refxml);    
    var ref   = el.attr("ref").toLowerCase();
    var name  = ((el.children()[0]).nodeName).toLowerCase();
    var value = el.text();
    
    if(name == "value")
    {
        $("."+ref).text(value);
    }
    else if(name == "font-weight")
    {
        $("."+ref).css("font-weight",value);
    }
}

var dosubmit = function()
{
    post_vars.sort(function(a,b)
                   {
                       return a.destination.charCodeAt(0) < 
                           b.destination.charCodeAt(0) ? -1 : 1;
                   });
    
    var str = "<create>";
    
    for(i=0; i< post_vars.length; i++)
    {
        var getChecked = function(arr)
        {
            for(x=0; x< arr.length; x++)
            {
                if((arr[x])[0].checked)
                    return arr[x].attr('rawvalue');
            }
        }
        
        var val = (post_vars[i].type != "radio") 
            ? post_vars[i].input.val()
            : getChecked(post_vars[i].input);
        
        str += "<formula><![CDATA["+val+"]]></formula>";
    }
    str += "</create>";
    
    $.post(root+"?attr&lastrow",str,null,"xml");
};

var edit_cell = function(cell)
{
    var parent = cell.parent();
    var onclick = function()
    {
        var save = function()
        {
            var val = input.val();
            var el = $("<div class='"+ref+" cell' name='"+ref+"'>"+val+"</div>");
            parent.empty().append(el);
            edit_cell(el);
            $.post(root+ref+"?attr","<create><formula><![CDATA["+val+"]]>"
                   + "</formula></create>",null,"xml");
        };

        var cancel = function()
        {
            var el = $("<div class='"+ref+" cell' name='"+ref+"'>"+value+"</div>");
            parent.empty().append(el);
            edit_cell(el);
        };

        var value = cell.text();
        var ref  = cell.attr("name");
        var input = $("<textarea></textarea>").val(value);
        var submit = $("<input type='submit' value='save' />").click(save);
        var cancel = $("<input type='submit' value='cancel' />").click(cancel);
        parent.empty().append(input).append(submit).append(cancel);
    }
    cell.click(onclick);
};

var handle = function(data)
{
    if(data[0] == "header")
    {
        $(data[1]).append($("<h1>"+data[2]+"</h1>"));
    }
    else if(data[0] == "text")
    {
        $(data[1]).append($("<p>"+data[2]+"</p>"));
    }
    else if(data[0] == "select")
    {       
        var str = $("<div class='row'><label>"+data[2]+"</label><br /><select></select></div>");
        var select = str.find("select");
        $.get(data[3]+"?attr",function(data) 
              {
                  var add_select = function()
                  {
                      if($(this).find("rawvalue").size() > 0)
                      {
                          select.append($("<option>"+
                                          $(this).find("rawvalue > string").text()
                                          + "</option>"));
                      }
                  };
                  $(data).find("ref").each(add_select);
              });
        
        $(data[1]).append(str);
        post_vars.push({type:"select",input:select,destination:data[4]});
    }
    else if(data[0] == "input")
    {
        var str = $("<div class='row'><label>"+data[2]+"</label><br /><input type=\"text\" /></div>");
        $(data[1]).append(str);
        post_vars.push({type:"text",input:str.find("input"),destination:data[4]});
    }
    else if(data[0] == "textarea")
    {
        var str = $("<div class='row'><label>"+data[2]+"</label><br /><textarea></textarea></div>");
        $(data[1]).append(str);
        post_vars.push({type:"textarea",input:str.find("textarea"),destination:data[4]});  
    }
    else if(data[0] == "livetable")
    {
        var x = $.fn.parse_cell(data[3].split(":")[0]);
        var y = $.fn.parse_cell(data[3].split(":")[1]);

        var datatable = $("<table></table>").appendTo($(data[1]));
        for(var z = x[1]; z <= y[1]; z++)
        {
            var row = $("<tr/>").appendTo(datatable);
                
            for(var n = x[0]; n <= y[0]; n++)
            {
                var ref = ($.fn.to_b26(n)+z);
                var td = $("<td><div class='"+ref+" cell' name='"+ref+"'></div></td>");

                if(data[2] == "editable")
                    edit_cell(td.find(".cell"));

                row.append(td);
            }
        }

        var populate = function(data)
        {
            var fun = function(y) { handle_attr(this); };
	    $(data).find("ref").each(fun);
        };
        $.get(root+data[3]+"?attr",populate);        
    }
    else if(data[0] == "button")
    {
        var button = $("<input type='submit' value='"+
                       data[2]+"' />").appendTo($(data[1]));
        button.click(dosubmit);
    }
}

$(function()
  {

      var so = new SWFObject("/swf/JsSocket.swf", 
	  "mymovie", 
	  "470", 
	  "200", 
	  "8", 
	  "#FFFFFF");
      so.addParam("allowScriptAccess", "always");
      so.write("flashcontent");    

      var loadform = function(data)
      {
          var datapath = $(data).text();
          var load = function(data)
          {
              var range = $(data).find("value").text();
              var end = function()
              {
              }
              if(range != "")
              {
                  get_sorted_attr(datapath,range,handle,end);
              }
          };
          $("input[type='submit']").click(dosubmit);
          $.get(datapath+"a1?attr",load);
      };

      $.get("?attr=form",loadform);
  });