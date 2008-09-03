function parseRef(cell)
{
    return [
        ((cell.match(/[a-z]+/i)[0]).charCodeAt(0)-64),
        parseInt(cell.match(/[0-9]+/)[0])];
}

var get_sorted_attr = function(root,range,handle,end)
{
    $.get(root+range+"?attr",function(data)   
          {
              var reflist = new Array();
              
              $(data).find("ref").each(function()
                                       
                                       {
                                           if($(this).find("rawvalue").size() > 0)
                                           {
                                               var ref = parseRef($(this).attr("ref"));
                                               if(typeof reflist[ref[1]] == "undefined")
                                                   reflist[ref[1]] = new Array();
                                               
                                               reflist[ref[1]][ref[0]-1] = $(this);
                                           }
                                       });
              
              $.each(reflist,function(i)
                     {
                         if(this.length>0)
                         {
                             var params = new Array();
                             
                             for(i=0; i < this.length; i++) 
                                       {
                                           params.push(this[i].find("string").text());
                                       }
                             handle(params);
                         }
                     });
              end();
          });
}