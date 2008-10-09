var post_vars = new Array();
var root = "http://"+document.location.host+document.location.pathname;
var action = false;


var handle = function(data)
{
  var id="#"+data[0][0];
  var page="trafficlight"+data[0][0];
  $(id).append($("<a href=\""+page+"/\"><img src=\""+root+"/img/"+data[0][1]+".jpg\"/></a>"));
}
  
  $(function()
    {
      $("#header").append($("<h1>"+root+"<br/>Dashboard</h1>"));
      console.log("root is "+root);
      var loadform = function(data)
        {
          var datapath = $(data).text();
          console.log("datapath is "+datapath);
          var reflist = new Array();
          var eachref = function()
          {
            if($(this).find("rawvalue").size() > 0)
              {
                var ref = parseRef($(this).attr("ref"));
                //console.log("ref[0] is "+ref[0]+" ref[1] is "+ref[1]);
                if(typeof reflist[ref[1]] == "undefined")
                  reflist[ref[1]] = new Array();
                
                reflist[ref[1]][ref[0]-1] = $(this);
              }
          }
          $(data).find("ref").each(eachref);
          
          var handle_stuff = function(j)
          {
            if(this.length>0)
              {
                var params = new Array();
                for(i=0; i < this.length; i++) 
                  {
                    var cellstuff = new Array();
                    cellstuff[0]=j;
                    cellstuff[1]=this[i].find("string").text();
                    params.push(cellstuff);
                  }
                handle(params);
              }
          };
          $.each(reflist,handle_stuff);
        };
      $.get("a?attr",loadform);
  });

function parseRef(cell)
{
    return [
        ((cell.match(/[a-z]+/i)[0]).charCodeAt(0)-64),
        parseInt(cell.match(/[0-9]+/)[0])];
}

