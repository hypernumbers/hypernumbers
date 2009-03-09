var rawvalue = new Array();
var mappoints = new Array();
var map;

var loc = document.location;
var url = loc.protocol+"//"+loc.host+loc.pathname;
var auth = false;

function init_flash()
{
  var so = new SWFObject("/swf/JsSocket.swf",
                         "mymovie","470","200","8","#FFFFFF");
  so.addParam("allowScriptAccess", "always");
  so.write("flashcontent");
}

function createMarker(i, x, y)
{
  var newPoint = new GLatLng(x, y);
  var marker = new GMarker(newPoint);
  GEvent.addListener(marker, "click", function() {
                       marker.openInfoWindowHtml(rawvalue[3][i]);
                     });
  return marker;
}

update_point = function(i, x, y)
{
  // console.log("in point " + i);
  var newMarker = createMarker(i, x, y)
  if (mappoints[i]) 
  {
    // console.log("point " + i + " " + mappoints[i] + " already exists, removing it...");
    map.removeOverlay(mappoints[i]);
  }
  map.addOverlay(newMarker);
  mappoints[i] = newMarker;
}

var handle_attr = function(refxml)
{
  var el = $(refxml);

  var ref   = el.attr("ref");
  var name  = ((el.children()[0]).nodeName).toLowerCase();
  var value = el.text();
  
  if(name == "rawvalue")
    {
      var split = new Array;
      split = HN.Util.parse_ref(ref);
      if (!rawvalue[split[1]]) {
        rawvalue[split[1]] = new Array;
      }
      y = split[0];
      x = split[1];
      // console.log("x is " + x + " y is " + y + " and value is " + value);
      rawvalue[x][y] = value;
      if (!rawvalue[7] || !rawvalue[8] || !rawvalue[3]) 
        {
        }
      else
        {
          if (rawvalue[7][y] && rawvalue[8][y] && rawvalue[3][y]) 
            {
              update_point(y, rawvalue[7][y], rawvalue[8][y]);
            }
        }
    }
};


load_data = function()
{
  var get_attr = function(data)
  {
	var fun = function(y) { handle_attr(this); };
	$(data).find("ref").each(fun);
  }
  
  $.ajax({
	type: "GET",
        url: document.location.pathname+"?attr",
        dataType: "xml",
        success: get_attr
        });
};

$(function() 
  {
    if (GBrowserIsCompatible()) 
      {
        var m = new GMap2(document.getElementById("map_canvas"));
        map = m;
        map.setCenter(new GLatLng(55.89215345, -3.6028951), 11);
      }
    init_flash();
    load_data();
  });			  
