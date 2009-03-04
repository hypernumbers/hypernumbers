var rawvalue = new Array();

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


var handle_attr = function(refxml)
{
  var el = $(refxml);

  var ref   = el.attr("ref");
  var name  = ((el.children()[0]).nodeName).toLowerCase();
  var value = el.text();

  if(name == "rawvalue")
    {
      rawvalue[ref] = value;
      var split = HN.Util.parse_ref(ref)
      alert(split);
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
  
}


$(function() {
    if (GBrowserIsCompatible()) {
      var map = new GMap2(document.getElementById("map_canvas"));
      map.setCenter(new GLatLng(55.90215345, -3.69828951), 13);
      
      // Add 10 markers to the map at random locations
      var bounds = map.getBounds();
      var southWest = bounds.getSouthWest();
      var northEast = bounds.getNorthEast();
      var lngSpan = northEast.lng() - southWest.lng();
      var latSpan = northEast.lat() - southWest.lat();
      for (var i = 0; i < 10; i++) {
        var point = new GLatLng(southWest.lat() + latSpan * Math.random(),
                                southWest.lng() + lngSpan * Math.random());
        map.addOverlay(new GMarker(point));
	  }
    }
    init_flash();
    load_data();
  });			  
