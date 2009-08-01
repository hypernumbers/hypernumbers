/**
 * @class HN.Util
 *
 */
HN.Util = {};

HN.Util.dispatch = function(fun, args)
{
  if( typeof fun !== "undefined" ) {
    fun.apply(this, args);
  }
};

HN.Util.trail = function(msg)
{
  HN.Util.postPath("./?trail", {set: {trail: msg}}, null);
};

HN.Util.addEvent = function(obj, type, fn)
{
  if ( obj.attachEvent ) {
    obj['e'+type+fn] = fn;
    obj[type+fn] = function(){obj['e'+type+fn]( window.event );};
    obj.attachEvent( 'on'+type, obj[type+fn] );
  } else
    obj.addEventListener( type, fn, false );
};

HN.Util.removeEvent = function(obj, type, fn)
{
  if ( obj.detachEvent ) {
    obj.detachEvent( 'on'+type, obj[type+fn] );
    obj[type+fn] = null;
  } else
    obj.removeEventListener( type, fn, false );
};

HN.Util.id = function(id)
{
  return document.getElementById(id);
};

HN.Util.y_pos = function(obj)
{
  var top = 0;
  if (obj.offsetParent) {
    do {
	  top += obj.offsetTop;
    } while ((obj = obj.offsetParent));
  }
  return top;
};

HN.Util.x_pos = function(obj)
{
  var left = 0;
  if (obj.offsetParent) {
    do {
	  left += obj.offsetLeft;
    } while ((obj = obj.offsetParent));
  }
  return left;
};

HN.Util.to_b26 = function(cell)
{
  return (--cell < 26)
    ? String.fromCharCode(cell+65)
    : HN.Util.to_b26(Math.floor(cell / 26)) + HN.Util.to_b26(cell%26+1);
};

HN.Util.from_b26 = function(cell)
{
  for(var i = cell.length, pow = 0, sum = 0; i > 0; i--) {
    sum += Math.round((cell.charCodeAt(i-1)-64) * Math.pow(26, pow++));
  }
  return sum;
};

HN.Util.parse_ref = function(ref)
{
  return {
    x:HN.Util.from_b26((ref.match(/[a-z]+/i)[0])),
    y:parseInt(ref.match(/[0-9]+/)[0])
  };
};

HN.Util.parse_range = function(range)
{
  var cells;
  var beginning;
  var end;
  cells = range.split(":");
  beginning = HN.Util.parse_ref(cells[0]);
  end = HN.Util.parse_ref(cells[1]);
  return {
    x1: beginning.x,
    y1: beginning.y,
    x2: end.x,
    y2: end.y
  };
};


HN.Util.coord_to_ref = function(cell)
{
  return HN.Util.to_b26(cell.x)+cell.y;
};

HN.Util.clone = function(obj)
{
  var newObj = (obj instanceof Array) ? [] : {};
  for (i in obj) {
    if (i == 'clone') {
      continue;
    }
    if (obj[i] && typeof obj[i] == "object") {
      newObj[i] = obj[i].clone();
    } else {
      newObj[i] = obj[i];
    }
  }
  return newObj;
};

HN.Util.range_to_url = function(range)
{
  return document.location.pathname
    + HN.Util.coord_to_ref({"x":range.x1, "y":range.y1})+":"
    + HN.Util.coord_to_ref({"x":range.x2, "y":range.y2})
    + "?attr";
};

HN.Util.range_to_str = function(range)
{
  return HN.Util.coord_to_ref({"x":range.x1, "y":range.y1})+":"
    + HN.Util.coord_to_ref({"x":range.x2, "y":range.y2});
};

HN.Util.range_to_str2 = function(range)
{
  if ((range.x1 == range.x2) && (range.y1 == range.y2)) {
    return HN.Util.coord_to_ref({"x":range.x1, "y":range.y1});
  } else {
    return HN.Util.coord_to_ref({"x":range.x1, "y":range.y1})+":"
          + HN.Util.coord_to_ref({"x":range.x2, "y":range.y2});
  }
};

HN.Util.postCell = function(cell, json)
{
  var url = document.location.pathname
    + HN.Util.coord_to_ref(cell)+"?attr";

  $.post(url, JSON.stringify(json));
};

HN.Util.postPath = function(path, json, cb)
{
  $.post(path, JSON.stringify(json), cb);
};

HN.Util.postColumn = function(start, end, json)
{
  var url = document.location.pathname + HN.Util.to_b26(start)
    + ":" + HN.Util.to_b26(end) +"?attr";
  $.post(url, JSON.stringify(json));
};

HN.Util.postRow = function(start, end, json)
{
  var url = document.location.pathname + start + ":" + end +"?attr";
  $.post(url, JSON.stringify(json));
};


HN.Util.postRange = function(range, json)
{
  $.post(HN.Util.range_to_url(range), JSON.stringify(json));
};

HN.Util.readCookie = function(name)
{
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
	var c = ca[i];
	while (c.charAt(0)==' ') {
     c = c.substring(1,c.length);
    }
	if( c.indexOf(nameEQ) == 0 ) {
      return c.substring(nameEQ.length+1,c.length-1);
    }
  }
  return null;
};

HN.Util.createCookie = function(name,value,days)
{
  if( days ) {
	var date = new Date();
	date.setTime(date.getTime()+(days*24*60*60*1000));
	var expires = "; expires="+date.toGMTString();
  } else {
    var expires = "";
  }
  document.cookie = name+"=\""+value+"\""+expires+"; path=/";
};

HN.Util.eraseCookie = function(name) {
	HN.Util.createCookie(name,"",-1);
};