HN.UI = {};

HN.UI.init_dialogs = function(layout)
{
  var el = document.getElementsByClassName("dialog"),
     len = el.length,
     cur = null,
    dpos = {x:0, y:0},
   start = {x:0, y:0};

  $(".mclose").click( function() {
    HN.UIActions.close_dialog(layout, $(this).parent().attr("id"));
  });

  var up = function( e ) {
    HN.Util.removeEvent(document, "mouseup", up);
    HN.Util.removeEvent(document, "mousemove", move);
  };

  var move = function( e ) {
    var y = (dpos.y + (e.clientY - start.y));
    var x = (dpos.x + (e.clientX - start.x));
    cur.style.top  = (( y > 0 ) ? y : 0) + "px";
    cur.style.left = (( x > 0 ) ? x : 0) + "px";
  };

  var down = function( e ) {
    if( e.target.className == "close") {
      this.style.display = "none";
      e.preventDefault();
    } else if(    e.target.nodeName == "H2"
               || e.target.parentNode.nodeName == "H2" ) {
      e.preventDefault();
      cur   = this;
      dpos  = {x:parseInt(this.style.left), y:parseInt(this.style.top)};
      start = {x:e.clientX, y:e.clientY};

      HN.Util.addEvent(document, "mousemove", move);
      HN.Util.addEvent(document, "mouseup", up);
    }
  };

  for( var i = 0; i < len; i++ ) {
    el[i].style.top = "50px";
    el[i].style.left = ($("body").width() - 400) + "px";
    el[i].innerHTML = "<div class='close'>&nbsp;</div>" + el[i].innerHTML;
    HN.Util.addEvent(el[i], "mousedown", down);
  }
};
