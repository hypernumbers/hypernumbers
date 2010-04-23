$(function()
  {
      
      if( readCookie("auth") !== null ) {
          
          $("#form").empty();
          var home = getHome();
          var secs = 10;
          var link = "http://"+document.location.host+"/u/"+home+"/";
          
          
          if( document.location.pathname == "/_user/login/" ) {
              
              $("#feedback").addClass("error").html(
                  "You are already logged in");
              
              $("#form").html(
                  "<p>You are logged in as <strong>"+home+"</strong>"
                      +"<p>Your home page is <a href=\"/u/"+home+"/\">"
                      +link+"</a></p>");              
          } else {

              $("#feedback").addClass("error").html(
                  "You are not authorised to view this page.");
              
              // TODO: Add username
              $("#form").html(
                  "<p>You are already logged in</p>");
          }
          
          $("#form").append("<p><button id='logout'>logout</button></p>");
          $("#logout").bind("mousedown", function(e) {
              createCookie("auth","",-1);
              window.location.reload( true );
          });
      } else {
          $("#feedback").html(
              "Please Enter Details to Login");   
      }
      
      var onsubmit = function()
      {
          var email = $("#email").val();
          var pass = $("#pass").val();
          var remember = $("#remember").is(":checked");
          
          if( pass!="" && email!="" ) {
              
              var success = function(data) {
                  if( data.response == "error" ) {
                      $("#feedback").addClass("error").text(
                          "Sorry, those credentials were not found");
                  } else {
                      window.location.reload( true );
                  }
              };
              
              var error = function(data) {
                  $("#feedback").addClass("error").text(
                      "Sorry, there was an error processing your login, "
                          + "please try again");
              };
              
              var data = '{"email\":"'+email+'", "pass":"'+pass
                  +'", "remember":"'+remember+'"}';
              $("#feedback").removeClass("error").text(
                  "Checking Details...");
              $.ajax({
                  "type"     : "POST", 
                  "url"      : "/_user/login/",
                  "data"     : data, 
                  "dataType" : "json",
                  "success"  : success, 
                  "error"    : error
              });
          } else {
              $("#feedback").addClass("error").text(
                  "Please enter full details");
          }
          return false;
      };
      $("form").submit(onsubmit);
  });

var createCookie = function(name,value,days)
{
	if (days) {
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"=\""+value+"\""+expires+"; path=/";
};

var readCookie = function(name)
{
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
	    var c = ca[i];
	    while (c.charAt(0)==' ') c = c.substring(1,c.length);
	    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
};

var getHome = function() 
{
    var cookie = readCookie("auth");
    var unesc_cookie = unescape(cookie);
    return unesc_cookie.split(":")[1];
};

var escapeAuthCookieData = function(data)
{
    var segs = data.split(":");
    return segs[0] + ":" + escape(segs[1]) + ":" + segs[2];
};