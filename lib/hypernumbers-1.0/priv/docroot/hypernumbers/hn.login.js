$(function()
  {
      
      if( readCookie("auth") !== null ) {
          
          $("#form").empty();
          var home = readCookie("auth").split(":")[1],
          secs     = 10,
          link     = "http://"+document.location.host+"/u/"+home+"/";
          
/*          var show = function() {
              if( secs == 0 ) {
                  window.clearInterval(id);
                  document.location.href = link;
              } else {
                  var s = (secs == 1) ? "second" : "seconds";
                  
                  secs--;
              }
          };
          show();
          var id = window.setInterval(show, 1000);
          
*/
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
              
              $("#form").html(
                  "<p>You are logged in as <strong>"+home+"</strong>"
                      +"<p>Your home page is <a href=\"/u/"+home+"/\">"
                      +link+"</a></p>");
          }

          $("#form").append("<p><a id='logout'>logout</a></p>");

          $("#logout").bind("mousedown", function(e) {
              createCookie("auth","",-1)
          });
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
                      var days = ( data.token.split(":")[0] == "session" )
                          ? false : 30;
                      createCookie("auth", data.token, days);
                      if( document.location.pathname.search("_user") === -1 ) {
                          window.location.reload( true );
                      } else {
                          window.location.href 
                              = "/u/"+data.token.split(":")[1]+"/";
                      }
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
              $.ajax({type:"POST", url:"/_user/login/",
                      data:data, dataType:"json",
                      success:success, error:error});
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

