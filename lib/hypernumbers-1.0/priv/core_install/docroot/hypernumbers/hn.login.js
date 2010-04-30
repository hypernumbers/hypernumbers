$(function()
  {
      var cookie = readCookie("auth"),
      email = parseEmail(cookie);

      if (email != "anonymous") {
          $("#feedback").addClass("error").html("Not Authorized");
          
          $("#form").html
          ("<p>Sorry your account <strong>"+email+"</strong> "
           +"does not have access to this page</p>");
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
                      window.location.href = data.redirect;
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
                  "url"      : "/_login/",
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

function readCookie(name)
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

function parseEmail(cookie)
{
    var email = cookie.split("|")[0];
    email = email.replace("!", "@");
    return email;
}   
