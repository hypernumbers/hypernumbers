$(function()
{

  if( readCookie("auth") !== null ) {
    var home = readCookie("auth").split(":")[1];
    var link = "http://"+document.location.hostname+"/"+home+"/";
    $("#feedback").addClass("error").html(
      "You are not authorised to view this spreadsheet, "
      +"<strong>home :</strong> <a href=\"/"+home+"/\">"+link+"</a>");
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
            "Sorry, the credentials you entered were not found");
        } else {
          var days = ( data.token.split(":")[0] == "session" )
            ? false : 30;
          createCookie("auth", data.token, days);
          if( document.location.pathname.search("_user") === -1 ) {
            window.location.reload( true );
          } else {
            window.location.href = "/"+data.token.split(":")[1]+"/";
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

