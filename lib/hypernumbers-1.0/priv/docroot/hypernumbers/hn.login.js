$(function()
{
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
      $.ajax({type:"POST", url:"/_auth/login/",
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
