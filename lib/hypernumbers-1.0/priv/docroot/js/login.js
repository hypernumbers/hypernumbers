function createCookie(name,value,days) 
{
    if (days) 
    {
	var date = new Date();
	date.setTime(date.getTime()+(days*24*60*60*1000));
	var expires = "; expires="+date.toGMTString();
    }
    else var expires = "";
    document.cookie = name+"="+value+expires+"; path=/";
}

$(function()
  {
      var onsubmit = function()
      {
          var email = $("#email").val();
          var pass = $("#pass").val();
          
          if(pass!="" && email!="")
          {
              var xml = "<login>"
                  +"<email>"+ $("#email").val() +"</email>"
                  +"<password>"+ $("#pass").val() +"</password>"
                  +"</login>";
              
              var callback = function(data)
              {
                  if($(data).find("unauthorised").length == 1)
                  {
                      $("#feedback").text("Invalid Details");
                  }
                  else 
                  {
                      var token = $(data).find("token").text();
                      createCookie("auth",token,30);
                      window.location.reload( true );
                  }
              }
              $.post("/",xml,callback,xml);
          }
          else
          {
              $("#feedback").text("Enter full details");
          }
          return false;
      }
      $("form").submit(onsubmit);
  });