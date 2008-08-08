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
          var token = $("#token").val();
          
          if(token != "")
          {
              createCookie("token",token,30);
              window.location.reload( true );
              
          }
          return false;
      }
      $("form").submit(onsubmit);
  });