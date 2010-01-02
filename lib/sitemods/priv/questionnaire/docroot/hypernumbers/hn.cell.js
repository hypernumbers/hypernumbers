$(function()
  {
      var success = function(data) {
          var ref = HN.Util.parseRef(document.location.pathname);
          var msg;
          if (data.rawvalue === "") {
              msg = "<p>No value for this hypernumber has been set</p>";
          } else {
              msg = "<p>Its value is: " + data.rawvalue + "</p>";
          };
          $("#text").html("<p><p>This page shows the hypernumber</p>"
                          + "<p>" + document.location.protocol +"//"
                          + document.location.host
                          + document.location.pathname + "</p>"
                          + msg
                          + "<br /><p>You probably want the page:</p>"
                          + "<p><a href=\"" + document.location.protocol +"//"
                          + document.location.host
                          + ref.path + "\">" 
                          + document.location.protocol +"//"
                          + document.location.host
                          + ref.path + "</p>"
                         );
      };
      var error = function(data) {
              $("#text").html("There has been an error!");
      };
      $.ajax({type: "GET", url: document.location.pathname,
              dataType:"json",
              success: success, error: error});
  });