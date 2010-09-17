/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: false */
/*global HN: false, hn: false, $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */
$(function()
  {
      var error,
      success;
      
      success = function (data) {
          var ref = HN.Util.parseRef(document.location.pathname),
          msg;

          if (data === "") {
              msg = "<p>No value for this hypernumber has been set</p>";
          } else {
              msg = "<p>Its value is: " + data + "</p>";
          }
          $("#text").html("<p><p>This page shows the hypernumber</p>" +
                          "<p>" + document.location.protocol + "//" +
                          document.location.host +
                          document.location.pathname + "</p>" +
                          msg +
                          "<br /><p>You probably want the page:</p>" +
                          "<p><a href=\"" + document.location.protocol +"//" +
                          document.location.host +
                          ref.path + "\">" +
                          document.location.protocol +"//" +
                          document.location.host +
                          ref.path + "</p>" 
                         );
      };
      error = function(data) {
          $("#text").html("There has been an error! ");
      };
      $.ajax({type: "GET", url: document.location.pathname,
              dataType:"json",
              success: success, error: error});
  });