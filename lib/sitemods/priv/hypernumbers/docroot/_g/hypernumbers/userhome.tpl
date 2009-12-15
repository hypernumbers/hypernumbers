<div id="ventris">
  
  <link rel="stylesheet" type="text/css" 
        href="/templates/ventris/default.css" />
  
  <div id="outer">
    
    <div id="inner">

	  <div class="clear"><div id="header">        
	    <div data-binding-from="/C2" class="hn header" data-type="text"></div>
	    
	    <div id="menu">
	      <div data-binding-from="/B6:G6" class="hn" data-type="text"></div>
        </div>
        
      </div></div>
	  
      <div id="contentwrapper">
        
	    <div id="primarycontent">

          <div data-binding-from="/u/B2" 
               class="hn header" data-type="text"></div>
          
          <br />
          <div id="newpageerror" style="visibility:hidden;">
            <strong>Error : </strong> The
            name can only \1 contain letters, numbers or underscores 
          </div>
          <form id="newpage"> 
            <input type="text" id="newpagename" />
            <input type="button" value="create new page" class="submit" />
          </form>

          <script>
            var is_valid_name = function(nm) {
              return nm !==  "" &&  nm.match(/^[0-9a-zA-Z_-]+$/);
            };

            var do_new = function(name) {
              var strip = name.split("/"), arr = [];
              for( var x = 0; x < strip.length; x++ ) {
                if ( is_valid_name(strip[x]) ) {
                  arr.push(strip[x]);
                }
              }

              if( arr.length == 0 ) {
                $("#newpageerror").css("visibility", "visible");
              } else {
               var url = document.location.pathname+arr.join("/")+"/";
               window.open(url);
               $("#newpagename").value = "";
             }
           };
                                  
            $("#newpage").submit( function(e) {
              e.preventDefault();
              do_new($("#newpagename").val());
              $("#newpagename").val("")
              return false;
            });
          </script>

        </div>
        
	    <div id="secondarycontent">
          
          <div data-binding-from="/u/B4" 
               class="hn header" data-type="text"></div>

          <div>
            <div data-binding-from="/u/B8:C6" class="hn" data-type="text">
              <div class="row">
                <div class="rowheader">
                  <span data-offset="1"></span>, <span data-offset="2"></span>
                </div>
                <span data-offset="3"></span>
              </div>
            </div>
          </div>
          
        </div>

      </div>
      
	  <div id="footer">
	    <div data-binding-from="/C3" class="hn" data-type="text"></div>
	  </div>
      
    </div>
  </div>
</div>
