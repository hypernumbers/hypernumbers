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

          <div data-binding-from="/B9" class="hn" data-type="text"></div>
          
          <form class="hn" data-type="form">
         
            <label>
              <span><div data-binding-from="/application/B2" class="hn" 
                         data-type="text"></div></span>
              <span class="hn" 
                    data-type="input" 
                    data-binding-to="/application/b:b"></span>
            </label>
            
            <label>
              <span><div data-binding-from="/application/B3" class="hn" 
                         data-type="text"></div></span>
              <span class="hn" 
                    data-type="input" 
                    data-binding-to="/application/c:c"></span>
            </label>

            <label>
              <span><div data-binding-from="/application/B4" class="hn" 
                         data-type="text"></div></span>
              <span class="hn" data-type="textarea" 
                    data-binding-to="/application/d:d"></span>
            </label>

            <input type="submit" value="Submit" />

          </form>
        </div>
        
	    <div id="secondarycontent">
          
          <div data-binding-from="/B11" 
               class="hn header" data-type="text"></div>

          <div id="news">
            <div data-binding-from="/B-1:D-3" class="hn" data-type="text">
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
