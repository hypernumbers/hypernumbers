<div id='ventris'>
  
  <link href='/templates/ventris/default.css' type='text/css' rel='stylesheet'></link>
  
  <div id='outer'>
    
    <div id='inner'>
    
	  <div class='clear'><div id='header'>        
	    <div data-type='text' class='hn header' data-binding-from='/C2'></div>
	    
	    <div id='menu'>
	      <div data-type='text' class='hn' data-binding-from='/B6:G6'></div>
        </div>
        
      </div></div>
	  
      <div id='contentwrapper'>
        
	    <div id='primarycontent'>

          <div data-type='text' class='hn' data-binding-from='/B9'></div>
          
          <form data-type='form' class='hn'>
         
            <label>
              <span><div data-type='text' class='hn' data-binding-from='/application/B2'></div></span>
              <span data-binding-to='/application/b:b' data-type='input' class='hn'></span>
            </label>
            
            <label>
              <span><div data-type='text' class='hn' data-binding-from='/application/B3'></div></span>
              <span data-binding-to='/application/c:c' data-type='input' class='hn'></span>
            </label>

            <label>
              <span><div data-type='text' class='hn' data-binding-from='/application/B4'></div></span>
              <span data-binding-to='/application/d:d' data-type='textarea' class='hn'></span>
            </label>

            <input type='submit' value='Submit'></input>

          </form>
        </div>
        
	    <div id='secondarycontent'>
          
          <div data-type='text' class='hn header' data-binding-from='/B11'></div>

          <div id='news'>
            <div data-type='text' class='hn' data-binding-from='/B-1:D-3'>
              <div class='tpl'>
                <div class='rowheader'>
                  <span data-offset='1'></span>, <span data-offset='2'></span>
                </div>
                <span data-offset='3'></span>
              </div>
            </div>
          </div>
          
        </div>

      </div>
      
	  <div id='footer'>
	    <div data-type='text' class='hn' data-binding-from='/C3'></div>
	  </div>
      
    </div>
  </div>
</div>
