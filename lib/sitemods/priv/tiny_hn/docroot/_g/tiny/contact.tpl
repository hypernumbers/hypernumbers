<div id='ventris'>

  <link type='text/css' rel='stylesheet'
        href='/templates/tiny/default.css'></link>

  <div id='outer'>
    
    <div data-type='text' class='hn header' data-binding-from='B2'></div>
    
    <div id='contentwrapper'>
      <div id='primarycontent'>
 
        <div data-type='text' class='hn' data-binding-from='B3'></div>
        
        <form class='hn' data-type='form'>
          
          <h2>Pick a site</h2>
          
          <div class='hn' data-type='radio' 
               data-binding-to='/request_site/A:A'>

            <div style="overflow:auto;">

              <div class="choice">
                <label for="tiny"><img src="/img/tiny.png" /><br />
                  Tiny Poll</label>
                <input type="radio" name="rand_1260814241566" value="Tiny Poll"
                       id="tiny" />
              </div>
              
              <div class="choice">
                <label for="quiz" disabled="disabled">
                  <img src="/img/comingsoon.png" /><br />Coming Soon</label>
                <input type="radio" value="Tiny Quiz"
                       id="quiz" disabled="disabled" />
              </div>
              
              <div class="choice">
                <label for="random" disabled="disabled">
                  <img src="/img/comingsoon.png" /><br />Coming Soon</label>
                <input type="radio" value="Night Out" id="random" 
                       disabled="disabled" />
              </div>

            </div>
          </div>
          
          <h2>Enter your email</h2>
          
          <div class='hn' data-type='input'
               data-binding-to='/request_site/B:B'></div>
          
          <input type='submit' class='submit' value='Create Your Site'></input>
        </form>

      </div>
      
      
      <div id='secondarycontent'>
        <div data-type='text' class='hn' data-binding-from='D-1:D-3'></div></div>
    </div>
    
    
    <div id='footer'>  
      <div data-type='text' class='hn' data-binding-from='B4'></div>
    </div>
    
  </div>
</div>
