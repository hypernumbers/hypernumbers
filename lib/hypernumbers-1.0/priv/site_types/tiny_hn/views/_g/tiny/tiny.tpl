<div id='ventris'>

  <link href='/templates/tiny/default.css' rel='stylesheet' type='text/css'></link>

  <div id='outer'>
    
    <div data-binding-from='B2' class='hn header' data-type='text'></div>
    
    <div id='contentwrapper'>
      <div id='primarycontent'>
 
        <h2 style='color: rgb(255, 93, 0);'>Build the Web Applications you want <i>(no programming required)</i></h2>
        
        
        <form data-type='form' class='hn'>
          
          <h2>1. Pick a Site Type</h2>
          
          <div data-binding-from='/B8:B10' data-binding-to='/request_site/A:A' data-type='radio' class='hn'>

            <div style='overflow: auto;'>

              <div class='choice'>
                <label for='tiny'><img src='/img/tiny_poll.png'></img><br></br>
                  Tiny Poll</label>
                <input type='radio' id='tiny' value='Tiny Poll' name='rand_1260814241566'></input>
              </div>
              
              <div class='choice'>
                <label for='quiz'>
                  <img src='/img/questionnaire.png'></img><br></br>Customer Questionnaire</label>
                <input type='radio' id='quiz' value='Questionnaire'></input>
              </div>
              
              <div class='choice'>
                <label disabled='disabled' for='random'>
                  <img src='/img/comingsoon.png'></img><br></br>Coming Soon</label>
                <input type='radio' disabled='disabled' id='random' value='Night Out'></input>
              </div>

            </div>
          </div>
          
          <h2>2. Enter your email</h2>
          
          <div data-binding-to='/request_site/B:B' data-type='input' class='hn'></div>
          
          <input type='submit' value='3. Create Your Site' class='submit'></input>
        </form>

      </div>
      
      
      <div id='secondarycontent'>
        <div data-binding-from='/D7:D13' class='hn' data-type='text'></div></div>
    </div>
    
    
    <div id='footer'>  
      <div data-binding-from='B4' class='hn' data-type='text'></div>
    </div>
    
  </div>
</div>