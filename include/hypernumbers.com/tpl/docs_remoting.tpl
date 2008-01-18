<div id="documentation" class="box padded clearfix">

    <h2>Remoting</h2><hr />
    <p>Hypernumbers runs a remoting socket connection that notifies changes to a websheet. 
        if you wish to be notified of real time changes then this is the prefered way 
        (as opposed to polling, which will work, but may consume resources)</p>    
    <br />
    <h3>Client Messages</h3>
   <table cellspacing="5px">
        <tr><td width="250px" valign="top">register <i>http://domain.com/path/<em>range</em></i></td>
            <td>This is used to notify the server that you wish to recieve updates to any cells 
                contained within this range</td></tr>
        <tr><td width="250px" valign="top">unregister <i>http://domain.com/path/<em>range</em></i></td>
            <td>This is used to notify the server that you dont want to recieve further notifications
                about cells within this range</td></tr>
    </table> 
    <br />
    <h3>Server Messages</h3>
   <table cellspacing="5px">
        <tr><td width="250px" valign="top">change <i>http://domain.com/path/<em>cell</em> value</i></td>
            <td>This is sent by the server to notify the client that a cell has changed, it includes the 
                current value of the cell</td></tr>
    </table>      

</div>