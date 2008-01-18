<div id="documentation" class="box padded clearfix">

    <h2>API</h2><hr />

    <p>The HyperNumbers API is a simple REST interface to allow users to create 
        and edit a websheet</p>

    <p>A Request to the HyperNumbers API is done with a GET or POST request to the 
        specified resouce, the general format of a request is</p>

    <code>protocol://sub.domain.tld:port/path/ref[?getvar|&getvar=value]</code>
    <p>for example http://example.org/path/a1?incoming&format=xml</p>

    <p>Pages are addressed with a trailing slash, if the last character before ? is not a slash, the
     text before is parsed as a reference, a reference either addresses a cell, row, column, or a range.</p>

    <p>The URL relating to a reference will returns its value, to set the value of a reference, you make a POST 
    request to that address, Data can be POSTed and recieved in various formats, use the GET parameter
    ?format=[xml|list|json] to specify which, a newline delimited list is default. Follow the 
    links to view examples of editing a reference or view this [[API Reference Table]]</p>
    
    <table border="1" width="70%" cellpadding="15" align="center">
        
        <tr><th>Address</th><th>Ref Type</th><th>API Spec</th></tr>
        <tr><td>/page/</td><td>page</td><td>Page API</td></tr>
        <tr><td>/page/a1</td><td>cell</td><td>Cell API</td></tr>
        <tr><td>/page/a</td><td>row</td><td>Row API</td></tr>
        <tr><td>/page/1</td><td>column</td><td>Column API</td></tr>
        <tr><td>/page/a1:b2</td><td>range</td><td>Range API</td></tr>
    
    </table>
    
    <p>Below is a list of queries that can be used for more information about a reference</p>
    
    <ul>
        <li>?toolbar</li>
        <li>?incoming</li>
        <li>?outgoing</li>
    </ul>


</div>