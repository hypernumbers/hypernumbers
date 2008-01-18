<div id="documentation" class="box padded clearfix">

    <h2>Writing Functions</h2><hr />

    <p>
        There is a module called: <i>userdef.erl</i> which can be found in 
        <i>/lib/spriki/src</i> in the root hypernumbers directory

        An example function (userdefined:hello_world) is included. You can edit 
        this function.

        Any function exported by userdef.erl will be available to the user.</p>

        <h3>Example</h3>
        <p>Type in this function:</p>
        <code>hello_universe([])->“and a big hello to you, love the universe”.</code>

        <p>Edit the export line to read:</p>
        <code>-export([hello_world/1,hello_universe/1]).</code>

        <p>You can compile this file with the script <i>./compile_userdef</i> 
        and then enter the following formula into a cell in a spreadsheet</p>
        <code>=hello_universe()</code>
</div>