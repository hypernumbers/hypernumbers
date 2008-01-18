<div id="documentation" class="box padded clearfix">

    <h2>Installing And Running the Server</h2><hr />

    <p>These instructions refer to the latest alpha release (.tar.gz)
        that can be downloaded <a href="/alpha/">here</a>, or for developers 
        who want to receieve more regular updates, an SVN repository is available 
        <a href="/docs/svn">here</a>.</p>

    <br />
    <h3>Install on linux or mac</h3>
    <p>Extract the tar.gz to any writeable directory, then run the 
        ./start_hypernumbers script from a bash shell. For example :</p>
        <code>
            tar -xzf hypernumbers.tar.gz -C hypernumbers<br />
            cd hypernumbers<br />
            ./start_hypernumbers<br/>
        </code>

    <br /><br />
    <h3>Install on Windows</h3>
    <p>Its easier on windows to download the package to c:/, extract the package 
    to the current directory (first try right click and look for "Extract here", this may 
    require <a href="http://www.rarlab.com/">winrar</a>), then open the command prompt 
    and run the start_win.bat script. For example :</p>
        <code>
            cd c:/hypernumbers<br />
            ./start_win.bat<br/>
        </code>

    <br /><br />
    <h3>Testing the Installation</h3>
    <p>The server will start a hypernumbers websheet running at <a href="http://127.0.0.1:9000">http://127.0.0.1:9000</a>
    if you visit that address, you should some something that looks like.</p>
    <div style="text-align:center;margin:15px 0px;">
    <img src="/img/screens/1.png" alt="hypernumbers screenshot" />
    </div>

    <p>If the page says unable to connect, make sure there are no other servers running on that ip / port, 
        alternatively you can configure the ip / port that hypernumbers runs on, read further at 
        <a href="/docs/config">Configuring the Hypernumbers Server</a>
    </p>

    <p>If the page attempts to load but looks nothing like the above image, feel free to contact us below, please report
        your operating system and browser details</p>

</div>