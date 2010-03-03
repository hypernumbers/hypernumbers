-include_lib("eunit/include/eunit.hrl").

unit_test_() ->  
    [?_assertEqual(        
        "<p><img src=\"_underscore\" alt=\"login\" title=\"\" /></p>",
        conv("![login](_underscore)")),
     ?_assertEqual(
        "<p><a href=\"_underscore\">login</a></p>",
        conv("[login](_underscore)")),
     ?_assertEqual(
        "<pre><code>alice\nbob\n</code></pre>\n\n<p>chaz</p>",
       conv("    alice\n    bob\nchaz")),
     ?_assertEqual(
        conv("> alice\n> bob\n> chaz"),
        "<blockquote>\n  <p>alice\n  bob\n  chaz</p>\n</blockquote>"),
     ?_assertEqual(
        conv(" - a\n - b\n"),
        "<ul>\n<li>a</li>\n<li>b</li>\n</ul>"),
     ?_assertEqual(
        conv(" 1. a\n 2. b\n"),
        "<ol>\n<li>a</li>\n<li>b</li>\n</ol>"),
     ?_assertEqual(
        conv("blah ![blah][1] blah\n\n\n  [1]: http://example.com (title)"),
        "<p>blah <img src=\"http://example.com\" alt=\"blah\" title=\"title\" "
        "/> blah</p>"),
     ?_assertEqual(
        conv("blah ![blah][1] blah\n\n\n  [1]: http://example.com \"title\""),
        "<p>blah <img src=\"http://example.com\" alt=\"blah\" title=\"title\" "
        "/> blah</p>"),
     ?_assertEqual(
        conv("blah ![blah][1] blah\n\n\n  [1]: http://example.com"),
        "<p>blah <img src=\"http://example.com\" alt=\"blah\" title=\"\" /> "
        "blah</p>"),
     ?_assertEqual(
        conv("Now is the winter of `our discontent` made glorious summer by "
             "this Son of York"),
        "<p>Now is the winter of <code>our discontent</code> made glorious "
        "summer by this Son of York</p>"),
     ?_assertEqual(
        conv("blah [blah][1] blah\n\n\n  [1]: http://example.com (title)"),
        "<p>blah <a href=\"http://example.com\" title=\"title\">blah</a> "
        "blah</p>"),
     ?_assertEqual(
        conv("blah [blah][1] blah\n\n\n  [1]: http://example.com \"title\""),
        "<p>blah <a href=\"http://example.com\" title=\"title\">blah</a> "
        "blah</p>"),
     ?_assertEqual(
        conv("blah [blah][1] blah\n\n\n  [1]: http://example.com"),
        "<p>blah <a href=\"http://example.com\">blah</a> blah</p>"),
     ?_assertEqual(conv("> blah\n-------------"),"<h2>> blah</h2>"),
     ?_assertEqual(conv("  \n"),""),
     ?_assertEqual(
        conv("![alt text][1]\n\n[1]: http://example.com"),
        "<p><img src=\"http://example.com\" alt=\"alt text\" title=\"\" "
        "/></p>"),
     ?_assertEqual(
        conv("![alt text][1]\n\n[1]: http://example.com \"optional title\""),
        "<p><img src=\"http://example.com\" alt=\"alt text\" title=\"optional "
        "title\" /></p>"),
     ?_assertEqual(
        conv("[link text][1]\n\n[1]: http://example.com"),
        "<p><a href=\"http://example.com\">link text</a></p>"),
     ?_assertEqual(
        conv("[link text][1]\n\n[1]: http://example.com \"optional title\""),
        "<p><a href=\"http://example.com\" title=\"optional title\">link te"
        "xt</a></p>"),
     ?_assertEqual(conv("blah\n\n\nbleh"),"<p>blah</p>\n\n<p>bleh</p>"),
     ?_assertEqual(
        conv("`<div>blah</div>`"),
        "<p><code>&lt;div&gt;blah&lt;/div&gt;</code></p>"),
     ?_assertEqual(
        conv("My [link][id] test\n[id]: http://example.com"),
        "<p>My <a href=\"http://example.com\">link</a> test</p>"),
     ?_assertEqual(
        conv("My [id] test\n[id]: http://example.com"),
        "<p>My <a href=\"http://example.com\">id</a> test</p>"),
     ?_assertEqual(
        conv("blah <https://something.com:1234/a/path> blah\na"),
        "<p>blah <a href=\"https://something.com:1234/a/path\">https://some"
        "thing.com:1234/a/path</a> blah\na</p>"),
     ?_assertEqual(
        conv("blah <http://something.com:1234/a/path> blah\na"),
        "<p>blah <a href=\"http://something.com:1234/a/path\">http://some"
        "thing.com:1234/a/path</a> blah\na</p>"),
     ?_assertEqual(
        conv("[id]: /a/path\nSome text ![hey] [id] there\na"),
        "<p>Some text <img src=\"/a/path\" alt=\"hey\" title=\"\" /> "
        "there\na</p>"),
     ?_assertEqual(
        conv("[id]: /a/path\nSome text ![hey][id] there\na"),
        "<p>Some text <img src=\"/a/path\" alt=\"hey\" title=\"\" /> "
        "there\na</p>"),
     ?_assertEqual(
        conv("[id]:   \t \t   /a/path\nSome text [hey][id] there\na"),
        "<p>Some text <a href=\"/a/path\">hey</a> there\na</p>"),
     ?_assertEqual(
        conv("[id]: /a/path\nSome text \\[id] there\na"),
        "<p>Some text [id] there\na</p>"),
     ?_assertEqual(
        conv("[id]: /a/path\nSome text [hey] [id] there\na"),
        "<p>Some text <a href=\"/a/path\">hey</a> there\na</p>"),
     ?_assertEqual(
        conv("[id]: /a/path\nSome text [hey][id] there\na"),
        "<p>Some text <a href=\"/a/path\">hey</a> there\na</p>"),
     ?_assertEqual(
        conv("an ![](path/jpg.jpg ) image\na"),
        "<p>an <img src=\"path/jpg.jpg\" alt=\"\" title=\"\" /> image\na</p>"),
     ?_assertEqual(
        conv("an ![Alt](path/jpg.jpg ) image\na"),
        "<p>an <img src=\"path/jpg.jpg\" alt=\"Alt\" title=\"\" /> "
        "image\na</p>"),
     ?_assertEqual(
        conv("an ![Alt](path/jpg.jpg 'Title') image\na"),
        "<p>an <img src=\"path/jpg.jpg\" alt=\"Alt\" title=\"Title\" /> "
        "image\na</p>"),
     ?_assertEqual(
        conv("an ![Alt](path/jpg.jpg \"Title\") image\na"),
        "<p>an <img src=\"path/jpg.jpg\" alt=\"Alt\" title=\"Title\" /> "
        "image\na</p>"),
     ?_assertEqual(
        conv("an ![Alt] (path/jpg.jpg \"Title\") image\na"),
        "<p>an <img src=\"path/jpg.jpg\" alt=\"Alt\" title=\"Title\" /> "
        "image\na</p>"),
     ?_assertEqual(
        conv("An [example](http://example.com/) of link\na"),
        "<p>An <a href=\"http://example.com/\">example</a> of link\na</p>"),
     ?_assertEqual(
        conv("An [](http://example.com/ \"Title\") of link\na"),
        "<p>An <a href=\"http://example.com/\" title=\"Title\"></a> "
        "of link\na</p>"),
     ?_assertEqual(
        conv("An [example](http://example.com/ 'Title') of link\na"),
        "<p>An <a href=\"http://example.com/\" title=\"Title\">example</a> "
        "of link\na</p>"),
     ?_assertEqual(
        conv("An [example](http://example.com/ \"Title\") of link\na"),
        "<p>An <a href=\"http://example.com/\" title=\"Title\">example</a> "
        "of link\na</p>"),
     ?_assertEqual(
        conv("An [example] (http://example.com/ \"Title\") of link\na"),
        "<p>An [example] (http://example.com/ \"Title\") of link\na</p>"),
     ?_assertEqual(
        conv("a\na\n   [id]: /example.com"),
        "<p>a\na</p>"),
     ?_assertEqual(
        conv("a\na\n   [id]: http://example.com"),
        "<p>a\na</p>"),
     ?_assertEqual(
        conv("a\na\n[id]: <http://example.com>"),
        "<p>a\na</p>"),
     ?_assertEqual(
        conv("a\na\n[id]: http://example.com"),
        "<p>a\na</p>"),
     ?_assertEqual(
        conv("a\na\n[id]: http://example.com (Title)"),
        "<p>a\na</p>"),
     ?_assertEqual(
        conv("a\na\n[id]: http://example.com \"Title\""),
        "<p>a\na</p>"),
     ?_assertEqual(
        conv("___blah\na"),
        "<p><em>_</em>blah\na</p>"),
     ?_assertEqual(
        conv("---blah\na"),
        "<p>---blah\na</p>"),
     ?_assertEqual(conv("***blah\na"),"<p><em>*</em>blah\na</p>"),
     ?_assertEqual(conv("_ _ _"),"<hr />"),
     ?_assertEqual(conv("- - -"),"<hr />"),
     ?_assertEqual(conv("* * *"),"<hr />"),
     ?_assertEqual(conv("_____"),"<hr />"),
     ?_assertEqual(conv("-----"),"<hr />"),
     ?_assertEqual(conv("*****"),"<hr />"),
     ?_assertEqual(conv("___"),"<hr />"),
     ?_assertEqual(conv("---"),"<hr />"),
     ?_assertEqual(conv("***"),"<hr />"),
     ?_assertEqual(
        conv("\t<div>&"),
        "<pre><code>&lt;div&gt;&amp;\n</code></pre>"),
     ?_assertEqual(conv("\t<div>"),"<pre><code>&lt;div&gt;\n</code></pre>"),
     ?_assertEqual(conv("+ a\n\t\tb"),"<ul>\n<li>a\n    b</li>\n</ul>"),
     ?_assertEqual(conv("+ a\n  \t  b"),"<ul>\n<li>a\n    b</li>\n</ul>"),
     ?_assertEqual(conv("+ a\n\t    b"),"<ul>\n<li>a\n    b</li>\n</ul>"),
     ?_assertEqual(
        conv("\tb\n\n\n\n\nc"),
        "<pre><code>b\n</code></pre>\n\n<p>c</p>"),
     ?_assertEqual(
        conv("\tb\n\n\n\nc"),
        "<pre><code>b\n</code></pre>\n\n<p>c</p>"),
     ?_assertEqual(
        conv("\tb\n\n\nc"),
        "<pre><code>b\n</code></pre>\n\n<p>c</p>"),
     ?_assertEqual(conv("\tb\n\nc"),"<pre><code>b\n</code></pre>\n\n<p>c</p>"),
     ?_assertEqual(conv("\tb\nc"),"<pre><code>b\n</code></pre>\n\n<p>c</p>"),
     ?_assertEqual(conv("\tb"),"<pre><code>b\n</code></pre>"),
     ?_assertEqual(conv("    b"),"<pre><code>b\n</code></pre>"),
     ?_assertEqual(
        conv("4. a\n\n5. b\n\n6. c"),
        "<ol>\n<li><p>a</p></li>\n<li><p>b</p></li>\n<li><p>c</p></li>\n</ol>"),
     ?_assertEqual(
        conv("4. a\n5. b\n6. c"),
        "<ol>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n</ol>"),
     ?_assertEqual(conv("4. blah\nblah"),"<ol>\n<li>blah\nblah</li>\n</ol>"),
     ?_assertEqual(conv("555.blah\na"),"<p>555.blah\na</p>"),
     ?_assertEqual(conv("555 @blah\na"),"<p>555 @blah\na</p>"),
     ?_assertEqual(conv("555. blah"),"<ol>\n<li>blah</li>\n</ol>"),
     ?_assertEqual(conv("4. blah"),"<ol>\n<li>blah</li>\n</ol>"),
     ?_assertEqual(conv("1. blah"),"<ol>\n<li>blah</li>\n</ol>"),
     ?_assertEqual(conv("- blah\nblah"),"<ul>\n<li>blah\nblah</li>\n</ul>"),
     ?_assertEqual(
        conv("- a\n\n+ b\n\n+ c\n* d"),
        "<ul>\n<li><p>a</p></li>\n<li><p>b</p></li>\n<li><p>c</p></li>"
        "\n<li>d</li>\n</ul>"),
     ?_assertEqual(
        conv("- a\n\n+ b"),
        "<ul>\n<li><p>a</p></li>\n<li><p>b</p></li>\n</ul>"),
     ?_assertEqual(
        conv("- a\n\n \n\t\n XX+ b"),
        "<ul>\n<li><p>a</p>\n\n<p>XX+ b</p></li>\n</ul>"),
     ?_assertEqual(
        conv("- a\n \n \n\t\n XX+ b"),
        "<ul>\n<li><p>a</p>\n\n<p>XX+ b</p></li>\n</ul>"),
     ?_assertEqual(
        conv("- a\n\n\n\n XX+ b"),
        "<ul>\n<li><p>a</p>\n\n<p>XX+ b</p></li>\n</ul>"),
     ?_assertEqual(
        conv("- a\n\n\n\nXX+ b"),
        "<ul>\n<li>a</li>\n</ul>\n\n<p>XX+ b</p>"),
     ?_assertEqual(
        conv("- a\n+ b\n- c"),
        "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n</ul>"),
     ?_assertEqual(conv("-blah\na"),"<p>-blah\na</p>"),
     ?_assertEqual(conv("- blah"),"<ul>\n<li>blah</li>\n</ul>"),
     ?_assertEqual(conv("*blah\na"),"<p>*blah\na</p>"),
     ?_assertEqual(conv("* blah"),"<ul>\n<li>blah</li>\n</ul>"),
     ?_assertEqual(conv("+blah\na"),"<p>+blah\na</p>"),
     ?_assertEqual(conv("+ blah"),"<ul>\n<li>blah</li>\n</ul>"),
     ?_assertEqual(
        conv("bleh\n> blah"),
        "<p>bleh</p>\n\n<blockquote>\n  <p>blah</p>\n</blockquote>"),
     ?_assertEqual(
        conv("> blah\na"),
        "<blockquote>\n  <p>blah\n  a</p>\n</blockquote>"),
     ?_assertEqual(
        conv("# blahblah ###\nbleh"),"<h1>blahblah</h1>\n\n<p>bleh</p>"),
     ?_assertEqual(
        conv("####### blahblah\nbleh"),"<h6># blahblah</h6>\n\n<p>bleh</p>"),
     ?_assertEqual(
        conv("###### blahblah\nbleh"),"<h6>blahblah</h6>\n\n<p>bleh</p>"),
     ?_assertEqual(
        conv("##### blahblah\nbleh"),"<h5>blahblah</h5>\n\n<p>bleh</p>"),
     ?_assertEqual(
        conv("#### blahblah\nbleh"),"<h4>blahblah</h4>\n\n<p>bleh</p>"),
     ?_assertEqual(
        conv("### blahblah\nbleh"),"<h3>blahblah</h3>\n\n<p>bleh</p>"),
     ?_assertEqual(
        conv("## blahblah\nbleh"),"<h2>blahblah</h2>\n\n<p>bleh</p>"),
     ?_assertEqual(conv("# blahblah\nbleh"),"<h1>blahblah</h1>\n\n<p>bleh</p>"),
     ?_assertEqual(conv("# blahblah ###"),"<h1>blahblah</h1>"),
     ?_assertEqual(conv("####### blahblah"),"<h6># blahblah</h6>"),
     ?_assertEqual(conv("###### blahblah"),"<h6>blahblah</h6>"),
     ?_assertEqual(conv("##### blahblah"),"<h5>blahblah</h5>"),
     ?_assertEqual(conv("#### blahblah"),"<h4>blahblah</h4>"),
     ?_assertEqual(conv("### blahblah"),"<h3>blahblah</h3>"),
     ?_assertEqual(conv("## blahblah"),"<h2>blahblah</h2>"),
     ?_assertEqual(conv("# blahblah"),"<h1>blahblah</h1>"),
     ?_assertEqual(conv("> a\n-"),"<h2>> a</h2>"),
     ?_assertEqual(conv("> a\n="),"<h1>> a</h1>"),
     ?_assertEqual(
        conv("blahblah\n-----\nblah"),
        "<h2>blahblah</h2>\n\n<p>blah</p>"),
     ?_assertEqual(
        conv("blahblah\n====\nblah"),
        "<h1>blahblah</h1>\n\n<p>blah</p>"),
     ?_assertEqual(conv("blahblah\n-----"),"<h2>blahblah</h2>"),
     ?_assertEqual(conv("blahblah\n===="),"<h1>blahblah</h1>"),
     ?_assertEqual("<p>blah\nblah</p>", conv("blah\r\nblah\n")),
     ?_assertEqual("<p>blah\nblah</p>", conv("blah\r\nblah")),
     ?_assertEqual("<p>blah\nblah</p>", conv("blah\nblah")),
     ?_assertEqual(
        conv("___you___ sad bastard\na"),
        "<p><strong><em>you</em></strong> sad bastard\na</p>"),
     ?_assertEqual(
        conv("__you__ sad bastard\na"),
        "<p><strong>you</strong> sad bastard\na</p>"),
     ?_assertEqual(
        conv("_you_ sad bastard\na"),
        "<p><em>you</em> sad bastard\na</p>"),
     ?_assertEqual(
        conv("***you*** sad bastard\na"),
        "<p><strong><em>you</em></strong> sad bastard\na</p>"),
     ?_assertEqual(
        conv("**you** sad bastard\na"),
        "<p><strong>you</strong> sad bastard\na</p>"),
     ?_assertEqual(
        conv("*you* sad bastard\na"),
        "<p><em>you</em> sad bastard\na</p>"),
     ?_assertEqual(
        conv("you \\_sad\\_ bastard\na"),
        "<p>you _sad_ bastard\na</p>"),
     ?_assertEqual(
        conv("you \\*sad\\* bastard\na"),
        "<p>you *sad* bastard\na</p>"),
     ?_assertEqual(
        conv("you_sad_bastard\na"),
        "<p>you<em>sad</em>bastard\na</p>"),
     ?_assertEqual(
        conv("you*sad*bastard\na"),
        "<p>you<em>sad</em>bastard\na</p>"),
     ?_assertEqual(
        conv("you ___sad___ bastard\na"),
        "<p>you <strong><em>sad</em></strong> bastard\na</p>"),
     ?_assertEqual(
        conv("you __sad__ bastard\na"),
        "<p>you <strong>sad</strong> bastard\na</p>"),
     ?_assertEqual(
        conv("you _sad_ bastard\na"),
        "<p>you <em>sad</em> bastard\na</p>"),
     ?_assertEqual(
        conv("you ***sad*** bastard\na"),
        "<p>you <strong><em>sad</em></strong> bastard\na</p>"),
     ?_assertEqual(
        conv("you **sad** bastard\na"),
        "<p>you <strong>sad</strong> bastard\na</p>"),
     ?_assertEqual(
        conv("you *sad* bastard\na"),
        "<p>you <em>sad</em> bastard\na</p>"),
     ?_assertEqual(conv("########\na"),"<h6>#</h6>\n\n<p>a</p>"),
     ?_assertEqual(conv("#######\na"),"<h6>#</h6>\n\n<p>a</p>"),
     ?_assertEqual(conv("######\na"),"<h5>#</h5>\n\n<p>a</p>"),
     ?_assertEqual(conv("#####\na"),"<h4>#</h4>\n\n<p>a</p>"),
     ?_assertEqual(conv("####\na"),"<h3>#</h3>\n\n<p>a</p>"),
     ?_assertEqual(conv("###\na"),"<h2>#</h2>\n\n<p>a</p>"),
     ?_assertEqual(conv("##\na"),"<h1>#</h1>\n\n<p>a</p>"),
     ?_assertEqual(conv("#\na"),"<p>#\na</p>"),
     ?_assertEqual(conv("\n[\na"),"<p>[\na</p>"),
     ?_assertEqual(conv("\n>\na"),"<p>>\na</p>"),
     ?_assertEqual(conv("\n-\na"),"<p>-\na</p>"),
     ?_assertEqual(conv("\n=\na"),"<p>=\na</p>"),
     ?_assertEqual(conv("[\na"),"<p>[\na</p>"),
     ?_assertEqual(conv(">\na"),"<p>>\na</p>"),
     ?_assertEqual(conv("-\na"),"<p>-\na</p>"),
     ?_assertEqual(conv("=\na"),"<p>=\na</p>"),
     ?_assertEqual(conv("abc\\`def\na"),"<p>abc`def\na</p>"),
     ?_assertEqual(conv("xyz\r\nab:c\na"),"<p>xyz\nab:c\na</p>"),
     ?_assertEqual(conv("xyz\nab:c\na"),"<p>xyz\nab:c\na</p>"),
     ?_assertEqual(conv("xyz\tab:c\na"),"<p>xyz    ab:c\na</p>"),
     ?_assertEqual(conv("xyz ab:c\na"),"<p>xyz ab:c\na</p>"),
     ?_assertEqual(conv("xyz(ab:c\na"),"<p>xyz(ab:c\na</p>"),
     ?_assertEqual(conv("xyz]ab:c\na"),"<p>xyz]ab:c\na</p>"),
     ?_assertEqual(conv("xyz[ab:c\na"),"<p>xyz[ab:c\na</p>"),
     ?_assertEqual(conv("xyz)ab:c\na"),"<p>xyz)ab:c\na</p>"),
     ?_assertEqual(conv("xyz(ab:c\na"),"<p>xyz(ab:c\na</p>"),
     ?_assertEqual(conv("xyz/ab:c\na"),"<p>xyz/ab:c\na</p>"),
     ?_assertEqual(conv("xyz\\ab:c\na"),"<p>xyz\\ab:c\na</p>"),
     ?_assertEqual(conv("xyz!ab:c\na"),"<p>xyz!ab:c\na</p>"),
     ?_assertEqual(conv("xyz`ab:c\na"),"<p>xyz`ab:c\na</p>"),
     ?_assertEqual(conv("xyz\"ab:c\na"),"<p>xyz\"ab:c\na</p>"),
     ?_assertEqual(conv("xyz'ab:c\na"),"<p>xyz'ab:c\na</p>"),
     ?_assertEqual(conv("xyz:ab:c\na"),"<p>xyz:ab:c\na</p>"),
     ?_assertEqual(conv("xyz.ab:c\na"),"<p>xyz.ab:c\na</p>"),
     ?_assertEqual(conv("xyz0ab:c\na"),"<p>xyz0ab:c\na</p>"),
     ?_assertEqual(conv("xyz9ab:c\na"),"<p>xyz9ab:c\na</p>"),
     ?_assertEqual(conv("xyz8ab:c\na"),"<p>xyz8ab:c\na</p>"),
     ?_assertEqual(conv("xyz7ab:c\na"),"<p>xyz7ab:c\na</p>"),
     ?_assertEqual(conv("xyz6ab:c\na"),"<p>xyz6ab:c\na</p>"),
     ?_assertEqual(conv("xyz5ab:c\na"),"<p>xyz5ab:c\na</p>"),
     ?_assertEqual(conv("xyz4ab:c\na"),"<p>xyz4ab:c\na</p>"),
     ?_assertEqual(conv("xyz3ab:c\na"),"<p>xyz3ab:c\na</p>"),
     ?_assertEqual(conv("xyz2ab:c\na"),"<p>xyz2ab:c\na</p>"),
     ?_assertEqual(conv("xyz1ab:c\na"),"<p>xyz1ab:c\na</p>"),
     ?_assertEqual(conv("xyz_ab:c\na"),"<p>xyz_ab:c\na</p>"),
     ?_assertEqual(conv("xyz*ab:c\na"),"<p>xyz*ab:c\na</p>"),
     ?_assertEqual(conv("xyz+ab:c\na"),"<p>xyz+ab:c\na</p>"),
     ?_assertEqual(conv("xyz>ab:c\na"),"<p>xyz>ab:c\na</p>"),
     ?_assertEqual(conv("xyz#ab:c\na"),"<p>xyz#ab:c\na</p>"),
     ?_assertEqual(conv("xyz-ab:c\na"),"<p>xyz-ab:c\na</p>"),
     ?_assertEqual(conv("xyz=ab:c\na"),"<p>xyz=ab:c\na</p>"),
     ?_assertEqual(conv("xyz/ab:c\na"),"<p>xyz/ab:c\na</p>"),
     ?_assertEqual(conv("\r\n ab:c\na"),"<p>ab:c\na</p>"),
     ?_assertEqual(conv("\n ab:c\na"),"<p>ab:c\na</p>"),
     ?_assertEqual(
        conv("\t ab:c\na"),
        "<pre><code> ab:c\n</code></pre>\n\n<p>a</p>"),
     ?_assertEqual(conv("  ab:c\na"),"<p>ab:c\na</p>"),
     ?_assertEqual(conv("( ab:c\na"),"<p>( ab:c\na</p>"),
     ?_assertEqual(conv("] ab:c\na"),"<p>] ab:c\na</p>"),
     ?_assertEqual(conv("[ ab:c\na"),"<p>[ ab:c\na</p>"),
     ?_assertEqual(conv(") ab:c\na"),"<p>) ab:c\na</p>"),
     ?_assertEqual(conv("( ab:c\na"),"<p>( ab:c\na</p>"),
     ?_assertEqual(conv("/ ab:c\na"),"<p>/ ab:c\na</p>"),
     ?_assertEqual(conv("\\ ab:c\na"),"<p>\\ ab:c\na</p>"),
     ?_assertEqual(conv("! ab:c\na"),"<p>! ab:c\na</p>"),
     ?_assertEqual(conv("` ab:c\na"),"<p>` ab:c\na</p>"),
     ?_assertEqual(conv("\" ab:c\na"),"<p>\" ab:c\na</p>"),
     ?_assertEqual(conv("' ab:c\na"),"<p>' ab:c\na</p>"),
     ?_assertEqual(conv(": ab:c\na"),"<p>: ab:c\na</p>"),
     ?_assertEqual(conv(". ab:c\na"),"<p>. ab:c\na</p>"),
     ?_assertEqual(conv("0 ab:c\na"),"<p>0 ab:c\na</p>"),
     ?_assertEqual(conv("9 ab:c\na"),"<p>9 ab:c\na</p>"),
     ?_assertEqual(conv("8 ab:c\na"),"<p>8 ab:c\na</p>"),
     ?_assertEqual(conv("7 ab:c\na"),"<p>7 ab:c\na</p>"),
     ?_assertEqual(conv("6 ab:c\na"),"<p>6 ab:c\na</p>"),
     ?_assertEqual(conv("5 ab:c\na"),"<p>5 ab:c\na</p>"),
     ?_assertEqual(conv("4 ab:c\na"),"<p>4 ab:c\na</p>"),
     ?_assertEqual(conv("3 ab:c\na"),"<p>3 ab:c\na</p>"),
     ?_assertEqual(conv("2 ab:c\na"),"<p>2 ab:c\na</p>"),
     ?_assertEqual(conv("1 ab:c\na"),"<p>1 ab:c\na</p>"),
     ?_assertEqual(conv("_ ab:c\na"),"<p>_ ab:c\na</p>"),
     ?_assertEqual(conv("* ab:c\na"),"<ul>\n<li>ab:c\na</li>\n</ul>"),
     ?_assertEqual(conv("+ ab:c\na"),"<ul>\n<li>ab:c\na</li>\n</ul>"),
     ?_assertEqual(
        conv("> ab:c\na"),
        "<blockquote>\n  <p>ab:c\n  a</p>\n</blockquote>"),
     ?_assertEqual(conv("# ab:c\na"),"<h1>ab:c</h1>\n\n<p>a</p>"),
     ?_assertEqual(conv("- ab:c\na"),"<ul>\n<li>ab:c\na</li>\n</ul>"),
     ?_assertEqual(conv("= ab:c\na"),"<p>= ab:c\na</p>"),
     ?_assertEqual(conv("/ ab:c\na"),"<p>/ ab:c\na</p>"),
     ?_assertEqual(conv("< /ab:c\na"),"<p>&lt; /ab:c\na</p>"),
     ?_assertEqual(conv("< ab:c\na"),"<p>&lt; ab:c\na</p>"),
     ?_assertEqual(conv("\r\nab:c\na"),"<p>ab:c\na</p>"),
     ?_assertEqual(conv("\nab:c\na"),"<p>ab:c\na</p>"),
     ?_assertEqual(
        conv("\tab:c\na"),
        "<pre><code>ab:c\n</code></pre>\n\n<p>a</p>"),
     ?_assertEqual(conv(" ab:c\na"),"<p>ab:c\na</p>"),
     ?_assertEqual(conv("(ab:c\na"),"<p>(ab:c\na</p>"),
     ?_assertEqual(conv("]ab:c\na"),"<p>]ab:c\na</p>"),
     ?_assertEqual(conv("[ab:c\na"),"<p>[ab:c\na</p>"),
     ?_assertEqual(conv(")ab:c\na"),"<p>)ab:c\na</p>"),
     ?_assertEqual(conv("(ab:c\na"),"<p>(ab:c\na</p>"),
     ?_assertEqual(conv("/ab:c\na"),"<p>/ab:c\na</p>"),
     ?_assertEqual(conv("\\ab:c\na"),"<p>\\ab:c\na</p>"),
     ?_assertEqual(conv("!ab:c\na"),"<p>!ab:c\na</p>"),
     ?_assertEqual(conv("`ab:c\na"),"<p>`ab:c\na</p>"),
     ?_assertEqual(conv("\"ab:c\na"),"<p>\"ab:c\na</p>"),
     ?_assertEqual(conv("'ab:c\na"),"<p>'ab:c\na</p>"),
     ?_assertEqual(conv(":ab:c\na"),"<p>:ab:c\na</p>"),
     ?_assertEqual(conv(".ab:c\na"),"<p>.ab:c\na</p>"),
     ?_assertEqual(conv("0ab:c\na"),"<p>0ab:c\na</p>"),
     ?_assertEqual(conv("9ab:c\na"),"<p>9ab:c\na</p>"),
     ?_assertEqual(conv("8ab:c\na"),"<p>8ab:c\na</p>"),
     ?_assertEqual(conv("7ab:c\na"),"<p>7ab:c\na</p>"),
     ?_assertEqual(conv("6ab:c\na"),"<p>6ab:c\na</p>"),
     ?_assertEqual(conv("5ab:c\na"),"<p>5ab:c\na</p>"),
     ?_assertEqual(conv("4ab:c\na"),"<p>4ab:c\na</p>"),
     ?_assertEqual(conv("3ab:c\na"),"<p>3ab:c\na</p>"),
     ?_assertEqual(conv("2ab:c\na"),"<p>2ab:c\na</p>"),
     ?_assertEqual(conv("1ab:c\na"),"<p>1ab:c\na</p>"),
     ?_assertEqual(conv("_ab:c\na"),"<p>_ab:c\na</p>"),
     ?_assertEqual(conv("*ab:c\na"),"<p>*ab:c\na</p>"),
     ?_assertEqual(conv("+ab:c\na"),"<p>+ab:c\na</p>"),
     ?_assertEqual(
        conv(">ab:c\na"),
        "<blockquote>\n  <p>ab:c\n  a</p>\n</blockquote>"),
     ?_assertEqual(conv("#ab:c\na"),"<h1>ab:c</h1>\n\n<p>a</p>"),
     ?_assertEqual(conv("-ab:c\na"),"<p>-ab:c\na</p>"),
     ?_assertEqual(conv("=ab:c\na"),"<p>=ab:c\na</p>"),
     ?_assertEqual(conv("/ab:c\na"),"<p>/ab:c\na</p>"),
     ?_assertEqual(
        conv("    \nHey\nHo!  \nLets Go"),
        "<p>Hey\nHo! <br />\nLets Go</p>"),
     ?_assertEqual(conv("Hey Ho\t\nLets Go"),"<p>Hey Ho <br />\nLets Go</p>"),
     ?_assertEqual(conv("Hey Ho  \nLets Go"),"<p>Hey Ho <br />\nLets Go</p>"),
     ?_assertEqual(conv("Hey\nHo!\nHardy\n\n"),"<p>Hey\nHo!\nHardy</p>"),
     ?_assertEqual(conv("Hey Ho!\na"),"<p>Hey Ho!\na</p>"),
     ?_assertEqual(conv(" 3 <4\na"),"<p>3 &lt;4\na</p>"),
     ?_assertEqual(conv(" 3 < 4\na"),"<p>3 &lt; 4\na</p>"),
     ?_assertEqual(conv("3 > 4\na"),"<p>3 > 4\na</p>"),
     ?_assertEqual(conv("a\nb\nc\n \n\t\n     "),"<p>a\nb\nc</p>"),
     ?_assertEqual(conv("a\nb\nc\n\n\n"),"<p>a\nb\nc</p>"),
     ?_assertEqual(conv("  \n"),""),
     ?_assertEqual(conv("\t\n"),""),
     ?_assertEqual(conv("\n\n"),""),
     ?_assertEqual(conv("\n"), ""),
         
     ?_assertEqual(
        "<p>some stuff <code>yaycode</code> more stuff <code>more code!"
        "</code></p>",
        conv("some stuff `yaycode` more stuff `more code!`")),
     
     ?_assertEqual(
        "<ul>\n<li>should be <em>italic</em></li>\n<li>should be "
        "<strong>bold</strong></li>\n<li>should be <strong><em>bold italic"
        "</em></strong></li>\n</ul>",
        conv("\n - should be *italic*\n  - should be **bold**\n  - "
             "should be ***bold italic***")),


     ?_assertEqual(
        "<h3 id='test'>Lets let html through</h3>",
        conv("<h3 id='test'>Lets let html through</h3>")),
     
     ?_assertEqual(
        "&copy;",
        conv("&copy;")),
     
     ?_assertEqual(
        "<h3>Lets let html through</h3>",
        conv("<h3>Lets let html through</h3>")),
     
     ?_assertEqual(
        "<form action=\"https://checkout.google.com/api/checkout/v2/"
        "checkoutForm/Merchant/960226209420618\" id=\"BB_BuyButtonForm\""
        "method=\"post\" name=\"BB_BuyButtonForm\" target=\"_top\">"
        "<input name=\"item_name_1\" type=\"hidden\" value=\"Premium"
        "Hypernumbers Account\"/><input name=\"item_description_1\" "
        "type=\"hidden\" value=\"Premium Hypernumbers Account\"/>"
        "<input name=\"item_quantity_1\" type=\"hidden\" value=\"1\"/>"
        "<input name=\"item_price_1\" type=\"hidden\" value=\"90.0\"/>"
        "<input name=\"item_currency_1\" type=\"hidden\" value=\"GBP\"/>"
        "<input name=\"_charset_\" type=\"hidden\" value=\"utf-8\"/>"
        "<input alt=\"\" src=\"https://checkout.google.com/buttons/buy.gif?"
        "merchant_id=960226209420618&amp;w=117&amp;h=48&amp;style=white&amp;"
        "variant=text&amp;loc=en_US\" type=\"image\"/></form>",
        conv("<form action=\"https://checkout.google.com/api/checkout/v2/"
             "checkoutForm/Merchant/960226209420618\" id=\"BB_BuyButtonForm\""
             "method=\"post\" name=\"BB_BuyButtonForm\" target=\"_top\">"
             "<input name=\"item_name_1\" type=\"hidden\" value=\"Premium"
             "Hypernumbers Account\"/><input name=\"item_description_1\" "
             "type=\"hidden\" value=\"Premium Hypernumbers Account\"/>"
             "<input name=\"item_quantity_1\" type=\"hidden\" value=\"1\"/>"
             "<input name=\"item_price_1\" type=\"hidden\" value=\"90.0\"/>"
             "<input name=\"item_currency_1\" type=\"hidden\" value=\"GBP\"/>"
             "<input name=\"_charset_\" type=\"hidden\" value=\"utf-8\"/>"
             "<input alt=\"\" src=\"https://checkout.google.com/buttons/buy."
             "gif?merchant_id=960226209420618&amp;w=117&amp;h=48&amp;style="
             "white&amp;variant=text&amp;loc=en_US\" type=\"image\"/></form>")),
        
     
     
     ?_assertEqual(1,1) % Annoyed by errors when reordering
    ].
