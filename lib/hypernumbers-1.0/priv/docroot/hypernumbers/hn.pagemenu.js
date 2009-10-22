HN.namespace("PageMenu");
HN.PageMenu = function() 
{
    var public = {},

    menubar  = $("#pagebar"),
    dialog   = $("#pagedialog")
    sib_pane = $("#sibiling_pane"),
    chi_pane = $("#children_pane"),
    pageurl  = $("#pageurl input"),
    dividor  = $("#pane_divider"),

    cur_url     = null,
    dialogTimer = null,
    tree        = null,
    nav_control = null;

    public.populate = function(u, ps) {
        cur_url = u;
        tree = HN.PageMenu.Tree.construct({"/":ps})[0];
        nav_control = HN.PageMenu.NavControl(pageurl, tree, selectNode);

        var segments = u.split("/");
        curtree = tree;

        tmp = HN.Util.id("pageroot");
        tmp._node = tree;
        HN.Util.addEvent(tmp, "click", function(e) {
            clickHandler(this);
        });
        
        for (var i = 0, len = segments.length; i < len; ++i) {
            if (!segments[i]) continue;

            var crumb = document.createElement("span");
            crumb.className = "crumb";
            menubar.append(crumb);

            var seg = segments[i];
            if (curtree.getChild(seg))
                curtree = curtree.getChild(seg);
            else
                curtree = curtree.addChild(seg);

            tmp = document.createElement("div");
            tmp.className = "trail";
            tmp.innerHTML = seg;
            tmp._node     = curtree;
            HN.Util.addEvent(tmp, "click", function(e) {
                clickHandler(this);
            });
            menubar.append(tmp);
        }
        tmp.className = "trail current";
    };

    public.urlPath = urlPath;
    var urlPath = function(node) {
        var segs = ["/"];
        while (node.value !== "/") {
            segs.push("/" + node.value);
            node = node.parent;
        }
        return segs.reverse().join("");
    };

    function selectNode(node) {
        function addToList(elems, list) {
            list.empty();
            for (var i = 0, len = elems.length; i < len; ++i) {
                var title = document.createElement("div")
                title.className = "title";
                title.innerHTML = elems[i].value;
                title.title     = elems[i].value;

                var open = document.createElement("span");
                open.className = "open";

                var icon = document.createElement("span");
                icon.className = elems[i].isLeaf() ? 
                    "leaf" : "crumb";

                var right = document.createElement("span");
                right.className = "righticons"
                right.appendChild(open);
                right.appendChild(icon);

                var clear = document.createElement("div");
                clear.style.clear = 'both';

                var item = document.createElement("li");
                item._node = elems[i];
                item.appendChild(title);
                item.appendChild(right);
                item.appendChild(clear);

                if (elems[i].value === node.value)
                    item.className = "selected";

                HN.Util.addEvent(open, "click", function(itm) {
                    return function(e) {
                        openDocument(itm._node);
                        e.preventDefault();
                        if (e.stopPropagation) e.stopPropagation();
                        else e.cancelBubble = true;
                        return false;
                    };
                }(item));
                // HN.Util.addEvent(item, "dblclick", function(itm) {
                //     return function(e) {
                //         var path = that._rootPath(itm, []);
                //         that._openDocument(path);
                //         return false;
                //     };
                // }(item));
                HN.Util.addEvent(item, "click", function(e) {
                    clickHandler(this);
                });
                HN.Util.addEvent(item, "mouseover", function(o) {
                    return function(e) {o.style.visibility = "visible"};
                }(open));
                HN.Util.addEvent(item, "mouseout", function(o) {
                    return function(e) {o.style.visibility = "hidden"};
                }(open));

                list.append(item);
            }
        }
        var sibilings = node.sibilings(),
        children      = node.children;

        addToList(sibilings, sib_pane);
        addToList(children, chi_pane);
        
        dividor.height(Math.max(sib_pane.height(), chi_pane.height()));
        nav_control.setNode(node, urlPath(node));
    };

    function clickHandler(me) {
        showDialog();
        if (me._node.isLeaf()) {
            openDocument(me._node);
        } else {
            selectNode(me._node);
        }
        return false;
    };
    
    function showDialog() {
        pageurl.focus();
        clearTimeout(dialogTimer);

        closeDialog = function() {
            dialog.fadeOut("fast");
        };

        dialog.mouseleave(function() {
            clearTimeout(dialogTimer);
            dialogTimer = setTimeout(closeDialog, 500);
        });

        dialog.mouseenter(function() {
            clearTimeout(dialogTimer);
        });

        dialogTimer = setTimeout(closeDialog, 10000);
        dialog.fadeIn("fast");
    };

    function openDocument(node) {
        var url = urlPath(node);
        if (url !== cur_url)
            window.location.href = url;
    };

    return public;
};

HN.namespace("HN.PageMenu.NavControl") 
HN.PageMenu.NavControl = function(input, tree, selectnode_fn) 
{
    var POLL_INTERVAL = 500;
    var LEFT_ARROW    = 37;
    var UP_ARROW      = 38;
    var RIGHT_ARROW   = 39;
    var DOWN_ARROW    = 40;

    var public = {},
    cur_node = null,
    previous = "";

    public.setNode = function(n, p) {
        cur_node = n;
        input.val(p);
        input.setRange(p.length, p.length);
        previous = p;
    };

    input.keydown(function(e) {
        if (e.keyCode >= LEFT_ARROW && e.keyCode <= DOWN_ARROW) {
            switch (e.keyCode) {
            case LEFT_ARROW:
                if (cur_node.parent)
                    changeNode(cur_node.parent);
                break;
            case UP_ARROW:
                changeNode(cur_node.prevSibiling());
                break;
            case RIGHT_ARROW:
                if (cur_node.children[0])
                    changeNode(cur_node.children[0]);
                break;
            case DOWN_ARROW:
                changeNode(cur_node.nextSibiling());
                break;
            }
            e.preventDefault();
            return;
        } else {
            var s_range = input.getRange();
            var before = input.val();
            function yielded() {
                var e_range = input.getRange(),
                now = input.val();
                if ( now[now.length-1] == "/" &&
                     before[before.length-1] == "/" &&
                     s_range.start != e_range.start)
                {
                    previous = "";
                    process(before);
                } else if (s_range.start < e_range.start){
                    process(now);
                }
            };
            setTimeout(yielded, 0);
        }
    });
    
    function process(text) {
        if (text.length == 0) {
            input.val("/");
            text = "/";
        }
        if (text == previous) 
            return;

        var node = tree, rem = "",
        segs = text.split("/");
        if (text[text.length-1] !== "/")
            rem = segs.pop();
        for (var i = 0; i < segs.length; i++) {
            if (segs[i])
                node = node ? node.getChild(segs[i]) : null;
        }

        if (node) {
            if (text.length > previous.length)
                changeNode(node);
            completion(text, rem, node);
        }
        previous = input.val();
    };

    function changeNode(n) {
        if (n !== cur_node) {
            selectnode_fn(n);
        }
    };

    function completion(text, pre, node) {
        var chi = node.children, res;
        for (var i = 0; i < chi.length; i++) {
            res = isprefix(pre, chi[i].value);
            if (res !== false) {
                input.val(text + res + "/");
                input.setRange(text.length, input.val().length);
                return;
            }
        }
    };

    function isprefix(pre, subject) {
        if (pre.length > subject.length)
            return false;
        for (var i = 0; i < pre.length; i++)
            if (pre[i] !== subject[i])
                return false;
        return subject.slice(pre.length);
    };

    return public;
}


HN.namespace("HN.PageMenu.Tree");
HN.PageMenu.Tree = function() 
{
    function Node(v,p,c) {
        this.value = v;
        this.parent = p;
        this.children = c || [];
    };
    Node.prototype.set = function(v,p,c) {
        this.value = v;
        this.parent = p;
        this.children = c;
    };
    Node.prototype.addChild = function(v) {
        var n = new Node(v, this, []);
        this.children.push(n);
        this.children.sort(nodeSort);
        return n;
    };
    Node.prototype.getChild = function(v) {
        var idx = nodeSeek(this.children, v);
        return idx == -1 ? null : this.children[idx];
    };
    Node.prototype.nextSibiling = function() {
        if (this.parent === null)
            return this;

        var sibilings = this.sibilings(),
        idx = nodeSeek(sibilings, this.value),
        ret = this.parent.children[idx + 1];
        return ret ? ret : this;
    };
    Node.prototype.prevSibiling = function() {
        if (this.parent === null)
            return this;

        var sibilings = this.sibilings(),
        idx = nodeSeek(sibilings, this.value),
        ret = this.parent.children[idx - 1];
        return ret ? ret : this;
    };
    Node.prototype.sibilings = function() {
        if (this.parent === null)
            return [this];
        else
            return this.parent.children;
    };
    Node.prototype.isLeaf = function() {
        return this.children.length == 0;
    };

    function nodeSort(n1, n2) {
        if (n1.value === n2.value) 
            return 0;
        return n1.value < n2.value ? -1 : 1;
    };

    // could be made faster, since in sorted order.
    function nodeSeek(nodes, value) {
        for (var i = 0; i < nodes.length; i++) 
            if (nodes[i].value === value) 
                return i
        return -1;
    };
      
    function construct(obj, parent) {
        parent = parent || null;
        if (!obj) 
            return [];
        else if (typeof obj !== 'object') {
            return [node(obj, parent, [])];
        } else {
            var nodes = [];
            for (var k in obj) {
                if (obj.hasOwnProperty(k)) {
                    var n = new Node();
                    n.set(k, parent, construct(obj[k], n));
                    nodes.push(n);
                }
            }
            nodes.sort(nodeSort);
            return nodes;
        }
    }
    return {construct: construct};
}();