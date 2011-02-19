/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, hn: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.namespace("PageMenu");
HN.PageMenu = function (opts) 
{
    var api = {},

    menubar  = $("#pagebar"),
    dialog   = $("#pagedialog"),
    close    = $("#pdclose"),
    sib_pane = $("#sibling_pane"),
    chi_pane = $("#children_pane"),
    pageurl  = $("#pageurl input"),
    /* visit    = $("#pdvisit"), */

    cur_url     = null,
    tree        = null,
    nav_control = null,
    urlPath;

    function closeDialog() {
        dialog.fadeOut("fast");
    }
    
    function openDialog() {
        pageurl.focus();
        dialog.fadeIn("fast");
    }
    
    function clickHandler(me) {
        openDialog();
        selectNode(me._node);
        return false;
    }
    
    function visitUrl() {
        if (opts && opts.visitHandler) {
            opts.visitHandler.apply(null, [pageurl.val()]);
        } else {
            if (pageurl.val() === cur_url) {
                closeDialog();
            } else {
                window.location.href = pageurl.val();
            }
        }        
    }

    function selectNode(node) {

        var siblings,
            children;
        
        function addToList(elems, list) {

            var i,
                len,
                title,
                open,
                icon,
                right,
                clear,
                item;
            
            list.empty();
            for (i = 0, len = elems.length; i < len; ++i) {
                title = document.createElement("div");
                title.className = "title";
                title.innerHTML = elems[i].value;
                title.title     = elems[i].value;

                open = document.createElement("span");
                open.className = "open";

                icon = document.createElement("span");
                icon.className = elems[i].isLeaf() ? 
                    "leaf" : "crumb";

                right = document.createElement("span");
                right.className = "righticons";
                // right.appendChild(open);
                right.appendChild(icon);

                clear = document.createElement("div");
                clear.style.clear = 'both';

                item = document.createElement("li");
                item._node = elems[i];
                item.appendChild(title);
                item.appendChild(right);
                item.appendChild(clear);

                if (elems[i].value === node.value) {
                    item.className = "selected";
                }

                // HN.Util.addEvent(open, "click", function(itm) {
                //     return function(e) {
                //         openDocument(itm._node);
                //         e.preventDefault();
                //         if (e.stopPropagation) e.stopPropagation();
                //         else e.cancelBubble = true;
                //         return false;
                //     };
                // }(item));
                // HN.Util.addEvent(item, "dblclick", function(itm) {
                //     return function(e) {
                //         var path = that._rootPath(itm, []);
                //         that._openDocument(path);
                //         return false;
                //     };
                // }(item));
                HN.Util.addEvent(item, "click", function (e) {
                    clickHandler(this);
                });
                // HN.Util.addEvent(item, "mouseover", function(o) {
                //     return function(e) {o.style.visibility = "visible"};
                // }(open));
                // HN.Util.addEvent(item, "mouseout", function(o) {
                //     return function(e) {o.style.visibility = "hidden"};
                // }(open));

                list.append(item);
            }
        }
        siblings = node.siblings();
        children = node.children;

        addToList(siblings, sib_pane);
        addToList(children, chi_pane);

        nav_control.setNode(node, urlPath(node));
    }
    
    urlPath = function (node) {
        var segs = ["/"];
        while (node.value !== "/") {
            segs.push("/" + node.value);
            node = node.parent;
        }
        return segs.reverse().join("");
    };

    
    close.click(closeDialog);
    /* visit.click(visitUrl); */

    api.populate = function (u, ps) {

        var segments,
        curtree,
        tmp,
        len,
        crumb,
        seg,
        i;
        
        cur_url = u;
        tree = HN.PageMenu.Tree.construct({"/": ps})[0];
        nav_control = HN.PageMenu.NavControl(pageurl, tree, selectNode);

        segments = u.split("/");
        curtree = tree;

        tmp = HN.Util.id("pageroot");
        tmp._node = tree;
        HN.Util.addEvent(tmp, "click", function (e) {
            clickHandler(this);
        });

        for (i = 0, len = segments.length; i < len; ++i) {
            if (!segments[i]) {
                continue;
            }

            crumb = document.createElement("span");
            crumb.className = "crumb";
            menubar.append(crumb);

            seg = segments[i];
            if (curtree.getChild(seg)) {
                curtree = curtree.getChild(seg);
            } else {
                curtree = curtree.addChild(seg);
            }
            tmp = document.createElement("div");
            tmp.className = "trail";
            tmp.innerHTML = seg;
            tmp._node     = curtree;
            HN.Util.addEvent(tmp, "click", function (e) {
                clickHandler(this);
            });
            menubar.append(tmp);
        }
        tmp.className = "trail current";
    };

    api.close = function () {
        closeDialog();
    };

    api.urlPath = urlPath;
    
    function openDocument(node) {
        var url = urlPath(node);
        if (url !== cur_url) {
            window.location.href = url;
        }
    }

    return api;
};

HN.namespace("HN.PageMenu.NavControl");
HN.PageMenu.NavControl = function (input, tree, selectnode_fn) 
{
    var POLL_INTERVAL = 500,
        api = {},
        cur_node = null;

    api.setNode = function (n, p) {
        cur_node = n;
        input.val(p);
        input.setRange(p.length, p.length);
    };

    function changeNode(n) {
        if (n !== cur_node) {
            selectnode_fn(n);
        }
    }
    
    input.keydown(function (e) {

        var text,
        s_range,
        before,
        e_range,
        now,
        fwd,
        yielded;
        
        if (e.keyCode === HN.Keys.ENTER) {
            text = input.val();
            if (text[0] !== "/") {
                text = "/" + text;
            }
            if (text[text.length - 1] !== "/") {
                text = text + "/";
            }
            window.location.href = text;
        } 
        else if (e.keyCode >= HN.Keys.LEFT && e.keyCode <= HN.Keys.DOWN) {
            switch (e.keyCode) {
            case HN.Keys.LEFT:
                if (cur_node.parent) {
                    changeNode(cur_node.parent);
                }
                break;
            case HN.Keys.UP:
                changeNode(cur_node.prevSibling());
                break;
            case HN.Keys.RIGHT:
                if (cur_node.children[0]) {
                    changeNode(cur_node.children[0]);
                }
                break;
            case HN.Keys.DOWN:
                changeNode(cur_node.nextSibling());
                break;
            }
            e.preventDefault();
            return;
        } else {
            s_range = input.getRange();
            before = input.val();
            yielded = function () {
                e_range = input.getRange();
                now = input.val();
                fwd = s_range.start < e_range.start;
                if (now[now.length - 1] === "/" &&
                     before[before.length - 1] === "/" &&
                     s_range.start < e_range.start) {
                    process(before, fwd);
                } else if (now !== before) {
                    process(now, fwd);
                }
                
                setTimeout(yielded, 0);
            };
        }
    });

    function isprefix(pre, subject) {

        var i;
        
        if (pre.length > subject.length) {
            return false;
        }
        for (i = 0; i < pre.length; i = i + 1) {
            if (pre[i] !== subject[i]) {
                return false;
            }
        }
        return subject.slice(pre.length);
    }

    function completion(text, pre, node) {

        var chi,
        res,
        i;
        
        if (!node) {
            return;
        }
        
        chi = node.children;
        for (i = 0; i < chi.length; i = i + 1) {
            res = isprefix(pre, chi[i].value);
            if (res !== false) {
                input.val(text + res + "/");
                input.setRange(text.length, input.val().length);
                return;
            }
        }
    }
    
    function process(text, fwd) {

        var node,
            rem = "",
            segs,
            i;
        
        if (text.length === 0) {
            text = "/";
            input.val(text);
        }

        node = tree;
        segs = text.split("/");
        if (text[text.length - 1] !== "/") {
            rem = segs.pop();
        }
        for (i = 0; i < segs.length; i = i + 1) {
            if (segs[i]) {
                node = node ? node.getChild(segs[i]) : null;
            }
        }

        if (node) {
            changeNode(node);
            input.val(text);
            if (fwd) { 
                completion(text, rem, node);
            }
        }
    }
    
    return api;
};


HN.namespace("HN.PageMenu.Tree");
HN.PageMenu.Tree = function () 
{
    function Node(v, p, c, i) {
        this.value = v;
        this.parent = p;
        this.children = c || [];
        this.pos = i || 0;
    }

    // some functions that will be used in the prototypes
    function nodeSort(n1, n2) {
        if (n1.value === n2.value) {
            return 0;
        }
        return n1.value < n2.value ? -1 : 1;
    }

    function orderNodes(nodes) {
        nodes.sort(nodeSort);
        for (var i = 0; i < nodes.length; i = i + 1) {
            nodes[i].pos = i;
        }
    }

    function nodeSeek(nodes, seeking) {
        var binSearch = function (low, high) {
            if (high < low) {
                return -1;
            } 
            var mid = Math.floor(low + ((high - low) / 2));
            if (nodes[mid].value > seeking) {
                return binSearch(low, mid - 1);
            } else if (nodes[mid].value < seeking) {
                return binSearch(mid + 1, high);
            } else {
                return mid;
            }
        };
        return binSearch(0, nodes.length - 1);
    }
    
    // Now do the prototypes of the Node object
    Node.prototype.set = function (v, p, c, i) {
        this.value = v;
        this.parent = p;
        this.children = c;
        this.pos = i || 0;
    };
    Node.prototype.addChild = function (v) {
        // n(log n) insertion -- slow, but rare use.
        var n = new Node(v, this, []);
        this.children.push(n);
        orderNodes(this.children);
        return n;
    };
    Node.prototype.getChild = function (v) {
        var idx = nodeSeek(this.children, v);
        return idx === -1 ? null : this.children[idx];
    };
    Node.prototype.nextSibling = function () {
        if (this.parent === null || 
            this.pos === this.parent.children.length - 1) {
            return this;
        }
        return this.parent.children[this.pos + 1];
    };
    Node.prototype.prevSibling = function () {
        if (this.parent === null || 
            this.pos === 0) {
            return this;
        }
        return this.parent.children[this.pos - 1];
    };
    Node.prototype.siblings = function () {
        if (this.parent === null) {
            return [this];
        } else {
            return this.parent.children;
        }
    };
    Node.prototype.isLeaf = function () {
        return this.children.length === 0;
    };

    function construct(obj, parent) {

        var nodes,
            k,
            n;
        
        parent = parent || null;
        if (!obj) { 
            return [];
        } else if (typeof obj !== 'object') {
            return [new Node(obj, parent, [])];
        } else {
            nodes = [];
            for (k in obj) {
                if (obj.hasOwnProperty(k)) {
                    n = new Node();
                    n.set(k, parent, construct(obj[k], n));
                    nodes.push(n);
                }
            }
            orderNodes(nodes);
            return nodes;
        }
    }
    return {construct: construct};
} ();