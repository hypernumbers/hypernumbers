HN.namespace("PageMenu");
HN.PageMenu = function() 
{
    var public = {},

    menubar  = $("#pagebar"),
    dialog   = $("#pagedialog")
    sib_pane = $("#sibiling_pane"),
    chi_pane = $("#children_pane"),
    pageurl  = $("#pageurl"),
    dividor  = $("#pane_divider"),

    cur_url     = null,
    dialogTimer = null,
    tree        = null,

    public.populate = function(u, ps) {
        cur_url = u;
        tree = HN.DS.Tree.construct({"/":ps})[0];

        var segments = u == "/" ? "" : u.substring(1, u.length-1).split('/'), 
        curtree = tree;

        tmp = HN.Util.id("pageroot");
        tmp._node = tree;
        HN.Util.addEvent(tmp, "click", function(e) {
            click_handler(this);
        });
        
        for (var i = 0, len = segments.length; i < len; ++i) {
            var crumb = document.createElement("span");
            crumb.className = "crumb";
            menubar.append(crumb);

            var seg = segments[i];
            if (curtree.getchild(seg))
                curtree = curtree.getchild(seg);
            else
                curtree = curtree.addChild(seg);

            tmp = document.createElement("div");
            tmp.className = "trail";
            tmp.innerHTML = seg;
            tmp._node     = curtree;
            HN.Util.addEvent(tmp, "click", function(e) {
                click_handler(this);
            });
            menubar.append(tmp);
        }
        tmp.className = "trail current";
    };

    function click_handler(me) {

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

                if (elems[i].value === me._node.value)
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
                    click_handler(this);
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

        if (me._node.isLeaf()) {
            openDocument(me._node);
        } else {
            var sibilings = me._node.sibilings(),
            children  = me._node.children;

            addToList(sibilings, sib_pane);
            addToList(children, chi_pane);

            pageurl.text(urlPath(me._node));
            showDialog();
            dividor.height(Math.max(sib_pane.height(), chi_pane.height()));
        }
        return false;
    };

    function showDialog() {
        clearTimeout(dialogTimer);

        closeDialog = function() {
            //that._dialog.fadeOut("fast");
        };

        dialog.mouseleave(function() {
            clearTimeout(dialogTimer);
            dialogTimer = setTimeout(closeDialog, 500);
        });

        dialog.mouseenter(function() {
            clearTimeout(dialogTimer);
        });

        dialogTimer = setTimeout(closeDialog, 5000);
        dialog.fadeIn("fast");
    };

    function urlPath(node) {
        var segs = ["/"];
        while (node.value !== "/") {
            segs.push("/" + node.value);
            node = node.parent;
        }
        return segs.reverse().join("");
    };

    function openDocument(node) {
        var url = urlPath(node);
        if (url !== cur_url)
            window.location.href = url;
    };

    return public;
};

HN.namespace("DS.Tree");
HN.DS.Tree = function() 
{
    function Node(v,p,c) {
        this.value = v
        this.parent = p
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
        return n;
    };
    Node.prototype.getchild = function(v) {
        for (var i = 0; i < this.children.length; i++) 
            if (this.children[i].value === v) 
                return this.children[i];
        return null;
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

    function parse(parent, obj) {
        if (!obj) 
            return [];
        else if (typeof obj !== 'object') {
            return [node(obj, parent, [])];
        } else {
            var nodes = [];
            for (var k in obj) {
                if (obj.hasOwnProperty(k)) {
                    var n = new Node();
                    n.set(k, parent, parse(n, obj[k]));
                    nodes.push(n);
                }
            }
            return nodes;
        }
    }
    return {construct: function(data) {return parse(null, data)}};
}();