HN.PageMenu = function() 
{
    this._menubar  = $("#pagebar");
    this._dialog   = $("#pagedialog");
    this._sib_pane = $("#sibiling_pane");
    this._chi_pane = $("#children_pane");
    this._pageurl  = $("#pageurl");
    this._dividor  = $("#pane_divider");

    this._cur_url     = null;
    this._dialogTimer = null;
    this._pages       = [];
};

HN.PageMenu.prototype.populate = function(url, pages)
{
    this._cur_url = url;
    this._pages = {"/":pages};

    var that = this,
    segments = url == "/" ? "" : url.substring(1, url.length-1).split('/'), 
    tmp = parent = HN.Util.id("pageroot");

    parent._name = "/";
    parent._parent = null;
    HN.Util.addEvent(parent, "click", function(e) {
        that._click(this);
        return false;
    });

    for (var i = 0, len = segments.length; i < len; ++i) {
        var crumb = document.createElement("span");
        crumb.className = "crumb";
        this._menubar.append(crumb);

        var seg = segments[i];
        tmp = document.createElement("div");
        tmp.className = "trail";
        tmp.innerHTML = seg;
        tmp._name     = seg;
        tmp._parent   = parent;
        HN.Util.addEvent(tmp, "click", function(e) {
            that._click(this);
            return false;
        });

        this._menubar.append(tmp);
        parent = tmp;        
    }
    tmp.className = "trail current";
};

HN.PageMenu.prototype._click = function(me)
{
    var that = this,
    addToList = function(elems, parElem, parTree, list) {
        list.empty();
        for (var i = 0, len = elems.length; i < len; ++i) {
            var title = document.createElement("div")
            title.className = "title";
            title.innerHTML = elems[i];
            title.title     = elems[i];

            var open = document.createElement("span");
            open.className = "open";

            var icon = document.createElement("span");
            icon.className = that._isLeaf(parTree[elems[i]]) ?
                "leaf" : "crumb";

            var right = document.createElement("span");
            right.className = "righticons"
            right.appendChild(open);
            right.appendChild(icon);

            var clear = document.createElement("div");
            clear.style.clear = 'both';

            var item = document.createElement("li");
            item._name = elems[i];
            item._parent = parElem;
            item.appendChild(title);
            item.appendChild(right);
            item.appendChild(clear);

            if (elems[i] == me._name)
                item.className = "selected";

            HN.Util.addEvent(open, "click", function(itm) {
                return function(e) {
                    var path = that._rootPath(itm, []);
                    that._openDocument(path);
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
                that._click(this);
                return false;
            });
            HN.Util.addEvent(item, "mouseover", function(o) {
                return function(e) {o.style.visibility = "visible"};
            }(open));
            HN.Util.addEvent(item, "mouseout", function(o) {
                return function(e) {o.style.visibility = "hidden"};
            }(open));

            list.append(item);
        }
    };

    var parentTree = this._seekTree(this._rootPath(me._parent, []));
    if (this._isLeaf(parentTree[me._name]))
        that._openDocument(that._rootPath(me, []));
    else {
        var my_sibilings = this._getChildren(parentTree),
        my_children      = this._getChildren(parentTree[me._name]),
        url              = this._pathToUrl(this._rootPath(me, []));

        addToList(my_sibilings, me._parent, parentTree, this._sib_pane);
        addToList(my_children, me, parentTree[me._name], this._chi_pane);

        this._pageurl.text(url);
        this._showDialog();
        this._dividor.height(Math.max(this._sib_pane.height(),
                                      this._chi_pane.height()));
    }
};

HN.PageMenu.prototype._showDialog = function() 
{
    clearTimeout(this._dialogTimer);

    var that = this,
    closeDialog = function() {
            //that._dialog.fadeOut("fast");
    };
    
    this._dialog.mouseleave(function() {
        clearTimeout(that._dialogTimer);
        that._dialogTimer = setTimeout(closeDialog, 500);
    });

    this._dialog.mouseenter(function() {
        clearTimeout(that._dialogTimer);
    });

    this._dialogTimer = setTimeout(closeDialog, 5000);
    this._dialog.fadeIn("fast");
};

/* Builds the path from an element to the root (reversed) */
HN.PageMenu.prototype._rootPath = function(elem, rp) 
{
    if (elem === null)
        return rp;
    rp.push(elem._name);
    return this._rootPath(elem._parent, rp);
};

/* Returns the unique tree described by path */
HN.PageMenu.prototype._seekTree = function(path) 
{
    if (!path) 
        return this._pages;
    tree = this._pages;
    for (var i = path.length; i-->0;)
        tree = tree[path[i]];
    return tree;
}

/* Return the names of tree's immediate children */
HN.PageMenu.prototype._getChildren = function(tree) 
{
    var children = [];
    for (var n in tree) 
        if(tree.hasOwnProperty(n)) {
            children.push(n);
        }
    return children;
};

HN.PageMenu.prototype._pathToUrl = function(path) 
{
    var url = "/";
    for (var i = path.length-1; i-->0;) {
        url += (path[i] + "/");
    }
    return url;
};

HN.PageMenu.prototype._isLeaf = function(tree) {
    return this._getChildren(tree).length == 0;
};
    
HN.PageMenu.prototype._openDocument = function(path) {
    var url = this._pathToUrl(path);
    if (url != this._cur_url)
        window.location.href = url;
};
