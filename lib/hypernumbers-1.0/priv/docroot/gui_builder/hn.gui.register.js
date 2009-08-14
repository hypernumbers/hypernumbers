/**
 * @class HN.Gui.Register
 * holds the register of all the hypernumbers pages loaded and all the objects
 * they are bound to on the GUI
 */
//
// the type parameter can be one of the following strings
// [dynamic | gui_builder]
//
// and it controls how which pages are loaded
// In normal usage the register is called with 'dynamic' because
// the statically bound items have already been bound and we neither
// need nor want to keep a connection to the page they were on
// BUT when the register is being used in the Gui Builder we DO want
// a dynamic link to them so the GUI can be WYSIWIG
HN.Gui.Register = function(type)
{
  // Define some vars
  var items;
  var i;
  var array = [];
  var registerobj = {};
  var pagesobj = {};
  var pages ={};
  var r;
  var p;
  var b1;
  var b2;
  var options;
  // Extract all the elements marked with the class name
  // 'hn' and parse them to parse_element
  // so that we can build the registration
  //
  // THE REGISTER IS ENTIRELY SPECIFIED BY THE CLASS NAME
  // See the documentation at parse_element for a description of it
  items = document.getElementsByClassName("hn");
  for (i = 0; i < items.length; i += 1) {
    array = HN.Gui.Register.parse_element(items[i]);
    if (HN.Gui.Register.is_valid(array[1], type)) {
      registerobj[array[0]] = array[1];
    };
  }
  // this.register = registerobj;
  //
  // Now traverse the register extracting the pages that we need to bind to
  // NOTE that there is not always a source page so check if it exists
  //
  for (r in registerobj) {
    pagesobj[registerobj[r].bindingpage] = {"page": "not loaded yet",
                                            "bindings": []};

    pagesobj[registerobj[r].sourcepage]  = {"page": "not loaded yet",
                                            "bindings": []};
  };
  //
  // We had a list of html items with the pages that they bind to
  // we now need to reverse that so we have a list of pages with the
  // html items that are bound to them
  //
  for (r in registerobj) {
    b1 = pagesobj[registerobj[r].sourcepage];
    b2 = b1.bindings;
    b2[b2.length] = registerobj[r];
    b1.bindings = b2;
    pagesobj[registerobj[r].sourcepage] = b1;
  }
  // We now have an object with a unique list of pages, instantiate each
  // page as an HN.data item
  //

  for (p in pagesobj) {
    options = new HN.Gui.DataOptions(pagesobj[p].bindings);
    pages[p] = new HN.Data(p, options);
  };
  this.pages = pages;
};

// Brutal!
HN.Gui.Register.reregister = function(type) {
  Register = new HN.Gui.Register(type);
};

//
// These are the class methods
//
HN.Gui.Register.prototype.get_values = function(page, ref, reftype) {
  var cells;
  var ref2;
  var value;
  cells = this.pages[page].data.cell;
  switch (reftype) {
    case "cell":
      ref2 = HN.Util.parse_ref(ref.toUpperCase());
      return HN.Gui.Register.get_cell_value(cells, ref2.x, ref2.y);
    case "range":
      ref2 = HN.Util.parse_range(ref.toUpperCase());
      return HN.Gui.Register.get_range_values(cells, ref2.x1, ref2.y1,
                                          ref2.x2, ref2.y2);
    default:
      return [];
  };
};

HN.Gui.Register.prototype.get_cells = function(page, cell) {
  var cells;
  var ref;
  var cell_contents;
  cells = this.pages[page].data.cell;
  ref = HN.Util.parse_ref(cell.toUpperCase());
  cell_contents = HN.Gui.Register.get_cell_contents(cells, ref.x, ref.y);
  return cell_contents;
};

HN.Gui.Register.prototype.set_cell_value = function(page, cell, value) {
  var cells;
  var ref;
  cells = this.pages[page].data.cell;
  ref = HN.Util.parse_ref(cell.toUpperCase());
  if (typeof cells[ref.y] == "undefined") {
    cells[ref.y] = {};
    }
  if (typeof cells[ref.y][ref.x] == "undefined") {
    cells[ref.y][ref.x] = {};
  }
  cells[ref.y][ref.x]['value'] = value;
};

//
// These are just internal utility functions of the class
//
HN.Gui.Register.get_cell_contents = function(cells, x, y)
{
  if (cells[y]) {
    if (cells[y][x]) {
      return cells[y][x];
    } else {
      return {};
    };
  } else {
    return {};
  };
};

HN.Gui.Register.get_cell_value = function(cells, x, y)
{
  if (cells[y]) {
    if (cells[y][x]) {
      if (cells[y][x]['value']) {
        return [cells[y][x]['value']];
      } else {
        return [""];
      };
    } else {
      return [""];
    };
  } else {
    return [""];
  };
};

HN.Gui.Register.get_range_values = function(cells, x1, y1, x2, y2)
{
  var i;
  var j;
  var array = [];
  var value;
  for (i = x1; i < x2 + 1; i +=1) {
    for (j = y1; j < y2 + 1; j +=1) {
      value  = HN.Gui.Register.get_cell_value(cells, i, j);
      array[array.length] = value[0];
    };
  };
  return array;
};


HN.Gui.Register.is_valid= function(component, guitype)
{
  switch (guitype) {
  case "gui_builder":
    if (component['validity'] === "valid") {
      return true;
    } else {
      return false;
    };
  case "dynamic":
    if (component['validity']   === "valid"
       && (component['rendertype']    === "both"
           || component['rendertype'] === "dynamic")) {
      return true;
    } else {
      return false;
    };
  };
};

HN.Gui.Register.parse_element = function(element) {
  var register = {};
  var returnarray = [];
  var binding;
  var bindingsobj = [];
  var rendertype;
  var source;
  var sourceobj = [];
  register['componenttype'] = element.getAttribute("data-gui-element-type");
  register['id'] = element.id;
  // first do the binding
  binding = element.getAttribute("data-binding");
  rendertype = element.getAttribute("data-render-type");
  // If these aren't set, or if the render type is anything other than
  // [dynamic | static | both] the binding is invalid
  if ((!binding || !rendertype || !element.id)
       || (rendertype !== "dynamic"
           && rendertype !== "static"
           && rendertype !== "both")) {
    register['validity'] = "invalid";
    returnarray[0] = element.id;
    returnarray[1] = register;
    return returnarray;
  }
  bindingsobj = HN.Gui.Register.parse_binding(binding);
  register['bindingpage'] = bindingsobj['page'];
  register['bindingref']  = bindingsobj['ref'];
  register['bindingtype'] = bindingsobj['type'];
  if (register['componenttype'] === "lable"
     || register['componenttype'] === "image"
     || register['componenttype'] === "inputtext") {
    // In these cases the source is the same as the binding
    register['sourcepage']  = register['bindingpage'];
    register['sourceref']   = register['bindingref'];
    register['sourcetype']  = register['bindingtype'];
    register['validity']    = "valid";
  } else if (register['componenttype'] === "inputradio"
    || register['componenttype'] === "select")
  {
    source = element.getAttribute("data-source-binding");
    sourceobj = HN.Gui.Register.parse_binding(source);
    register['sourcepage']  = sourceobj['page'];
    register['sourceref']   = sourceobj['ref'];
    register['sourcetype']  = sourceobj['type'];
    register['validity']    = "valid";

  } else {
    register['validity'] = invalid;
  }
  returnarray[0] = element.id;
  returnarray[1] = register;
  return returnarray;
};

HN.Gui.Register.parse_binding = function(binding)
{
  var bits;
  bits = binding.split("/");
    switch (bits[0]) {
    case "http:":
      return HN.Gui.Register.break_out(binding);
    case "https:":
      return HN.Gui.Register.break_out(binding);
    default:
      switch (binding[0]) {
        case "/":
          return HN.Gui.Register.break_out(document.location.protocol
                                           + "//"
                                           + document.location.host
                                           + binding);
          default:
            return HN.Gui.Register.break_out(document.location.protocol
                                             + "//"
                                             + document.location.host
                                             + document.location.pathname
                                             + binding);
      };
    };
};

HN.Gui.Register.break_out = function(string)
{
  var bits;
  var slice;
  var returnobj = {};
  bits = string.split("/");
  switch (string[string.length - 1]) {
    case "/":
      returnobj['page'] = HN.Gui.Register.compress_url(bits);
      returnobj['ref']  = "";
      returnobj['type'] = "page";
      break;
    default:
      slice = bits.slice(0,bits.length - 1);
      returnobj['page'] = HN.Gui.Register.compress_url(slice);
      returnobj['ref'] = bits[bits.length - 1];
      returnobj['type'] = HN.Gui.Register.get_type(returnobj['ref']);
  }
  return returnobj;
};

HN.Gui.Register.get_type = function(ref)
{
  var cell    = /^[a-zA-Z]+[0-9]+$/;
  var range   = /^[a-zA-Z]+[0-9]+:[a-zA-Z]+[0-9]+$/;
  var col     = /^[a-zA-Z]+:[a-zA-Z]+$/;
  var lastcol = /^[a-zA-Z]+\-[0-9]+$/;
  var row     = /^[0-9]+:[0-9]+$/;
  var lastrow = /^\-[a-zA-Z]+[0-9]+$/;
  if (cell.exec(ref)           && (cell.exec(ref)[0]    === ref)) {
    return "cell";
  } else if (range.exec(ref)   && (range.exec(ref)[0]   === ref)) {
    return "range";
  } else if (col.exec(ref)     && (col.exec(ref)[0]     === ref)) {
    return "column";
  } else if (lastcol.exec(ref) && (lastcol.exec(ref)[0] === ref)) {
    return "lastcolumn";
  } else if (row.exec(ref)     && (row.exec(ref)[0]     === ref)) {
    return "row";
  } else if (lastrow.exec(ref) && (lastrow.exec(ref)[0] === ref)) {
    return "lastrow";
  } else {
    return "invalid";
  };
};

HN.Gui.Register.compress_url = function(array)
{
  switch (array[0]) {
    case "http:":
      return array[0] + "//" + array[2]
        + HN.Gui.Register.compress([], array.slice(3, array.length));
      case "https:":
        return array[0] + "//" + array[2]
          + HN.Gui.Register.compress([], array.slice(3, array.length));
      default:
        return HN.Gui.Register.compress([], array);
  };
};

// this function takes a URL of type proto://sub.dom.tld:port/blah/bleh/../bloh
// and compresses it to proto://sub.dom.tld:port/blah/bloh
HN.Gui.Register.compress = function(array1, array2)
{
  switch (array2[0]){
    case "..":
      return HN.Gui.Register.compress(array1.slice(0, array1.length - 1),
        array2.slice(1, array2.length));
    case ".":
      return HN.Gui.Register.compress(array1,
        array2.slice(1, array2.length));
    case "":
      return HN.Gui.Register.compress(array1, array2.slice(1, array2.length));
    case undefined:
      return "/" + array1.join("/") + "/";
    default:
      array1[array1.length] = array2[0];
      return HN.Gui.Register.compress(array1,
        array2.slice(1, array2.length));
  };
};
