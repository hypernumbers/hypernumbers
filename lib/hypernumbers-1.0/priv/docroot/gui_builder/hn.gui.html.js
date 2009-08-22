/**
 * @class Hn.Gui.HTML
 * handles the behaviour of individual html elements
 */

HN.Gui.HTML = function(id, componenttype,
                       bindingpage, bindingref, bindingtype,
                       sourcepage, sourceref, sourcetype) {
  var element;
  element = HN.Util.id(id);
  // lots of different component types except with lables for CSS to treat them
  // differently for layup...
  switch (componenttype) {
    case "lable":
      return HN.Gui.HTML.make_p(id, sourcepage, sourceref, sourcetype);
    case "image":
      return HN.Gui.HTML.make_img(id, sourcepage, sourceref, sourcetype);
    case "inputtext":
      return HN.Gui.HTML.make_input_text_fn(id, sourcepage,
                                            sourceref, sourcetype,
                                            bindingpage, bindingref,
                                            bindingtype);
    case "inputradio":
      return HN.Gui.HTML.make_input_radio_fn(id, sourcepage,
                                             sourceref, sourcetype,
                                             bindingpage, bindingref,
                                             bindingtype);
    case "select":
      return HN.Gui.HTML.make_select_fn(id, sourcepage,
                                        sourceref, sourcetype,
                                        bindingpage, bindingref, bindingtype);
    case "dblclick_to_edit":
    return HN.Gui.HTML.make_dblclick(id,  sourcepage,
                                     sourceref, sourcetype,
                                     bindingpage, bindingref, bindingtype);
    default:
      return HN.Gui.HTML.nullfn;
  }
};

HN.Gui.HTML.nullfn = function() {};

HN.Gui.HTML.make_p = function(id, spage, sref, stype) {
  return function() {
    var page;
    var values;
    var element;
    var html ="";
    var i;
    page =
    values = HNGui.register.get_values(spage, sref, stype);
    element = HN.Util.id(id);
    for (i = 0; i < values.length; i += 1){
      html += "<p>" + values[i] + "</p>";
    }
    element.innerHTML = html;
  };
};

HN.Gui.HTML.make_img = function(id, spage, sref, stype) {
  return function() {
    var values;
    var element;
    var html ="";
    var i;
    values = HNGui.register.get_values(spage, sref, stype);
    element = HN.Util.id(id);
    for (i = 0; i < values.length; i += 1){
      html += "<img src=\"" + values[i] + "\" />";
    }
    element.innerHTML = html;
  };
};

HN.Gui.HTML.make_input_text_fn = function(id,
                                          spage, sref, stype,
                                          bpage, bref, btype) {
  return function() {
    var values;
    var element;
    var child;
    var html ="";
    var i;
    values = HNGui.register.get_values(spage, sref, stype);
    element = HN.Util.id(id);
    for (i = 0; i < values.length; i += 1){
      html += "<input type=\"text\" value=\"" + values[i] +"\" class=\"ui-state-default\"/>";
    };
    element.innerHTML = html;
    for (i = 0; i < values.length; i += 1){
      child = element.childNodes[i];
      HN.Util.addEvent(child, "change", HN.Gui.HTML.onchangefn(bpage, bref));
    };
  };
};

HN.Gui.HTML.make_input_radio_fn = function(id,
                                           spage, sref, stype,
                                           bpage, bref, btype) {
  return function() {
    var currentvalues;
    var values;
    var element;
    var child;
    var html;
    currentvalues = HNGui.register.get_values(bpage, bref, btype);
    values = HNGui.register.get_values(spage, sref, stype);
    element = HN.Util.id(id);
    for (var i = 0; i < currentvalues.length; i += 1) {
      html = "<div id=\"" + id + i + "\" >";
      html += "<table class=\"hntable\">";
      for (var j = 0; j < values.length; j += 1) {
        html += "<tr><td>" + values[j] + "</td><td>";
        html += "<input name=\"radio_" + id + "_" + i
          + "\" type=\"radio\" value=\""
          + values[j] + "\"";
        if (values[j] === currentvalues[i]) {
          html += " checked";
        }
        html += "class=\"ui-state-default\" /></td></tr>";
      }
      html += "</table></div>";
      element.innerHTML = html;
      HN.Util.addEvent(element.childNodes[i], "click",
                       HN.Gui.HTML.onchangefn(bpage, bref));
    };
  };
};

HN.Gui.HTML.make_select_fn = function(id,
                                      spage, sref, stype,
                                      bpage, bref, btype) {
  return function() {
    var currentvalues;
    var values;
    var element;
    var child;
    var html;
    currentvalues = HNGui.register.get_values(bpage, bref, btype);
    values = HNGui.register.get_values(spage, sref, stype);
    element = HN.Util.id(id);
    for (var i = 0; i < currentvalues.length; i += 1) {
      html = "<select class=\"ui-state-default\">";
      for (var j = 0; j < values.length; j += 1) {
        if (values[j]) {
          html += "<option value=\"" + values[j] + "\" ";
          if (values[j] === currentvalues[i]) {
            html += " selected";
          };
          html += "\">" + values[j] + "</option>";
        };
      };
      html += "</select>";
      element.innerHTML = html;
      HN.Util.addEvent(element.childNodes[i],
                       "change", HN.Gui.HTML.onchangefn(bpage, bref));
    };
  };
};

HN.Gui.HTML.make_dblclick = function(id,
                                     spage, sref, stype,
                                     bpage, bref, btype) {
  return function() {
    var values;
    var element;
    var child;
    var html ="";
    var i;
    var contents;
    values = HNGui.register.get_values(spage, sref, stype);
    element = HN.Util.id(id);
    for (i = 0; i < values.length; i += 1){
      if (values[i] === "") {
        contents = "&nbsp;";
      } else {
        contents = values[i];
      }
      html += "<p>" + contents + "</p>";
    };
    element.innerHTML = html;
    for (i = 0; i < values.length; i += 1){
      child = element.childNodes[i];
      HN.Util.addEvent(child, "dblclick",
                       HN.Gui.HTML.make_dbl(id, spage, sref, stype));
    };
  };
};

HN.Gui.HTML.make_dbl = function(id, spage, sref, stype) {
  return function(e) {
    var values;
    var element;
    var i;
    var html ="";
    var child;
    values = HNGui.register.get_values(spage, sref, stype);
    element = HN.Util.id(id);
    for (i = 0; i < values.length; i += 1){
      html += "<input type=\"text\" value=\"" + values[i] +"\"class=\"ui-state-default\" />";
    };
    element.innerHTML = html;
    for (i = 0; i < values.length; i += 1){
      child = element.childNodes[i];
      HN.Util.addEvent(child, "change", HN.Gui.HTML.onchangefn(spage, sref));
      HN.Util.addEvent(child, "blur",
                       HN.Gui.HTML.dbl_blur(id, spage, sref, stype));
      // force this element to get the focus
    }
    element.childNodes[0].focus();
  };
};

HN.Gui.HTML.dbl_blur = function(id, spage, sref, stype) {
  return function(e) {
    console.log("dblblur firing on " + spage + " " + sref);
    // Cheatin - we know that spage is the same as bpage, etc, etc
    // Double-cheatin - we are evaluating the result of make_dblclick here!
    HN.Gui.HTML.make_dblclick(id, spage, sref, stype, spage, sref, stype)();
  };
 };

HN.Gui.HTML.onchangefn = function(bpage, bref) {
  return function(e) {
    HNGui.register.set_cell_value(bpage, bref, this.value);
    HN.Gui.HTML.post(bpage, bref, e.target.value);
  };
};


/* HN.Gui.HTML.onblurfn = function(bpage, bref) {
  return function() {
    HNGui.register.set_cell_value(bpage, bref, this.value);
    HN.Gui.HTML.post(bpage, bref, this.value);
  };
}; */

HN.Gui.HTML.post = function(page, ref, value)
{
    var json = (value == "")
    ? {clear: "contents"}
    : {set: {formula: value}};

  var url = page + ref;

  $.post(url, JSON.stringify(json));

};