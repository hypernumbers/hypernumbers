/**
 * @class Hn.Gui.HTML
 * handles the behaviour of individual html elements
 */

HN.Gui.HTML = function(id, componenttype, to_page, to_ref, to_type,
                       from_page, from_ref, from_type, post) {
  var element;
  element = HN.Util.id(id);
  switch (componenttype) {
    case "block":
      return HN.Gui.HTML.make_span(id, from_page, from_ref, from_type, post);

    case "inputtext":
      return HN.Gui.HTML.make_input_text_fn(id, from_page,
                                            from_ref, from_type,
                                            to_page, to_ref,
                                            to_type, post);
    case "inputtextarea":
      return HN.Gui.HTML.make_input_textarea_fn(id, from_page,
                                                from_ref, from_type,
                                                to_page, to_ref,
                                                to_type, post);
    case "radio":
      return HN.Gui.HTML.make_radio_fn(id, from_page,
                                           from_ref, from_type,
                                           to_page, to_ref,
                                           to_type, post);
    case "checkbox":
      return HN.Gui.HTML.make_checkbox_fn(id,from_page,
                                          from_ref, from_type,
                                          to_page, to_ref,
                                          to_type, post);
    case "select":
      return HN.Gui.HTML.make_select_fn(id,from_page,
                                        from_ref, from_type,
                                        to_page, to_ref, to_type, post);
    case "dblclick_block":
      return HN.Gui.HTML.make_dblclick_block(id, from_page,
                                             from_ref, from_type,
                                             to_page, to_ref, to_type, post);
    case "table":
      return HN.Gui.HTML.make_table(id, from_page,
                                    from_ref, from_type,
                                    to_page, to_ref, to_type, post);

    case "dblclick_table":
      return HN.Gui.HTML.make_dblclick_table(id, from_page,
                                             from_ref, from_type,
                                              to_page, to_ref, to_type, post);

  default:
      return HN.Gui.HTML.nullfn;
  }
};

HN.Gui.HTML.nullfn = function() {};

HN.Gui.HTML.make_span = function(id, spage, sref, stype, post) {
  return function() {
    var value;
    var element;
    var html;
    value = HN.Gui.HTML.get_cell(spage, sref, stype);
    if (post === "auto") {
      html = "<span>" + value + "</span>";
    } else {
      html  = "<span></span>";
    }
    element = HN.Util.id(id);
    element.innerHTML = html;
  };
};

HN.Gui.HTML.make_input_text_fn = function(id,
                                          spage, sref, stype,
                                          bpage, bref, btype, post) {
  return function() {
    var value;
    var element;
    var child;
    var html;
    value = HN.Gui.HTML.get_cell(spage, sref, stype);
    if (post === "auto") {
      html = "<input type=\"text\" value=\"" + value +"\" />";
    } else {
      html = "<input type=\"text\" value=\"\" />";
    }
    element = HN.Util.id(id);
    element.innerHTML = html;
    if (post === "auto") {
      child = element.childNodes[0],
      HN.Util.addEvent(child, "change", HN.Gui.HTML.onchangefn(bpage, bref, btype));
    };
  };
};

HN.Gui.HTML.make_input_textarea_fn = function(id,
                                              spage, sref, stype,
                                              bpage, bref, btype, post) {
  return function() {
    var value;
    var element;
    var child;
    var html;
    value = HN.Gui.HTML.get_cell(bpage, bref, btype);
    if (post === "auto") {
      html = "<textarea>" + value +"</textarea>";
    } else {
      html = "<textarea></textarea>";
    }
    element = HN.Util.id(id);
    element.innerHTML = html;
    if (post === "auto") {
      child = element.childNodes[0];
      HN.Util.addEvent(child, "change", HN.Gui.HTML.onchangefn(bpage, bref, btype));
    };
  };
};

HN.Gui.HTML.make_checkbox_fn = function(id,
                                        spage, sref, stype,
                                        bpage, bref, btype, post) {
  return function() {
    var value;
    var element;
    var child;
    var html;
    value = HN.Gui.HTML.get_cell(spage, sref, stype);
    if (post === "auto") {
      html = "<input type=\"checkbox\" value=\"" + value;
      // not sure why (value === "true") and (value === true) both fail here, hmm...
      if (value == "true") {
        html += "\"  checked/>";
      } else {
        html += "\" />";
      };
    } else {
      html = "<input type=\"checkbox\" value=\"\" />";
    }
    element = HN.Util.id(id);
    element.innerHTML = html;
    if (post === "auto") {
      child = element.childNodes[0];
      HN.Util.addEvent(child, "change", HN.Gui.HTML.oncheckfn(bpage, bref, btype));
    };
  };
};

HN.Gui.HTML.make_radio_fn = function(id, spage, sref, stype,
                                     bpage, bref, btype, post) {
  return function() {
    var currentvalue;
    var values;
    var element;
    var child;
    var html;
    var x;
    var y;
    var i;
    if (post === "auto") {
      currentvalue = HN.Gui.HTML.get_cell(bpage, bref, btype);
    } else {
      currentvalue = "";
    };
    values = HNGui.register.get_values(spage, sref, stype);
    html = "<div id=\"" + id + x + "_" + y + "\" >";
    html += "<table>";
    for (i = 0; i < values.length; i += 1) {
      html += "<tr><td>" + values[i] + "</td><td>";
      html += "<input name=\"radio_" + id + "_" + i
        + "\" type=\"radio\" value=\""
        + values[i] + "\"";
      if (values[i] == currentvalue) {
        html += " checked ";
      };
      html += " /></td></tr>";
    };
    html += "</table></div>";
    element = HN.Util.id(id);
    element.innerHTML = html;
    if (post === "auto") {
      HN.Util.addEvent(element.childNodes[0], "click",
          HN.Gui.HTML.onchangefn(bpage, bref, btype));
    };
  };
};

HN.Gui.HTML.make_select_fn = function(id, spage, sref, stype,
                                      bpage, bref, btype, post) {
  return function() {
    var currentvalue;
    var values;
    var element;
    var child;
    var html;
    var x;
    var y;
    var i;
    if (post === "auto") {
      currentvalue = HN.Gui.HTML.get_cell(bpage, bref, btype);
    } else {
      currentvalue = "";
    };
    values = HNGui.register.get_values(spage, sref, stype);
    html = "<select>";
    for (i = 0; i < values.length; i += 1) {
      if (values[i]) {
        html += "<option value=\"" + values[i] + "\" ";
        if (values[i] == currentvalue) {
          html += " selected";
        };
        html += "\">" + values[i] + "</option>";
      };
    };
    html += "</select>";
    element = HN.Util.id(id);
    element.innerHTML = html;
    if (post === "auto") {
      HN.Util.addEvent(element.childNodes[0],
                         "change", HN.Gui.HTML.onchangefn(bpage, bref, btype));
    };
  };
};

HN.Gui.HTML.make_dblclick_block = function(id, spage, sref, stype,
                                           bpage, bref, btype, post) {
  return function() {
    var value;
    var element;
    var child;
    var html;
    var x;
    var y;
    var i;
    var contents;
    value = HN.Gui.HTML.get_cell(spage, sref, stype);
    if ((value === "") || (post === "manual")) {
      contents = "&nbsp;";
    } else {
      contents = value;
    }
    html = "<p>" + contents + "</p>";
    element = HN.Util.id(id);
    element.innerHTML = html;
    child = element.childNodes[0];
    HN.Util.addEvent(child, "dblclick",
                     HN.Gui.HTML.make_dbl(id, spage, sref, stype, post));
  };
};

HN.Gui.HTML.make_dbl = function(id, spage, sref, stype, post) {
  return function(e) {
    var value;
    var element;
    var html;
    var child;
    value = HN.Gui.HTML.get_cell(spage, sref, stype);
    html = "<input type=\"text\" value=\"" + value +"\"class=\"ui-state-default\" />";
    element = HN.Util.id(id);
    element.innerHTML = html;
    child = element.childNodes[0];
    if (post === "auto") {
      HN.Util.addEvent(child, "change", HN.Gui.HTML.onchangefn(spage, sref, stype));
      HN.Util.addEvent(child, "blur", HN.Gui.HTML.dbl_blur(id, spage, sref, stype));
    };
    // force this element to get the focus
    element.childNodes[0].focus();
  };
};

HN.Gui.HTML.make_table = function(id, spage, sref, stype,
                                  bpage, bref, btype, post) {
  return function() {
    var values;
    var x;
    var y;
    var html;
    var element;
    values = HNGui.register.get_range(spage, sref, stype);
    html = "<table>";
    for (y in values) {
      html +="<tr>";
      for (x in values[y]) {
        html += "<td>" + values[y][x] + "</td>";
      }
      html += "</tr>";
    }
    html += "</table>";
    element = HN.Util.id(id);
    element.innerHTML = html;
  };
};

HN.Gui.HTML.make_dblclick_table = function(id, spage, sref, stype,
                                           bpage, bref, btype, post) {
  return function() {
    var values;
    var x;
    var y;
    var table;
    var row;
    var cell;
    var new_id;
    var element;
    values = HNGui.register.get_range(spage, sref, stype);
    element = HN.Util.id(id);
    // wipe the existing element....
    element.innerHTML = "";
    table = document.createElement("table");
    for (y in values) {
      row = table.insertRow(-1);
      for (x in values[y]) {
        cell = row.insertCell(-1);
        new_id = HN.Util.get_new_id();
        cell.innerHTML = "<p id = \"" + new_id + "\">" + values[y][x] + "</a>";
        HN.Util.addEvent(cell, "dblclick",
                         HN.Gui.HTML.make_dbl(new_id, spage, HN.Util.to_b26(x) + y , "cell", post));
      };
    element.appendChild(table);
    };
  };
};

//
// functions to be bound to components
//

HN.Gui.HTML.dbl_blur = function(id, spage, sref, stype, post) {
  return function(e) {
    // Cheatin - we know that spage is the same as bpage, etc, etc
    // Double-cheatin - we are evaluating the result of make_dblclick here!
    HN.Gui.HTML.make_dblclick_block(id, spage, sref, stype, spage, sref, stype, post)();
  };
 };

HN.Gui.HTML.onchangefn = function(page, ref, type) {
  return function(e) {
    if ((type === "row") || (type === "col")) {
      HN.Gui.HTML.post(page, ref + "?last", e.target.value);
    } else {
      HNGui.register.set_cell_value(page, ref, this.value);
      HN.Gui.HTML.post(page, ref, e.target.value);
    };
  };
};

HN.Gui.HTML.oncheckfn = function(bpage, bref) {
  return function(e) {
    HNGui.register.set_cell_value(bpage, bref, e.target.checked);
    HN.Gui.HTML.post(bpage, bref, "TRUE");
    if (e.target.checked === true) {
      HN.Gui.HTML.post(bpage, bref, "TRUE");
    } else if (e.target.checked === false) {
      HN.Gui.HTML.post(bpage, bref, "FALSE");
    };
  };
};

//
// Internal functions
//
HN.Gui.HTML.get_cell = function(page, ref, type) {
  if ((type === "col") || (type === "row")) {
    return [];
  } else {
    return HNGui.register.get_values(page, ref, type);
  };
};

HN.Gui.HTML.post = function(page, ref, value)
{
    var json = (value == "")
    ? {clear: "contents"}
    : {set: {formula: value}};

  var url = page + ref;

  $.post(url, JSON.stringify(json));

};
