/**
 * @class Hn.Gui.HTML
 * handles the behaviour of individual html elements
 */

HN.Gui.HTML = function(id, componenttype, to_obj,
                       from_page, from_ref, from_type,
                       post, validity, errors) {
  var element;
  element = HN.Util.id(id);
  if (validity === "invalid") {
    return HN.Gui.HTML.make_error(id, componenttype,
                                  from_page, from_ref, from_type,
                                  to_obj, post, errors);
  }
  switch (componenttype) {
    case "block":
      return HN.Gui.HTML.make_span(id, from_page, from_ref, from_type,
                                   to_obj, post);

    case "inputtext":
      return HN.Gui.HTML.make_input_text_fn(id, from_page,
                                            from_ref, from_type,
                                            to_obj, post);
    case "inputtextarea":
      return HN.Gui.HTML.make_input_textarea_fn(id, from_page,
                                                from_ref, from_type,
                                                to_obj, post);
    case "radio":
      return HN.Gui.HTML.make_radio_fn(id, from_page,
                                           from_ref, from_type,
                                           to_obj, post);
    case "checkbox":
      return HN.Gui.HTML.make_checkbox_fn(id,from_page,
                                          from_ref, from_type,
                                          to_obj, post);
    case "select":
      return HN.Gui.HTML.make_select_fn(id,from_page,
                                        from_ref, from_type,
                                        to_obj, post);
    case "dblclick_block":
      return HN.Gui.HTML.make_dblclick_block(id, from_page,
                                             from_ref, from_type,
                                             to_obj, post);
    case "table":
      return HN.Gui.HTML.make_table(id, from_page,
                                    from_ref, from_type,
                                    to_obj, post);

    case "dblclick_table":
      return HN.Gui.HTML.make_dblclick_table(id, from_page,
                                             from_ref, from_type,
                                              to_obj, post);

  default:
     return HN.Gui.HTML.nullfn;
  }
};

HN.Gui.HTML.nullfn = function() {};

HN.Gui.HTML.make_span = function(id, from_page, from_ref, from_type,
                                 to_obj, post) {
  return function() {
    var value;
    var element;
    var html;
    value = HN.Gui.HTML.get_cell(from_page, from_ref, from_type);
    html = "<span>" + value + "</span>";
    element = HN.Util.id(id);
    element.innerHTML = html;
  };
};

HN.Gui.HTML.make_input_text_fn = function(id,
                                          from_page, from_ref, from_type,
                                          to_obj, post) {
  return function() {
    var value;
    var element;
    var child;
    var html;
    value = HN.Gui.HTML.get_cell(from_page, from_ref, from_type);
    html = "<input type=\"text\" value=\"" + value +"\" />";
    element = HN.Util.id(id);
    element.innerHTML = html;
    if (post === "auto") {
      child = element.childNodes[0],
      HN.Util.addEvent(child, "change", HN.Gui.HTML.onchangefn(to_obj));
    };
  };
};

HN.Gui.HTML.make_input_textarea_fn = function(id,
                                              from_page, from_ref, from_type,
                                              to_obj, post) {
  return function() {
    var value;
    var element;
    var child;
    var html;
    value = HN.Gui.HTML.get_cell(from_page, from_ref, from_type);
    html = "<textarea>" + value +"</textarea>";
    element = HN.Util.id(id);
    element.innerHTML = html;
    if (post === "auto") {
      child = element.childNodes[0];
      HN.Util.addEvent(child, "change", HN.Gui.HTML.onchangefn(to_obj));
    };
  };
};

HN.Gui.HTML.make_checkbox_fn = function(id,
                                        from_page, from_ref, from_type,
                                        to_obj, post) {
  return function() {
    var value;
    var element;
    var child;
    var html;
    value = HN.Gui.HTML.get_cell(from_page, from_ref, from_type);
    html = "<input type=\"checkbox\" value=\"" + value;
    // not sure why (value === "true") and (value === true) both fail here, hmm...
    if (value == "true") {
      html += "\"  checked/>";
    } else {
      html += "\" />";
    };
    element = HN.Util.id(id);
    element.innerHTML = html;
    if (post === "auto") {
      child = element.childNodes[0];
      HN.Util.addEvent(child, "change", HN.Gui.HTML.oncheckfn(to_obj));
    };
  };
};

HN.Gui.HTML.make_radio_fn = function(id, from_page, from_ref, from_type,
                                     to_obj, post) {
  return function() {
    var to_page;
    var to_ref;
    var to_type;
    var currentvalue;
    var values;
    var element;
    var child;
    var html;
    var x;
    var y;
    var i;
    // mark the value of the radio function by the cell of the first
    // 'to' reference (if it is not a column or a row)
    to_page = to_obj[0].page;
    to_ref = to_obj[0].ref;
    to_type = to_obj[0].type;
    if (to_type === "cell"
        || to_type === "row_from_last"
        || to_type === "col_from_last") {
      currentvalue = HN.Gui.HTML.get_cell(to_page, to_ref, to_type);
    } else {
      currentvalue = "";
    }
    values = HNGui.register.get_values(from_page, from_ref, from_type);
    html = "<table>";
    for (i = 0; i < values.length; i += 1) {
      html += "<tr><td>" + values[i] + "</td><td>";
      html += "<input name=\"radio_" + id
        + "\" type=\"radio\" value=\""
        + values[i] + "\"";
      if (values[i] == currentvalue) {
        html += " checked ";
      };
      html += " /></td></tr>";
    };
    html += "</table>";
    element = HN.Util.id(id);
    element.innerHTML = html;
    if (post === "auto") {
      HN.Util.addEvent(element.childNodes[0], "click",
          HN.Gui.HTML.onchangefn(to_obj));
    };
  };
};

HN.Gui.HTML.make_select_fn = function(id, from_page, from_ref, from_type,
                                      to_obj, post) {
  return function() {
    var to_page;
    var to_ref;
    var to_type;
    var currentvalue;
    var values;
    var element;
    var child;
    var html;
    var x;
    var y;
    var i;
    to_page = to_obj[0].page;
    to_ref = to_obj[0].ref;
    to_type = to_obj[0].type;
    if (to_type === "cell"
        || to_type === "row_from_last"
        || to_type === "col_from_last") {
      currentvalue = HN.Gui.HTML.get_cell(to_page, to_ref, to_type);
    } else {
      currentvalue = "";
    }
    values = HNGui.register.get_values(from_page, from_ref, from_type);
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
                         "change", HN.Gui.HTML.onchangefn(to_obj));
    };
  };
};

HN.Gui.HTML.make_dblclick_block = function(id, from_page, from_ref, from_type,
                                           to_obj, post) {
  return function() {
    var value;
    var element;
    var child;
    var html;
    var x;
    var y;
    var i;
    var contents;
    value = HN.Gui.HTML.get_cell(from_page, from_ref, from_type);
    if (value === "") {
      contents = "&nbsp;";
    } else {
      contents = value;
    }
    html = "<p>" + contents + "</p>";
    element = HN.Util.id(id);
    element.innerHTML = html;
    child = element.childNodes[0];
    HN.Util.addEvent(child, "dblclick",
                     HN.Gui.HTML.make_dbl(id, from_page, from_ref,
                                          from_type, to_obj, post));
  };
};

HN.Gui.HTML.make_dbl = function(id, from_page, from_ref, from_type, to_obj, post) {
  return function(e) {
    var value;
    var element;
    var html;
    var child;
    value = HN.Gui.HTML.get_cell(from_page, from_ref, from_type);
    html = "<input type=\"text\" value=\"" + value +"\"class=\"ui-state-default\" />";
    element = HN.Util.id(id);
    element.innerHTML = html;
    child = element.childNodes[0];
    if (post === "auto") {
      HN.Util.addEvent(child, "change", HN.Gui.HTML.onchangefn(to_obj));
      HN.Util.addEvent(child, "blur", HN.Gui.HTML.dbl_blur(id, from_page, from_ref, from_type));
    };
    // force this element to get the focus
    element.childNodes[0].focus();
  };
};

HN.Gui.HTML.make_table = function(id, from_page, from_ref, from_type,
                                  to_obj, post) {
  return function() {
    var values;
    var x;
    var y;
    var html;
    var element;
    values = HNGui.register.get_range(from_page, from_ref, from_type);
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

HN.Gui.HTML.make_dblclick_table = function(id, from_page, from_ref, from_type,
                                           to_obj, post) {
  return function() {
    var values;
    var x;
    var y;
    var table;
    var row;
    var cell;
    var new_id;
    var element;
    values = HNGui.register.get_range(from_page, from_ref, from_type);
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
                         HN.Gui.HTML.make_dbl(new_id, from_page, HN.Util.to_b26(x) + y , "cell", post));
      };
    element.appendChild(table);
    };
  };
};

HN.Gui.HTML.make_error = function(id, componenttype,
                                  from_page, from_ref, from_type,
                                  to_obj, post, errors) {
  return function() {
    var text;
    var i;
    var to_page;
    var to_ref;
    var to_type;
    var err;
    var element;
    text = "<div style=\"{border: 1px solid red}\"><p><font color=\"red\">Error:</font> "
      + "<small>Control of type: <code>" +  componenttype + "<code> is invalid . "
      + "<strong>From Bindings</strong> are: " + from_page + " "
      + from_ref + " " + from_type + ".  "
      + "<p>Errors are:</p><small>";
    for (i in to_obj) {
      to_page = to_obj[i].page;
      to_ref = to_obj[i].ref;
      to_type = to_obj[i].type;
      text += "<strong>To Bindings</strong> are: " + to_page + " "
        + to_ref + " " + to_type + ". "
        + "<strong>Post</strong> is " + post + "</p>";
    };
    for (err in errors) {
        text += "<p>" + errors[err] + "</p>";
    }
    element = HN.Util.id(id);
    element.innerHTML = text + "</small></div>";
  };
};


//
// functions to be bound to components
//

HN.Gui.HTML.dbl_blur = function(id, from_page, from_ref, from_type, to_obj, post) {
  return function(e) {
    // Double-cheatin - we are evaluating the result of make_dblclick here!
    HN.Gui.HTML.make_dblclick_block(id, from_page, from_ref, to_obj, post)();
  };
 };

HN.Gui.HTML.onchangefn = function(to_obj) {
  return function(e) {
    var i;
    var page;
    var ref;
    var type;
    for (i in to_obj) {
      page = to_obj[i].page;
      ref = to_obj[i].ref;
      type = to_obj[i].type;
      if ((type === "row") || (type === "col")) {
        HN.Gui.HTML.post(page, ref + "?last", e.target.value);
      } else {
        HNGui.register.set_cell_value(page, ref, this.value);
        HN.Gui.HTML.post(page, ref, e.target.value);
      };
    };
  };
};

HN.Gui.HTML.oncheckfn = function(to_obj) {
  return function(e) {
    var i;
    var page;
    var ref;
    for (i in to_obj) {
      page = to_obj[i].page;
      ref = to_obj[i].ref;
      HNGui.register.set_cell_value(to_page, to_ref, e.target.checked);
      HN.Gui.HTML.post(to_page, to_ref, "TRUE");
      if (e.target.checked === true) {
        HN.Gui.HTML.post(to_page, to_ref, "TRUE");
      } else if (e.target.checked === false) {
        HN.Gui.HTML.post(to_page, to_ref, "FALSE");
      };
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
