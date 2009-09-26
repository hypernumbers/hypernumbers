HN.UIActions = {};

HN.UIActions.open_dialog = function(layout, id)
{
  var dialog = $("#"+id),
         css = {"margin-left": -(dialog.width() /2),
                "margin-top" : -(dialog.height()/2)};

  layout.selection.state = HN.States.NOT_EDITING;

  $("#cover").fadeIn("fast");
  dialog.css(css).fadeIn("fast").find("input").eq(0).focus();
};

HN.UIActions.close_dialog = function(layout, id)
{
  var sel = layout.selection;

  sel.state = (sel.bounds.y1 == sel.bounds.y2
               && sel.bounds.x1 == sel.bounds.x2)
    ? HN.States.SELECTED_CELL : HN.States.SELECTED_RANGE;

  $("#"+id).fadeOut("fast");
  $("#cover").fadeOut("fast");
};