var HN = {};

var Y = {str:"y", coord:"top",  dimension:"height",
         to_index: function(x) { return x; }};
var X = {str:"x", coord:"left", dimension:"width",
         to_index: function(x) { return HN.Util.to_b26(x); }};

/* definitions of what to enable between alpha / beta */
HN.INSTALL = "alpha";

HN.LANGUAGES = {};
HN.LANGUAGES["alpha"] = ["en_gb", "fr", "ru", "de", "es", "br", "it", "pt", "en_us"];
HN.LANGUAGES["beta"]  = ["en_gb", "ru"];

HN.TOSHOW = {};
HN.TOSHOW["alpha"] = ["borders", "bordersep", "importmenu", "font_colour"];
HN.TOSHOW["beta"]  = [];

HN.UP    = 1;
HN.DOWN  = 2;
HN.LEFT  = 3;
HN.RIGHT = 4;

/**
 * Key Constants
 */
HN.Keys = {};
HN.Keys.ENTER  = 13;
HN.Keys.DELETE = 46;
HN.Keys.UP     = 38;
HN.Keys.DOWN   = 40;
HN.Keys.LEFT   = 37;
HN.Keys.RIGHT  = 39;
HN.Keys.SHIFT  = 16;
HN.Keys.CTRL   = 17;
HN.Keys.TAB    = 9;
HN.Keys.ALT    = 18;
HN.Keys.BACKSPACE = 8;

/**
 * Constants relating to the current editing state of spreadsheet
 */
HN.States = {};
HN.States.DRAGGING = 1;
HN.States.SELECTED_RANGE = 2;
HN.States.SELECTED_CELL = 3;
HN.States.EDIT_LIGHT_CELL = 4;
HN.States.EDIT_LIGHT_RANGE = 5;
HN.States.EDIT_FULL_CELL = 6;
HN.States.NOT_EDITING = 7;
HN.States.COPY_URL = 8;

