var HN = {};

var Y = {str:"y", coord:"top", dimension:"height",
         to_index: function(x) { return x; }};
var X = {str:"x", coord:"left",dimension:"width",
         to_index: function(x) { return HN.Util.to_b26(x); }};

HN.UP = 1;
HN.DOWN = 2;
HN.LEFT = 3;
HN.RIGHT = 4;
