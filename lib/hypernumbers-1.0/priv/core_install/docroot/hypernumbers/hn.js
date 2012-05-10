/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true, maxerr: 10000 */
/*global hn: false, $: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */
var HN = {};

HN.navigatingAwayFromPage = false;

HN.areFunsLoaded = false;

function cssValue(css, value) { 
    
    var i, tmp, styles = css.split(";");
    
    for (i = 0; i < styles.length; i += 1) {
        tmp = styles[i].split(":");
        if (tmp[0] === value) { 
            return tmp[1];
        }
    }
    return false;
}

HN.namespace = function (name) {
    var parts = name.split('.'), cur = HN, p;
    for (p in parts) {
        if (parts.hasOwnProperty(p)) {
            if (! cur[parts[p]])  {
                cur[parts[p]] = {};
            }
            cur  = cur[parts[p]];
        }
    }
};

var Y = { 
    str       : "y", 
    coord     : "top",  
    dimension : "height",
    client    : "clientHeight",
    to_index  : function (x) {
        return x;
    }
};

var X = {
    str       : "x", 
    coord     : "left", 
    dimension : "width",
    client    : "clientWidth",
    to_index  : function (x) {
        return HN.Util.to_b26(x);
    }
};

HN.UP    = 1;
HN.DOWN  = 2;
HN.LEFT  = 3;
HN.RIGHT = 4;

/**
 * Key Constants
 */
HN.Keys           = {};
HN.Keys.ENTER     = 13;
HN.Keys.DELETE    = 46;
HN.Keys.UP        = 38;
HN.Keys.DOWN      = 40;
HN.Keys.LEFT      = 37;
HN.Keys.RIGHT     = 39;
HN.Keys.SHIFT     = 16;
HN.Keys.CTRL      = 17;
HN.Keys.TAB       = 9;
HN.Keys.ALT       = 18;
HN.Keys.BACKSPACE = 8;
HN.Keys.ESC       = 27;

/**
 * Constants relating to the current editing state of spreadsheet
 */
HN.States                  = {};
HN.States.DRAGGING         = 1;
HN.States.SELECTED_RANGE   = 2;
HN.States.SELECTED_CELL    = 3;
HN.States.EDIT_LIGHT_CELL  = 4;
HN.States.EDIT_LIGHT_RANGE = 5;
HN.States.EDIT_FULL_CELL   = 6;
HN.States.NOT_EDITING      = 7;
HN.States.COPY_URL         = 8;
HN.States.PAINTER          = 9;

HN.DEFAULT_COLOUR = {
    "name"   : "recent picks",
    "combos" : [{"background-color": "FFFFFF", "color": "000000"},
                {"background-color": "FFFFFF", "color": "000000"},
                {"background-color": "FFFFFF", "color": "000000"},
                {"background-color": "FFFFFF", "color": "000000"},
                {"background-color": "FFFFFF", "color": "000000"}]
};

HN.THEMES = [ 
    {"urm"    : "gustav", 
     "name"   : "cmon up",
     "combos" : [{"background-color": "E4FFE6", "color": "000000"},
                 {"background-color": "D4FF00", "color": "000000"},
                 {"background-color": "00D6DD", "color": "000000"},
                 {"background-color": "68776C", "color": "FFFFFF"},
                 {"background-color": "13140F", "color": "FFFFFF"}]},
    {"urm"    : "nathaniel", 
     "name"   : "1944mustang",
     "combos" : [{"background-color": "FFFFFF", "color": "000000"},
                 {"background-color": "FF9800", "color": "000000"},
                 {"background-color": "7E8AA2", "color": "FFFFFF"},
                 {"background-color": "263248", "color": "FFFFFF"},
                 {"background-color": "000000", "color": "FFFFFF"}]},
    {"urm"    : "matthepworth", 
     "name"   : "pistachio",
     "combos" : [{"background-color": "F6E8B1", "color": "000000"},
                 {"background-color": "B0CC99", "color": "000000"},
                 {"background-color": "B7CA79", "color": "000000"},
                 {"background-color": "89725B", "color": "FFFFFF"},
                 {"background-color": "677E52", "color": "FFFFFF"}]}
];

HN.BGCOMBO = [
    ["macevamel", "japanese garden",
     [["D8CAA8", "000000"],
      ["5C832F", "FFFFFF"],
      ["363942", "FFFFFF"],
      ["284907", "FFFFFF"],
      ["382513", "FFFFFF"]]],
    ["ps", "sandy stone beach ocean",
     [["EFECCA", "000000"],
      ["E6E2AF", "000000"],
      ["A7A37E", "000000"],
      ["046380", "FFFFFF"],
      ["002F2F", "FFFFFF"]]],
    ["klynch", "dolores",
     [["FFFAE4", "000000"],
      ["E8E595", "000000"],
      ["D0A825", "000000"],
      ["40627C", "FFFFFF"],
      ["26393D", "FFFFFF"]]],
    ["matthepworth", "salamander",
     [["CEF09D", "000000"],
      ["B8ECD7", "000000"],
      ["B1E001", "000000"],
      ["476C5E", "FFFFFF"],
      ["083643", "FFFFFF"]]],
    ["tajeri68", "aspirin c",
     [["F3FFE2", "000000"],
      ["ACF0F2", "000000"],
      ["EB7F00", "000000"],
      ["1695A3", "FFFFFF"],
      ["225378", "FFFFFF"]]],
    ["craigmillar101", "first impressions",
     [["E4F8FF", "000000"],
      ["FFF0A3", "000000"],
      ["B8CC6E", "000000"],
      ["4B6000", "FFFFFF"],
      ["004460", "FFFFFF"]]],
    ["whoneycutt", "business casual",
     [["F4EFDC", "000000"],
      ["BA9B65", "000000"],
      ["799AE0", "000000"],
      ["365FB7", "FFFFFF"],
      ["133463", "FFFFFF"]]],
    ["matthepworth", "firenze",
     [["FFF0A5", "000000"],
      ["FFB03B", "000000"],
      ["468966", "FFFFFF"],
      ["B64926", "FFFFFF"],
      ["8E2800", "FFFFFF"]]],
    ["whoneycutt", "herbs and spice",
     [["FDE792", "000000"],
      ["A9CC66", "000000"],
      ["D1570D", "FFFFFF"],
      ["477725", "FFFFFF"],
      ["5A1F00", "FFFFFF"]]],
    ["donald.agarrat", "cherry cheesecake",
     [["FCFAE1", "000000"],
      ["F6E497", "000000"],
      ["BD8D46", "000000"],
      ["B9121B", "FFFFFF"],
      ["4C1B1B", "FFFFFF"]]],
    ["matthepworth", "bordeaux",
     [["F7F2B2", "000000"],
      ["ADCF4F", "000000"],
      ["84815B", "FFFFFF"],
      ["8E3557", "FFFFFF"],
      ["4A1A2C", "FFFFFF"]]],
    ["erin", "tech office",
     [["FFFFFF", "000000"],
      ["ACCFCC", "000000"],
      ["B8AE9C", "000000"],
      ["595241", "FFFFFF"],
      ["8A0917", "FFFFFF"]]],
    ["info", "red and smooth",
     [["FBF7E4", "000000"],
      ["E7E8D1", "000000"],
      ["D3CEAA", "000000"],
      ["424242", "FFFFFF"],
      ["8E001C", "FFFFFF"]]],
    ["koralute", "ocean sunset",
     [["FFD393", "000000"],
      ["FF974F", "000000"],
      ["9C9B7A", "000000"],
      ["F54F29", "FFFFFF"],
      ["405952", "FFFFFF"]]],
    ["mats.holmberg", "orange on gray",
     [["FFF8E3", "000000"],
      ["CCCC9F", "000000"],
      ["9FB4CC", "000000"],
      ["DB4105", "FFFFFF"],
      ["33332D", "FFFFFF"]]],
    ["whoneycutt", "quiet cry",
     [["EEEFF7", "000000"],
      ["92CDCF", "000000"],
      ["445878", "FFFFFF"],
      ["31353D", "FFFFFF"],
      ["1C1D21", "FFFFFF"]]],
    ["kristi", "optimist",
     [["F4F7D9", "000000"],
      ["A4CFBE", "000000"],
      ["6C6E58", "FFFFFF"],
      ["417378", "FFFFFF"],
      ["3E423A", "FFFFFF"]]],
    ["vixo", "dark greys",
     [["555555", "FFFFFF"],
      ["444444", "FFFFFF"],
      ["333333", "FFFFFF"],
      ["222222", "FFFFFF"],
      ["111111", "FFFFFF"]]],
    ["vixo", "bluish",
     [["0DB2FF", "000000"],
      ["0C68E8", "FFFFFF"],
      ["6A0DFF", "FFFFFF"],
      ["0028FF", "FFFFFF"],
      ["240CE8", "FFFFFF"]]],
    ["vixo", "reddish",
     [["FF530D", "FFFFFF"],
      ["FF0DFF", "FFFFFF"],
      ["E82C0C", "FFFFFF"],
      ["E80C7A", "FFFFFF"],
      ["FF0000", "FFFFFF"]]],
    ["vixo", "greenish",
     [["8EFF07", "000000"],
      ["07FFB2", "000000"],
      ["13FF31", "000000"],
      ["33E806", "000000"],
      ["06E862", "000000"]]],
    ["vixo", "yellowish",
     [["DBFF03", "000000"],
      ["FFD80A", "000000"],
      ["E8DA02", "000000"],
      ["FFA403", "000000"],
      ["E8AC02", "000000"]]],
    ["vixo", "light greys",
     [["EEEEEE", "000000"],
      ["DDDDDD", "000000"],
      ["CCCCCC", "000000"],
      ["BBBBBB", "000000"],
      ["AAAAAA", "000000"]]]
];

HN.TEXTCOMBO = [
    ["gustav", "cmon up",
     [["000000", "E4FFE6"],
      ["000000", "D4FF00"],
      ["000000", "00D6DD"],
      ["FFFFFF", "68776C"],
      ["FFFFFF", "13140F"]]],
    ["nathaniel", "1944mustang",
     [["000000", "FFFFFF"],
      ["000000", "FF9800"],
      ["000000", "7E8AA2"],
      ["FFFFFF", "263248"],
      ["FFFFFF", "000000"]]],
    ["matthepworth", "pistachio",
     [["000000", "F6E8B1"],
      ["000000", "B0CC99"],
      ["000000", "B7CA79"],
      ["FFFFFF", "89725B"],
      ["FFFFFF", "677E52"]]],
    ["macevamel", "japanese garden",
     [["000000", "D8CAA8"],
      ["FFFFFF", "5C832F"],
      ["FFFFFF", "363942"],
      ["FFFFFF", "284907"],
      ["FFFFFF", "382513"]]],
    ["ps", "sandy stone beach ocean",
     [["000000", "EFECCA"],
      ["000000", "E6E2AF"],
      ["000000", "A7A37E"],
      ["FFFFFF", "046380"],
      ["FFFFFF", "002F2F"]]],
    ["klynch", "dolores",
     [["000000", "FFFAE4"],
      ["000000", "E8E595"],
      ["000000", "D0A825"],
      ["FFFFFF", "40627C"],
      ["FFFFFF", "26393D"]]],
    ["matthepworth", "salamander",
     [["000000", "CEF09D"],
      ["000000", "B8ECD7"],
      ["000000", "B1E001"],
      ["FFFFFF", "476C5E"],
      ["FFFFFF", "083643"]]],
    ["tajeri68", "aspirin c",
     [["000000", "F3FFE2"],
      ["000000", "ACF0F2"],
      ["000000", "EB7F00"],
      ["FFFFFF", "1695A3"],
      ["FFFFFF", "225378"]]],
    ["craigmillar101", "first impressions",
     [["000000", "E4F8FF"],
      ["000000", "FFF0A3"],
      ["000000", "B8CC6E"],
      ["FFFFFF", "4B6000"],
      ["FFFFFF", "004460"]]],
    ["whoneycutt", "business casual",
     [["000000", "F4EFDC"],
      ["000000", "BA9B65"],
      ["000000", "799AE0"],
      ["FFFFFF", "365FB7"],
      ["FFFFFF", "133463"]]],
    ["matthepworth", "firenze",
     [["000000", "FFF0A5"],
      ["000000", "FFB03B"],
      ["FFFFFF", "468966"],
      ["FFFFFF", "B64926"],
      ["FFFFFF", "8E2800"]]],
    ["whoneycutt", "herbs and spice",
     [["000000", "FDE792"],
      ["000000", "A9CC66"],
      ["FFFFFF", "D1570D"],
      ["FFFFFF", "477725"],
      ["FFFFFF", "5A1F00"]]],
    ["donald.agarrat", "cherry cheesecake",
     [["000000", "FCFAE1"],
      ["000000", "F6E497"],
      ["000000", "BD8D46"],
      ["FFFFFF", "B9121B"],
      ["FFFFFF", "4C1B1B"]]],
    ["matthepworth", "bordeaux",
     [["000000", "F7F2B2"],
      ["000000", "ADCF4F"],
      ["FFFFFF", "84815B"],
      ["FFFFFF", "8E3557"],
      ["FFFFFF", "4A1A2C"]]],
    ["erin", "tech office",
     [["000000", "FFFFFF"],
      ["000000", "ACCFCC"],
      ["000000", "B8AE9C"],
      ["FFFFFF", "595241"],
      ["FFFFFF", "8A0917"]]],
    ["info", "red and smooth",
     [["000000", "FBF7E4"],
      ["000000", "E7E8D1"],
      ["000000", "D3CEAA"],
      ["FFFFFF", "424242"],
      ["FFFFFF", "8E001C"]]],
    ["koralute", "ocean sunset",
     [["000000", "FFD393"],
      ["000000", "FF974F"],
      ["000000", "9C9B7A"],
      ["FFFFFF", "F54F29"],
      ["FFFFFF", "405952"]]],
    ["mats.holmberg", "orange on gray",
     [["000000", "FFF8E3"],
      ["000000", "CCCC9F"],
      ["000000", "9FB4CC"],
      ["FFFFFF", "DB4105"],
      ["FFFFFF", "33332D"]]],
    ["whoneycutt", "quiet cry",
     [["000000", "EEEFF7"],
      ["000000", "92CDCF"],
      ["FFFFFF", "445878"],
      ["FFFFFF", "31353D"],
      ["FFFFFF", "1C1D21"]]],
    ["kristi", "optimist",
     [["000000", "F4F7D9"],
      ["000000", "A4CFBE"],
      ["FFFFFF", "6C6E58"],
      ["FFFFFF", "417378"],
      ["FFFFFF", "3E423A"]]],
    ["vixo", "dark greys",
     [["FFFFFF", "555555"],
      ["FFFFFF", "444444"],
      ["FFFFFF", "333333"],
      ["FFFFFF", "222222"],
      ["FFFFFF", "111111"]]],
    ["vixo", "bluish",
     [["000000", "0DB2FF"],
      ["FFFFFF", "0C68E8"],
      ["FFFFFF", "6A0DFF"],
      ["FFFFFF", "0028FF"],
      ["FFFFFF", "240CE8"]]],
    ["vixo", "reddish",
     [["FFFFFF", "FF530D"],
      ["FFFFFF", "FF0DFF"],
      ["FFFFFF", "E82C0C"],
      ["FFFFFF", "E80C7A"],
      ["FFFFFF", "FF0000"]]],
    ["vixo", "greenish",
     [["000000", "8EFF07"],
      ["000000", "07FFB2"],
      ["000000", "13FF31"],
      ["000000", "33E806"],
      ["000000", "06E862"]]],
    ["vixo", "yellowish",
     [["000000", "DBFF03"],
      ["000000", "FFD80A"],
      ["000000", "E8DA02"],
      ["000000", "FFA403"],
      ["000000", "E8AC02"]]],
    ["vixo", "light greys",
     [["000000", "EEEEEE"],
      ["000000", "DDDDDD"],
      ["000000", "CCCCCC"],
      ["000000", "BBBBBB"],
      ["000000", "AAAAAA"]]]
];
