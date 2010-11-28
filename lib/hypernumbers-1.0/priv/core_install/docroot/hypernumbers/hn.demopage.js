/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true */
/*global $: false, window: false  */
var height = $(window).height();
$("#hn_spreadsheet").css("height", (height - 100) / 2 + "px");
$("#hn_wikipage").css("height", (height - 130) / 2 + "px");
$("#hn_webpage").css("height", (height - 130) / 2 + "px");