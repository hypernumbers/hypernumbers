/*jslint browser: true, undef: true, eqeqeq: true, nomen: true, white: true , maxerr: 10000 */
/*global HN: false, alert: false,  $: false, Y: false, X: false, localStorage: false, escape: false, unescape: false, window: false, jscolor: false */

HN.RichText = function () {
 var currentEditor = "",
    getCurrentEditor,
    setCurrentEditor;

    getCurrentEditor = function () {
        return currentEditor;
    };

    setCurrentEditor = function(div) {
        currentEditor = div
    };
    
};