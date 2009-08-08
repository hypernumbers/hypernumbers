/**
 * @class HN.Gui.Util
 */

HN.Gui.Util = {};

HN.Gui.Util.is_member = function(val, array) {
  console.log("val is " + val + " and seeking it in array of:");
  console.log(array);
  var i;
  for (i = 0; i < array.length; i += 1) {
    if (array[i] === val) {
      return true;
    };
  };
  return false;
};