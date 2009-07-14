/**
 * @class DF.Util
 *
 */
DF.Util = {};

DF.Util.make_proper = function(value) {
  if (value[value.length] !== "/") {
    value += "/";
  };
  if (value[0] !== "/") {
    value = "/" + value;
  };
  return value;
};


DF.Util.get_max = function(page) {
  // we are looking for the maximum cell with a set value
  // not the maximum cell with anything (ie styles but no values don't count)
  var nFlag;
  var tempMaxX;
  var tempMaxY;
  tempMaxX = 1;
  tempMaxY = 1;
  for( var i in page.data.cell ) {
    nFlag = false;
    if( parseInt(i) > page.max.y) {
      tempMaxY = parseInt(i);
      //console.log("tempMaxY is " + tempMaxY);
    }
    for ( var j in page.data.cell[i] ) {
      if (page.data.cell[i][j].value) {
        nFlag = true;
        if(parseInt(j) > page.max.x ) {
        //console.log("tempMaxX is " + tempMaxX);
          tempMaxX = parseInt(j);
        }
      }
    }
    // only update the max's if there was a value anywhere in the row
    if (nFlag === true) {
      page.max.y = tempMaxY;
      page.max.x = tempMaxX;
    }
  }
  return page.max;
};

DF.Util.get_value = function(data, x, y) {
  var cell;
  cell = data.data.cell;
  if (cell[y] &&  cell[y][x]
      && cell[y][x].value) {
    return cell[y][x].value;
    } else {
      //console.log("wigging out!");
      return "";
    }
};

