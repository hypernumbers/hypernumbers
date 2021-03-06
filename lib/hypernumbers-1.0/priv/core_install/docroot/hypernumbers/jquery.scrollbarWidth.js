/*!
* jQuery Scrollbar Width v1.0
*
* Copyright 2011, Rasmus Schultz
* Licensed under LGPL v3.0
* http://www.gnu.org/licenses/lgpl-3.0.txt
* 
* http://plugins.jquery.com/project/scrollbarWidth
*/

(function($){

$.scrollbarWidth = function() {
  if (!$._scrollbarWidth) {
     var $body = $('body');
    var w = $body.css('overflow', 'hidden').width();
    $body.css('overflow','scroll');
    w -= $body.width();
    if (!w) w=$body.width()-$body[0].clientWidth; // IE in standards mode
    $body.css('overflow','');
    $._scrollbarWidth = w;
  }
  return $._scrollbarWidth;
};

})(jQuery);

