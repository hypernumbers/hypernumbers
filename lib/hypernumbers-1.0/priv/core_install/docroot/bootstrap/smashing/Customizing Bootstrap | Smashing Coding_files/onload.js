/**
 * @package WordPress
 * @subpackage SmashingMagazine_Theme
 * @maintainer Smashing Media <admin@smashing-media.com>
 */

var $j = jQuery.noConflict();

$j(function() {
	$j("select#subsection-select").change(function () { location.href = $j(this).val(); });
	$j("select#networkselection").change(function () { location.href = $j(this).val(); });
	
	$j('a img').parents('a').css({border: 'none'});
	$j('img').removeAttr('width').removeAttr('height');
	
	if($j(".sub-tabs").size())
	{
	   var subTabber = jQuery( '.subtab-pages > div' );
	   $j( '.sub-tabs li > a' ).click(function()
	   {
	       subTabber.hide();
	       subTabber.filter(this.hash).show();
	       $j( '.sub-tabs li > a' ).removeClass( 'active' );
	       $j(this).addClass( 'active' );
	       return false;
	   }).filter( ':first' ).click();
	}

	$j.each($j(".reporttable"), function(index, value) {
		var table_id = this.id;
		
		$j("#"+table_id+" tr:odd").addClass("odd");
		$j("#"+table_id+" tr:not(.odd)").hide();
		$j("#"+table_id+" tr:first-child").show();
		$j("#"+table_id+" tr.odd").click(function(){
			$j(this).next("tr").toggle();
			$j(this).find(".arrow").toggleClass("up");
		});
	});
});
