var $j = jQuery.noConflict();

$j(function() {
	"use strict";
	
	var links = $j('.single article.post p a[href]:not([href^=#],[href^=mailto],[rel=nofollow]), .single article.post ul:not(.postmetadata) a[href]:not([href^=#],[href^=mailto],[rel=nofollow])');
	
	var notelist = $j('<ul class="print_only_notelist"></ul>').insertAfter($j('.single article'));
	var count = 0;
	
	$j('<li>', { html: '<h4>Footnotes:</h4>' }).appendTo(notelist);
	$j.each(links, function(i){
		var link_url = $j(this).attr('href');
		var link_text = $j(this).text();
		if($j(this).children('img').length === 0 && link_url !== '' && (/^https?:\/\//.test(link_url)) && link_url.search(/\.(jpg|jpeg|gif|png|ico|css|js|zip|tgz|gz|rar|bz2|doc|xls|exe|ppt|txt|tar|mid|midi|wav|bmp|mp3)/) === -1 && link_url.search(/(original_referer|auslieferung)/) === -1)
		{
			count = count+1;
			$j('<sup>',{text:''+count+''}).addClass('print_only').insertAfter($j(this));	
			var ftext = '';			
			if(link_text.length > 0)
			{
				ftext = link_text+' - '+link_url;
			}else{
				ftext = link_url;
			}
			$j('<li>', { html: '<sup>'+count+'</sup> '+ftext }).appendTo(notelist);
		}
	});
});