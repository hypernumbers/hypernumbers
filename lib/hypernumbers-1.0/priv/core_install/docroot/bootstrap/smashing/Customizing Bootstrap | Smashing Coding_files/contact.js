/**
 * @package WordPress
 * @subpackage SmashingMagazine_Theme
 * @maintainer Smashing Media <admin@smashing-media.com>
 */

var ajaxurl = globals.wp_home+'/wp-admin/admin-ajax.php';

var $j = jQuery.noConflict();

var contactForm =
{
    submit: function(event)
    {
        $j('.contactform form.smashform input[type="submit"]').attr('disabled', 'disabled');
        $j('.contactform form.smashform .container span.error').html('');

        var data = $j('.contactform form.smashform').serialize();

        if(data.length>0)
        {
            data = data+"&action=validateSmashContactFormHandler";
        } else {
            data = "action=validateSmashContactFormHandler";
        }

        $j.post(globals.wp_home+'/wp-admin/admin-ajax.php', data, function(response)
        {
            if($j.fn.jquery == '1.2.6')
            {
                response = JSON.parse(response);
            }

            $j.each(response.fields, function(index, value)
            {
                $j('.contactform form.smashform .container.'+index+' span.error').html('<p>'+value+'</p>');
            });
            $j('.contactform form.smashform input[type="submit"]').removeAttr("disabled");

            if(response.fields && response.fields.length == 0)
            {
                window.location = '/contact-form-thank-you/';
            }
        });

        return false;	
    },
}

$j(function() {

	subject_select = false;
	
	if($j('.contactform').size())
	{
		$j('.contactform form.smashform').submit(contactForm.submit);
	}
	
	initSubjectHighlighting();

	infofield_cnt = new Array();
	infofield_cnt['s_shop'] = 'To make it a bit easier for us to process your request, please always provide your order number (ex. <b>100012345</b>).';
	
	if(subject_select !== false)
	{
		toggle_infofield(subject_select);
	}

	$j('.container label').click(function()
	{
		toggle_infofield(this.id);

	});

});

function toggle_infofield(label_id)
{
	if(typeof(infofield_cnt[label_id]) !== 'undefined')
	{
		$j('.infofield').html(infofield_cnt[label_id]).show();
	}else{
		$j('.infofield').empty().hide();
	}
}

function initSubjectHighlighting()
{
	if(/\/contact?.+\//i.test(location.pathname))
	{
		var getparam = (unescape(location.search).length>0)?unescape(location.search.split('?')[1]):'';
		if(getparam !== '' && getparam !== 0)
		{
			if(isNaN(getparam))
			{
				var inputEl = $j('input[value='+getparam+']');
				if(inputEl.length>0)
				{
					subject_select = inputEl.parent('label').attr('id');
					inputEl.parent('label').html('<input type="radio" value="'+getparam+'" name="subject" class="radio" checked="checked"><span class="highlighted">'+getparam+'</span>');
				}
			}else{
				var idx = parseFloat(getparam);
				if(idx >= 1 || idx < $j('.subject label').length)
				{
					var label_el = $j('.subject label');
					var label = label_el.slice(idx,idx+1);
					var text = label.text();	
					label.html('<input type="radio" value="'+text+'" name="subject" class="radio" checked="checked"><span class="highlighted">'+text+'</span>');
				}
			}
			$j('.subject label input').change(function(e)
			{
				e.preventDefault();
				$j('.subject label span.highlighted').removeClass('highlighted');
			});
		}
	}
}