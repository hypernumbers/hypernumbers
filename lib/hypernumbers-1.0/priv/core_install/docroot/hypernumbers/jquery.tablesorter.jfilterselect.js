/* Based on code from http://www.jordigirones.com/131-filtros-desplegables-con-tablesorter.html */

$.tablesorter.addWidget({
  id: 'jFilterSelect',
  format: function(element){
    var __jFilter = this;
    var table = $(element);
    var table_id = table.attr('id');
    if (table_id==false){ table_id = 'jFTable-'+Math.round(Math.random()*10000); table.attr('id',table_id);}
    if ($('#filters-for-'+table_id).length==0 && $('th[jFilterSelect="1"]').length>0){
      $('thead tr',table).after('<tr id="filters-for-'+table_id+'"></tr>');
      $('th',table).each(function(n,e){
        $('#filters-for-'+table_id).append('<th id="filter-column-'+table_id+'-'+n+'">Filter </th>');
        if ($(e).attr('jFilterSelect')==1){
          $('#filter-column-'+table_id+'-'+n).append('<select id="filter-select-for-'+table_id+'-column-'+n+'"></select>');
          $('#filter-column-'+table_id+'-'+n).append('<input id="filter-input-for-'+table_id+'-column-'+n+'" type="text" size="10" />');

          var options = ['<option value="(All)">(Select All)</option>'];
          var optionsAdded = [];
          $('td:nth-child('+(n+1)+')',table).each(function(o,itm){
            var tItm = $(itm).text();
            if ((optionsAdded.length < 16) && ($.inArray(tItm,optionsAdded)<0)){
              options[options.length] = '<option value="'+tItm+'">'+((tItm == '') ? '(Blank)' : tItm)+'</option>';
              optionsAdded[optionsAdded.length] = tItm;
            }
          });
	  if (optionsAdded.length >= 16) {
              $('#filter-select-for-'+table_id+'-column-'+n).remove();
	  } else {
              $('#filter-select-for-'+table_id+'-column-'+n).html(options.join('')).bind('change',function(ev){
                __jFilter.jFilterRun(element);
              });
	  }
          $('#filter-input-for-'+table_id+'-column-'+n).bind('keyup',function(ev){
            __jFilter.jFilterRun(element);
          });
        }
      });
    }
  },
  jFilterRun: function(table){
    $('tbody tr:hidden', table).show();
    $('tbody tr',$(table)).each(function(n,r){
      $('td',$(r)).each(function(i,f){
        $(f).attr('passFilter',true);
        var myFilterColumn = $('#filter-select-for-'+$(table).attr('id')+'-column-'+i);
        if ((myFilterColumn.length) && (myFilterColumn.val() != '(All)')) {
          $(f).attr('passFilter', ($(f).text() == myFilterColumn.val()));
        } 
        else {
            var myFilterText   = $('#filter-input-for-'+$(table).attr('id')+'-column-'+i);
                        if (myFilterText.length > 0) {
          $(f).attr('passFilter', $(f).text().toLowerCase().indexOf(myFilterText.val().toLowerCase()) != -1);
            }
        }
      });
      if ($('td',$(r)).length==$('td[passFilter="true"]',$(r)).length){
        $(r).show();
      }else{
        $(r).hide();
      }
    });
  }
});
