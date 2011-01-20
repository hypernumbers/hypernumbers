/*
 * jQuery columnManager plugin
 * Version: 0.2.5
 *
 * Copyright (c) 2007 Roman Weich
 * http://p.sohei.org
 *
 * Dual licensed under the MIT and GPL licenses 
 * (This means that you can choose the license that best suits your project, and use it accordingly):
 *   http://www.opensource.org/licenses/mit-license.php
 *   http://www.gnu.org/licenses/gpl.html
 *
 * Changelog: 
 * v 0.2.5 - 2008-01-17
 *	-change: added options "show" and "hide". with these functions the user can control the way to show or hide the cells
 *	-change: added $.fn.showColumns() and $.fn.hideColumns which allows to explicitely show or hide any given number of columns
 * v 0.2.4 - 2007-12-02
 *	-fix: a problem with the on/off css classes when manually toggling columns which were not in the column header list
 *	-fix: an error in the createColumnHeaderList function incorectly resetting the visibility state of the columns
 *	-change: restructured some of the code
 * v 0.2.3 - 2007-12-02
 *	-change: when a column header has no text but some html markup as content, the markup is used in the column header list instead of "undefined"
 * v 0.2.2 - 2007-11-27
 *	-change: added the ablity to change the on and off CSS classes in the column header list through $().toggleColumns()
 *	-change: to avoid conflicts with other plugins, the table-referencing data in the column header list is now stored as an expando and not in the class name as before
 * v 0.2.1 - 2007-08-14
 *	-fix: handling of colspans didn't work properly for the very first spanning column
 *	-change: altered the cookie handling routines for easier management
 * v 0.2.0 - 2007-04-14
 *	-change: supports tables with colspanned and rowspanned cells now
 * v 0.1.4 - 2007-04-11
 *	-change: added onToggle option to specify a custom callback function for the toggling over the column header list
 * v 0.1.3 - 2007-04-05
 *	-fix: bug when saving the value in a cookie
 *	-change: toggleColumns takes a number or an array of numbers as argument now
 * v 0.1.2 - 2007-04-02
 * 	-change: added jsDoc style documentation and examples
 * 	-change: the column index passed to toggleColumns() starts at 1 now (conforming to the values passed in the hideInList and colsHidden options)
 * v 0.1.1 - 2007-03-30
 * 	-change: changed hideInList and colsHidden options to hold integer values for the column indexes to be affected
 *	-change: made the toggleColumns function accessible through the jquery object, to toggle the state without the need for the column header list
 *	-fix: error when not finding the passed listTargetID in the dom
 * v 0.1.0 - 2007-03-27
 */

(function($) 
{
	var defaults = {
		listTargetID : null,
		onClass : '',
		offClass : '',
		hideInList: [],
		colsHidden: [],
		saveState: false,
		onToggle: null,
		show: function(cell){
			showCell(cell);
		},
		hide: function(cell){
			hideCell(cell);
		}
	};
	
	var idCount = 0;
	var cookieName = 'columnManagerC';

	/**
	 * Saves the current state for the table in a cookie.
	 * @param {element} table	The table for which to save the current state.
	 */
	var saveCurrentValue = function(table)
	{
		var val = '', i = 0, colsVisible = table.cMColsVisible;
		if ( table.cMSaveState && table.id && colsVisible && $.cookie )
		{
			for ( ; i < colsVisible.length; i++ )
			{
				val += ( colsVisible[i] == false ) ? 0 : 1;
			}
			$.cookie(cookieName + table.id, val, {expires: 9999});
		}
	};
	
	/**
	 * Hides a cell.
	 * It rewrites itself after the browsercheck!
	 * @param {element} cell	The cell to hide.
	 */
	var hideCell = function(cell)
	{
		if ( jQuery.browser.msie )
		{
			(hideCell = function(c)
			{
				c.style.setAttribute('display', 'none');
			})(cell);
		}
		else
		{
			(hideCell = function(c)
			{
				c.style.display = 'none';
			})(cell);
		}
	};

	/**
	 * Makes a cell visible again.
	 * It rewrites itself after the browsercheck!
	 * @param {element} cell	The cell to show.
	 */
	var showCell = function(cell)
	{
		if ( jQuery.browser.msie )
		{
			(showCell = function(c)
			{
				c.style.setAttribute('display', 'block');
			})(cell);
		}
		else
		{
			(showCell = function(c)
			{
				c.style.display = 'table-cell';
			})(cell);
		}
	};

	/**
	 * Returns the visible state of a cell.
	 * It rewrites itself after the browsercheck!
	 * @param {element} cell	The cell to test.
	 */
	var cellVisible = function(cell)
	{
		if ( jQuery.browser.msie )
		{
			return (cellVisible = function(c)
			{
				return c.style.getAttribute('display') != 'none';
			})(cell);
		}
		else
		{
			return (cellVisible = function(c)
			{
				return c.style.display != 'none';
			})(cell);
		}
	};

	/**
	 * Returns the cell element which has the passed column index value.
	 * @param {element} table	The table element.
	 * @param {array} cells		The cells to loop through.
	 * @param {integer} col	The column index to look for.
	 */
	var getCell = function(table, cells, col)
	{
		for ( var i = 0; i < cells.length; i++ )
		{
			if ( cells[i].realIndex === undefined ) //the test is here, because rows/cells could get added after the first run
			{
				fixCellIndexes(table);
			}
			if ( cells[i].realIndex == col )
			{
				return cells[i];
			}
		}
		return null;
	};

	/**
	 * Calculates the actual cellIndex value of all cells in the table and stores it in the realCell property of each cell.
	 * Thats done because the cellIndex value isn't correct when colspans or rowspans are used.
	 * Originally created by Matt Kruse for his table library - Big Thanks! (see http://www.javascripttoolbox.com/)
	 * @param {element} table	The table element.
	 */
	var fixCellIndexes = function(table) 
	{
		var rows = table.rows;
		var len = rows.length;
		var matrix = [];
		for ( var i = 0; i < len; i++ )
		{
			var cells = rows[i].cells;
			var clen = cells.length;
			for ( var j = 0; j < clen; j++ )
			{
				var c = cells[j];
				var rowSpan = c.rowSpan || 1;
				var colSpan = c.colSpan || 1;
				var firstAvailCol = -1;
				if ( !matrix[i] )
				{ 
					matrix[i] = []; 
				}
				var m = matrix[i];
				// Find first available column in the first row
				while ( m[++firstAvailCol] ) {}
				c.realIndex = firstAvailCol;
				for ( var k = i; k < i + rowSpan; k++ )
				{
					if ( !matrix[k] )
					{ 
						matrix[k] = []; 
					}
					var matrixrow = matrix[k];
					for ( var l = firstAvailCol; l < firstAvailCol + colSpan; l++ )
					{
						matrixrow[l] = 1;
					}
				}
			}
		}
	};
	
	/**
	 * Manages the column display state for a table.
	 *
	 * Features:
	 * Saves the state and recreates it on the next visit of the site (requires cookie-plugin).
	 * Extracts all headers and builds an unordered(<UL>) list out of them, where clicking an list element will show/hide the matching column.
	 *
	 * @param {map} options		An object for optional settings (options described below).
	 *
	 * @option {string} listTargetID	The ID attribute of the element the column header list will be added to.
	 *						Default value: null
	 * @option {string} onClass		A CSS class that is used on the items in the column header list, for which the column state is visible 
	 *						Works only with listTargetID set!
	 *						Default value: ''
	 * @option {string} offClass		A CSS class that is used on the items in the column header list, for which the column state is hidden.
	 *						Works only with listTargetID set!
	 *						Default value: ''
	 * @option {array} hideInList	An array of numbers. Each column with the matching column index won't be displayed in the column header list.
	 *						Index starting at 1!
	 *						Default value: [] (all columns will be included in the list)
	 * @option {array} colsHidden	An array of numbers. Each column with the matching column index will get hidden by default.
	 *						The value is overwritten when saveState is true and a cookie is set for this table.
	 *						Index starting at 1!
	 *						Default value: []
	 * @option {boolean} saveState	Save a cookie with the sate information of each column.
	 *						Requires jQuery cookie plugin.
	 *						Default value: false
	 * @option {function} onToggle	Callback function which is triggered when the visibility state of a column was toggled through the column header list.
	 *						The passed parameters are: the column index(integer) and the visibility state(boolean).
	 *						Default value: null
	 *
	 * @option {function} show		Function which is called to show a table cell.
	 *						The passed parameters are: the table cell (DOM-element).
	 *						Default value: a functions which simply sets the display-style to block (visible)
	 *
	 * @option {function} hide		Function which is called to hide a table cell.
	 *						The passed parameters are: the table cell (DOM-element).
	 *						Default value: a functions which simply sets the display-style to none (invisible)
	 *
	 * @example $('#table').columnManager([listTargetID: "target", onClass: "on", offClass: "off"]);
	 * @desc Creates the column header list in the element with the ID attribute "target" and sets the CSS classes for the visible("on") and hidden("off") states.
	 *
	 * @example $('#table').columnManager([listTargetID: "target", hideInList: [1, 4]]);
	 * @desc Creates the column header list in the element with the ID attribute "target" but without the first and fourth column.
	 *
	 * @example $('#table').columnManager([listTargetID: "target", colsHidden: [1, 4]]);
	 * @desc Creates the column header list in the element with the ID attribute "target" and hides the first and fourth column by default.
	 *
	 * @example $('#table').columnManager([saveState: true]);
	 * @desc Enables the saving of visibility informations for the columns. Does not create a column header list! Toggle the columns visiblity through $('selector').toggleColumns().
	 *
	 * @type jQuery
	 *
	 * @name columnManager
	 * @cat Plugins/columnManager
	 * @author Roman Weich (http://p.sohei.org)
	 */
	$.fn.columnManager = function(options)
	{
		var settings = $.extend({}, defaults, options);

		/**
		 * Creates the column header list.
		 * @param {element} table	The table element for which to create the list.
		 */
		var createColumnHeaderList = function(table)
		{
			if ( !settings.listTargetID )
			{
				return;
			}
			var $target = $('#' + settings.listTargetID);
			if ( !$target.length )
			{
				return;
			}
			//select headrow - when there is no thead-element, use the first row in the table
			var headRow = null;
			if ( table.tHead && table.tHead.length )
			{
				headRow = table.tHead.rows[0];
			}
			else if ( table.rows.length )
			{
				headRow = table.rows[0];
			}
			else
			{
				return; //no header - nothing to do
			}
			var cells = headRow.cells;
			if ( !cells.length )
			{
				return; //no header - nothing to do
			}
			//create list in target element
			var $list = null;
			if ( $target.get(0).nodeName.toUpperCase() == 'UL' )
			{
				$list = $target;
			}
			else
			{
				$list = $('<ul></ul>');
				$target.append($list);
			}
			var colsVisible = table.cMColsVisible;
			//create list elements from headers
			for ( var i = 0; i < cells.length; i++ )
			{
				if ( $.inArray(i + 1, settings.hideInList) >= 0 )
				{
					continue;
				}
				colsVisible[i] = ( colsVisible[i] !== undefined ) ? colsVisible[i] : true;
				var text = $(cells[i]).text(), 
					addClass;
				if ( !text.length )
				{
					text = $(cells[i]).html();
					if ( !text.length ) //still nothing?
					{
						text = 'undefined';
					}
				}
				if ( colsVisible[i] && settings.onClass )
				{
					addClass = settings.onClass;
				}
				else if ( !colsVisible[i] && settings.offClass )
				{
					addClass = settings.offClass;
				}
				var $li = $('<li class="' + addClass + '">' + text + '</li>').click(toggleClick);
				$li[0].cmData = {id: table.id, col: i};
				$list.append($li);
			}
			table.cMColsVisible = colsVisible;
		};

		/**
		 * called when an item in the column header list is clicked
		 */
		var toggleClick = function()
		{
			//get table id and column name
			var data = this.cmData;
			if ( data && data.id && data.col >= 0 )
			{
				var colNum = data.col, 
					$table = $('#' + data.id);
				if ( $table.length )
				{
					$table.toggleColumns([colNum + 1], settings);
					//set the appropriate classes to the column header list
					var colsVisible = $table.get(0).cMColsVisible;
					if ( settings.onToggle )
					{
						settings.onToggle.apply($table.get(0), [colNum + 1, colsVisible[colNum]]);
					}
				}
			}
		};

		/**
		 * Reads the saved state from the cookie.
		 * @param {string} tableID	The ID attribute from the table.
		 */
		var getSavedValue = function(tableID)
		{
			var val = $.cookie(cookieName + tableID);
			if ( val )
			{
				var ar = val.split('');
				for ( var i = 0; i < ar.length; i++ )
				{
					ar[i] &= 1;
				}
				return ar;
			}
			return false;
		};

        return this.each(function()
        {
			this.id = this.id || 'jQcM0O' + idCount++; //we need an id for the column header list stuff
			var i, 
				colsHide = [], 
				colsVisible = [];
			//fix cellIndex values
			fixCellIndexes(this);
			//some columns hidden by default?
			if ( settings.colsHidden.length )
			{
				for ( i = 0; i < settings.colsHidden.length; i++ )
				{
					colsVisible[settings.colsHidden[i] - 1] = true;
					colsHide[settings.colsHidden[i] - 1] = true;
				}
			}
			//get saved state - and overwrite defaults
			if ( settings.saveState )
			{
				var colsSaved = getSavedValue(this.id);
				if ( colsSaved && colsSaved.length )
				{
					for ( i = 0; i < colsSaved.length; i++ )
					{
						colsVisible[i] = true;
						colsHide[i] = !colsSaved[i];
					}
				}
				this.cMSaveState = true;
			}
			//assign initial colsVisible var to the table (needed for toggling and saving the state)
			this.cMColsVisible = colsVisible;
			//something to hide already?
			if ( colsHide.length )
			{
				var a = [];
				for ( i = 0; i < colsHide.length; i++ )
				{
					if ( colsHide[i] )
					{
						a[a.length] = i + 1;
					}
				}
				if ( a.length )
				{
					$(this).toggleColumns(a);
				}
			}
			//create column header list
			createColumnHeaderList(this);
        }); 
	};

	/**
	 * Shows or hides table columns.
	 *
	 * @param {integer|array} columns		A number or an array of numbers. The display state(visible/hidden) for each column with the matching column index will get toggled.
	 *							Column index starts at 1! (see the example)
	 *
	 * @param {map} options		An object for optional settings to handle the on and off CSS classes in the column header list (options described below).
	 * @option {string} listTargetID	The ID attribute of the element with the column header.
	 * @option {string} onClass		A CSS class that is used on the items in the column header list, for which the column state is visible 
	 * @option {string} offClass		A CSS class that is used on the items in the column header list, for which the column state is hidden.
	 * @option {function} show		Function which is called to show a table cell.
	 * @option {function} hide		Function which is called to hide a table cell.
	 *
	 * @example $('#table').toggleColumns([2, 4], {hide: function(cell) { $(cell).fadeOut("slow"); }});
	 * @before <table id="table">
	 *   			<thead>
	 *   				<th>one</th
	 *   				<th>two</th
	 *   				<th>three</th
	 *   				<th>four</th
	 *   			</thead>
	 * 		   </table>
	 * @desc Toggles the visible state for the columns "two" and "four". Use custom function to fade the cell out when hiding it.
	 *
	 * @example $('#table').toggleColumns(3, {listTargetID: 'theID', onClass: 'vis'});
	 * @before <table id="table">
	 *   			<thead>
	 *   				<th>one</th
	 *   				<th>two</th
	 *   				<th>three</th
	 *   				<th>four</th
	 *   			</thead>
	 * 		   </table>
	 * @desc Toggles the visible state for column "three" and sets or removes the CSS class 'vis' to the appropriate column header according to the visibility of the column.
	 *
	 * @type jQuery
	 *
	 * @name toggleColumns
	 * @cat Plugins/columnManager
	 * @author Roman Weich (http://p.sohei.org)
	 */
	$.fn.toggleColumns = function(columns, cmo)
	{
        return this.each(function() 
        {
			var i, toggle, di, 
				rows = this.rows, 
				colsVisible = this.cMColsVisible;

			if ( !columns )
				return;

			if ( columns.constructor == Number )
				columns = [columns];

			if ( !colsVisible )
				colsVisible = this.cMColsVisible = [];

			//go through all rows in the table and hide the cells
			for ( i = 0; i < rows.length; i++ )
			{
				var cells = rows[i].cells;
				for ( var k = 0; k < columns.length; k++ )
				{
					var col = columns[k] - 1;
					if ( col >= 0 )
					{
						//find the cell with the correct index
						var c = getCell(this, cells, col);
						//cell not found - maybe a previous one has a colspan? - search it!
						if ( !c )
						{
							var cco = col;
							while ( cco > 0 && !(c = getCell(this, cells, --cco)) ) {} //find the previous cell
							if ( !c )
							{
								continue;
							}
						}
						//set toggle direction
						if ( colsVisible[col] == undefined )//not initialized yet
						{
							colsVisible[col] = true;
						}
						if ( colsVisible[col] )
						{
							toggle = cmo && cmo.hide ? cmo.hide : hideCell;
							di = -1;
						}
						else
						{
							toggle = cmo && cmo.show ? cmo.show : showCell;
							di = 1;
						}
						if ( !c.chSpan )
						{
							c.chSpan = 0;
						}
						//the cell has a colspan - so dont show/hide - just change the colspan
						if ( c.colSpan > 1 || (di == 1 && c.chSpan && cellVisible(c)) )
						{
							//is the colspan even reaching this cell? if not we have a rowspan -> nothing to do
							if ( c.realIndex + c.colSpan + c.chSpan - 1 < col )
							{
								continue;
							}
							c.colSpan += di;
							c.chSpan += di * -1;
						}
						else if ( c.realIndex + c.chSpan < col )//a previous cell was found, but doesn't affect this one (rowspan)
						{
							continue;
						}
						else //toggle cell
						{
							toggle(c);
						}
					}
				}
			}
			//set the colsVisible var
			for ( i = 0; i < columns.length; i++ )
			{
				this.cMColsVisible[columns[i] - 1] = !colsVisible[columns[i] - 1];
				//set the appropriate classes to the column header list, if the options have been passed
				if ( cmo && cmo.listTargetID && ( cmo.onClass || cmo.offClass ) )
				{
					var onC = cmo.onClass, offC = cmo.offClass, $li;
					if ( colsVisible[columns[i] - 1] )
					{
						onC = offC;
						offC = cmo.onClass;
					}
					$li = $("#" + cmo.listTargetID + " li").filter(function(){return this.cmData && this.cmData.col == columns[i] - 1;});
					if ( onC )
					{
						$li.removeClass(onC);
					}
					if ( offC )
					{
						$li.addClass(offC);
					}
				}
			}
			saveCurrentValue(this);
		});
	};

	/**
	 * Shows all table columns.
	 * When columns are passed through the parameter only the passed ones become visible.
	 *
	 * @param {integer|array} columns		A number or an array of numbers. Each column with the matching column index will become visible.
	 *							Column index starts at 1!
	 *
	 * @param {map} options		An object for optional settings which will get passed to $().toggleColumns().
	 *
	 * @example $('#table').showColumns();
	 * @desc Sets the visibility state of all hidden columns to visible.
	 *
	 * @example $('#table').showColumns(3);
	 * @desc Show column number three.
	 *
	 * @type jQuery
	 *
	 * @name showColumns
	 * @cat Plugins/columnManager
	 * @author Roman Weich (http://p.sohei.org)
	 */
	$.fn.showColumns = function(columns, cmo)
	{
        return this.each(function() 
        {
			var i,
				cols = [],
				cV = this.cMColsVisible;
			if ( cV )
			{
				if ( columns && columns.constructor == Number ) 
					columns = [columns];

				for ( i = 0; i < cV.length; i++ )
				{
					//if there were no columns passed, show all - or else show only the columns the user wants to see
					if ( !cV[i] && (!columns || $.inArray(i + 1, columns) > -1) )
						cols.push(i + 1);
				}
				
				$(this).toggleColumns(cols, cmo);
			}
		});
	};

	/**
	 * Hides table columns.
	 *
	 * @param {integer|array} columns		A number or an array of numbers. Each column with the matching column index will get hidden.
	 *							Column index starts at 1!
	 *
	 * @param {map} options		An object for optional settings which will get passed to $().toggleColumns().
	 *
	 * @example $('#table').hideColumns(3);
	 * @desc Hide column number three.
	 *
	 * @type jQuery
	 *
	 * @name hideColumns
	 * @cat Plugins/columnManager
	 * @author Roman Weich (http://p.sohei.org)
	 */
	$.fn.hideColumns = function(columns, cmo)
	{
        return this.each(function() 
        {
			var i,
				cols = columns,
				cV = this.cMColsVisible;
			if ( cV )
			{
				if ( columns.constructor == Number ) 
					columns = [columns];
				cols = [];

				for ( i = 0; i < columns.length; i++ )
				{
					if ( cV[columns[i] - 1] || cV[columns[i] - 1] == undefined )
						cols.push(columns[i]);
				}
				
			}
			$(this).toggleColumns(cols, cmo);
		});
	};
})(jQuery);
