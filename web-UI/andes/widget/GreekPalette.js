dojo.provide("andes.widget.GreekPalette");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit._PaletteMixin");
dojo.require("dojo.i18n");
dojo.require("andes.typeset");

dojo.requireLocalization("dojox.editor.plugins", "latinEntities");

dojo.declare("andes.widget.GreekPalette",
	[dijit._Widget, dijit._Templated, dijit._PaletteMixin],
	{
	// summary:
	//		A keyboard accessible color-picking widget
	// description:
	//		Grid showing various colors, so the user can pick a certain color.
	//		Can be used standalone, or as a popup.
	//
	// example:
	// |	<div dojoType="dijit.ColorPalette"></div>
	//
	// example:
	// |	var picker = new dijit.ColorPalette({ },srcNode);
	// |	picker.startup();

	postMixInProperties: function(){
		// Convert hash of entities into two-dimensional rows/columns table (array of arrays)
		var choices = andes.Greeks.code;
		var numChoices = 0;
		var entityKey;
		for(entityKey in choices){numChoices++;}
		var choicesPerRow = Math.floor(Math.sqrt(numChoices));
		var numRows = choicesPerRow;
		var currChoiceIdx = 0;
		var rows = [];
		var row = [];
		for(entityKey in choices){
			currChoiceIdx++;
			row.push(entityKey);
			if(currChoiceIdx % numRows === 0){
				rows.push(row);
				row = [];
			}
		}
		if(row.length > 0){
			rows.push(row);
		}
		this._palette = rows;
	},

	// templateString: String
	//		The template of this widget.
	templateString: '<div class="dojoxEntityPalette">\n' +
			'	<table>\n' +
			'		<tbody>\n' +
			'			<tr>\n' +
			'				<td>\n' +
			'					<table class="dijitPaletteTable">\n' +
			'						<tbody dojoAttachPoint="gridNode"></tbody>\n' +
			'				   </table>\n' +
			'				</td>\n' +
			'			</tr>\n' +
			'			<tr>\n' +
			'				<td>\n'+
			'					<table dojoAttachPoint="previewPane" class="dojoxEntityPalettePreviewTable">\n' +
			'						<tbody>\n' +
			'							<tr>\n' +
			'								<td class="dojoxEntityPalettePreviewDetailEntity">Type: <span class="dojoxEntityPalettePreviewDetail" dojoAttachPoint="previewNode"></span></td>\n' +
			'							</tr>\n' +
			'						</tbody>\n' +
			'					</table>\n' +
			'				</td>\n' +
			'			</tr>\n' +
			'		</tbody>\n' +
			'	</table>\n' +
			'</div>',


	baseClass: "dojoxEntityPalette",
        
        // showPreview: [public] Boolean
	//	  Whether the preview pane will be displayed, to show details about the selected entity.
	showPreview: true,

	// palette: [public] String
	//		The symbol pallete to display.  The only current one is 'latin'.
	//palette: "latin",

	dyeClass: 'andes.Greeks',

	// domNodeClass [protected] String
	paletteClass: 'editorLatinEntityPalette',

	cellClass: "dojoxEntityPaletteCell",

	buildRendering: function(){
		// Instantiate the template, which makes a skeleton into which we'll insert a bunch of
		// <img> nodes

		this.inherited(arguments);

		var i18n = dojo.i18n.getLocalization("dojox.editor.plugins", "latinEntities");

		this._preparePalette(
			this._palette,
			i18n
		);
		
		var cells = dojo.query(".dojoxEntityPaletteCell", this.gridNode);
		dojo.forEach(cells, function(cellNode){
			this.connect(cellNode, "onmouseenter", "_onCellMouseEnter");
		}, this);
	},
        
        _onCellMouseEnter: function(e){
		// summary:
		//		Simple function to handle updating the display at the bottom of
		//		the palette.
		// e:
		//		The event.
		// tags:
		//		private
		if(this.showPreview){
			this._displayDetails(e.target);
		}
	},
	
	_onCellClick: function(/*Event*/ evt){
		// summary:
		//		Handler for click, enter key & space key. Selects the cell.
		// evt:
		//		The event.
		// tags:
		//		private
		var target = evt.type == "click" ? evt.currentTarget : this._currentFocus,	
			value = this._getDye(target).getValue();

		// First focus the clicked cell, and then send onChange() notification.
		// onChange() (via _setValueAttr) must be after the focus call, because
		// it may trigger a refocus to somewhere else (like the Editor content area), and that
		// second focus should win.
		// Use setTimeout because IE doesn't like changing focus inside of an event handler.
		this._setCurrent(target);
		setTimeout(dojo.hitch(this, function(){
			dijit.focus(target);		
			this._setValueAttr(value, true);		
		}));

		// workaround bug where hover class is not removed on popup because the popup is
		// closed and then there's no onblur event on the cell
		dojo.removeClass(target, "dijitPaletteCellHover");

		dojo.stopEvent(evt);
	},
	
	onCancel: function(/*Boolean*/ closeAll){
		// summary: attach point for notification about when the user cancels the current menu
	},


	postCreate: function(){
		this.inherited(arguments);

		if(!this.showPreview){
			dojo.style(this.previewNode,"display","none");
		}
	},

	_setCurrent: function(/*DOMNode*/ node){
		// summary:
		//		Called when a entity is hovered or focused.
		// description:
		//		Removes highlight of the old entity, and highlights
		//		the new entity.
		// tags:
		//		protected
			// summary:
		//		Sets which node is the focused cell.
		// description:
   		//		At any point in time there's exactly one
		//		cell with tabIndex != -1.   If focus is inside the palette then
		// 		focus is on that cell.
		//
		//		After calling this method, arrow key handlers and mouse click handlers
		//		should focus the cell in a setTimeout().
		// tags:
		//		protected
		if("_currentFocus" in this){
			// Remove tabIndex on old cell
			dojo.attr(this._currentFocus, "tabIndex", "-1");
			dojo.removeClass(this._currentFocus,"dojoxEntityPaletteCellHover");
		}

		// Set tabIndex of new cell
		this._currentFocus = node;
		if(node){
			dojo.attr(node, "tabIndex", this.tabIndex);
			dojo.addClass(this._currentFocus,"dojoxEntityPaletteCellHover");
		}
		if(this.showPreview){
			this._displayDetails(node);
		}
	},

	_displayDetails: function(/*DOMNode*/ cell){
		// summary:
		//	  Display the details of the currently focused entity in the preview pane
		var dye = this._getDye(cell);
		if(dye){
			var ehtml = dye.getValue();
			var ename = dye._alias;
                        //console.warn("Greek help: ",dye._alias);
			this.previewNode.innerHTML=ehtml;
		}else{
			this.previewNode.innerHTML="";
			this.descNode.innerHTML="";
		}
	},
	
	_preparePalette: function(choices, titles) {
		// summary:
		//		Subclass must call _preparePalette() from postCreate(), passing in the tooltip
		//		for each cell
		// choices: String[][]
		//		id's for each cell of the palette, used to create Dye JS object for each cell
		// titles: String[]
		//		Localized tooltip for each cell

		this._cells = [];
		var url = this._blankGif;
		
		var dyeClassObj = dojo.getObject(this.dyeClass);

		for(var row=0; row < choices.length; row++){
			var rowNode = dojo.create("tr", {tabIndex: "-1"}, this.gridNode);
			for(var col=0; col < choices[row].length; col++){
				var value = choices[row][col];
				if(value){
					var cellObject = new dyeClassObj(value);
					
					var cellNode = dojo.create("td", {
						"class": this.cellClass,
						tabIndex: "-1",
						title: titles[value]
					});

					// prepare cell inner structure
					cellObject.fillCell(cellNode, url);

					this.connect(cellNode, "ondijitclick", "_onCellClick");
					this._trackMouseState(cellNode, this.cellClass);

					dojo.place(cellNode, rowNode);

					cellNode.index = this._cells.length;

					// save cell info into _cells
					this._cells.push({node:cellNode, dye:cellObject});
				}
			}
		}
		this._xDim = choices[0].length;
		this._yDim = choices.length;
		
	},
	
	_navigateByArrow: function(evt){
		// summary:
		// 	  	This is the callback for typematic.
		// 		It changes the focus and the highlighed cell.
		// increment:
		// 		How much the key is navigated.
		// tags:
		//		private
		var keyIncrementMap = {
			38: -this._xDim,
			// The down key the index is increase by the x dimension.
			40: this._xDim,
			// Right and left move the index by 1.
			39: this.isLeftToRight() ? 1 : -1,
			37: this.isLeftToRight() ? -1 : 1
		};
		
		var increment = keyIncrementMap[evt.keyCode];
		console.warn("navigation commenced: ",increment);
		var newFocusIndex = this._currentFocus.index + increment;
		if(newFocusIndex < this._cells.length && newFocusIndex > -1){
			var focusNode = this._cells[newFocusIndex].node;
			this._setCurrent(focusNode);
			// Actually focus the node, for the benefit of screen readers.
			// Use setTimeout because IE doesn't like changing focus inside of an event handler
			//setTimeout(dojo.hitch(dijit, "focus", focusNode), 0);
		}
	}
});

dojo.declare("andes.Greeks",
        null,
	{
		// summary:
		//		Represents a character.
		//		Initialized using an alias for the character (like cent) rather
		//		than with the character itself.

 		constructor: function(/*String*/ alias){
			// summary:
			//	 Construct JS object representing an entity (associated w/a cell
			//		in the palette)
			// value: String
			//		alias name: 'cent', 'pound' ..
			this._alias = alias;
		},

		getValue: function(){
			// summary:
			//   Returns HTML representing the character, like &amp;
			//
			return this._alias;
		},

		fillCell: function(/*DOMNode*/ cell){
			// Deal with entities that have keys which are reserved words.
			cell.innerHTML = "&"+this._alias+";";
                }
});

andes.Greeks.code = {
            //These should be accessed from the typeset object, which means the
            //greeks variable needs to be pulled into the typeset object.
            //I got it working but wanted to test it first and the server was down
            //so this will go up for now.
            "alpha": 945,   //alpha,     U+03B1 ISOgrk3 -->
            "beta": 946,   //beta, U+03B2 ISOgrk3 -->
            "gamma": 947,   //gamma,  U+03B3 ISOgrk3 -->
            "delta": 948,   //delta,        U+03B4 ISOgrk3 -->
            "epsilon": 949,   //epsilon,    U+03B5 ISOgrk3 -->
            "zeta": 950,   //zeta, U+03B6 ISOgrk3 -->
            "eta": 951,   //eta, U+03B7 ISOgrk3 -->
            "theta": 952,   //theta,   U+03B8 ISOgrk3 -->
            "iota": 953,   //iota, U+03B9 ISOgrk3 -->
            "kappa": 954,   //kappa,   U+03BA ISOgrk3 -->
            "lambda": 955,   //lambda,    U+03BB ISOgrk3 -->
            "mu": 956,   //mu, U+03BC ISOgrk3 -->
            "nu": 957,   //nu, U+03BD ISOgrk3 -->
            "xi": 958,   //xi, U+03BE ISOgrk3 -->
            "omicron": 959,   //omicron, U+03BF NEW -->
            "pi": 960,   //pi, U+03C0 ISOgrk3 -->
            "rho": 961,   //rho, U+03C1 ISOgrk3 -->
            "sigmaf": 962,   //final sigma,  U+03C2 ISOgrk3 -->
            "sigma": 963,   //sigma,  U+03C3 ISOgrk3 -->
            "tau": 964,   //tau, U+03C4 ISOgrk3 -->
            "upsilon": 965,   //upsilon,   U+03C5 ISOgrk3 -->
            "phi": 966,   //phi, U+03C6 ISOgrk3 -->
            "chi": 967,   //chi, U+03C7 ISOgrk3 -->
            "psi": 968,   //psi, U+03C8 ISOgrk3 -->
            "omega": 969,   //omega,      U+03C9 ISOgrk3 -->
            "thetasym": 977,   //theta symbol,  U+03D1 NEW -->
            "upsih": 978,     // upsilon with hook symbol,  U+03D2 NEW -->
            "piv": 982,     // greek pi symbol, U+03D6 ISOgrk3 -->
	    "Alpha": 913,    // alpha, U+0391 -->
            "Beta": 914,      // beta, U+0392 -->
            "Gamma": 915,    //gamma, U+0393 ISOgrk3 -->
            "Delta": 916,    //delta,           U+0394 ISOgrk3 -->
            "Epsilon": 917,    //epsilon, U+0395 -->
            "Zeta": 918,    //zeta, U+0396 -->
            "Eta": 919,    //eta, U+0397 -->
            "Theta": 920,    //theta,           U+0398 ISOgrk3 -->
            "Iota": 921,    //iota, U+0399 -->
            "Kappa": 922,    //kappa, U+039A -->
            "Lambda": 923,    //lambda,      U+039B ISOgrk3 -->
            "Mu": 924,    //mu, U+039C -->
            "Nu": 925,    //nu, U+039D -->
            "Xi": 926,    //xi, U+039E ISOgrk3 -->
            "Omicron": 927,    //omicron, U+039F -->
            "Pi": 928,    //pi, U+03A0 ISOgrk3 -->
            "Rho": 929,    //rho, U+03A1 -->
            "Sigma": 931,    //sigma,     U+03A3 ISOgrk3 -->
            "Tau": 932,    //tau, U+03A4 -->
            "Upsilon": 933,    //upsilon,   U+03A5 ISOgrk3 -->
            "Phi": 934,    //phi, U+03A6 ISOgrk3 -->
            "Chi": 935,    //chi, U+03A7 -->
            "Psi": 936,    //psi,   U+03A8 ISOgrk3 -->
            "Omega": 937    //omega,     U+03A9 ISOgrk3 -->
};