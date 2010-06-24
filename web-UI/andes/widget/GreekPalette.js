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
	templateString: '<table class="dijitInline dojoxEntityPalette dijitPaletteTable"' +
		' cellSpacing=0 cellPadding=0><tbody dojoAttachPoint="gridNode"></tbody></table>',


	baseClass: "dojoxEntityPalette",
        
        // showPreview: [public] Boolean
	//	  Whether the preview pane will be displayed, to show details about the selected entity.
	showPreview: true,

	// showCode: [public] boolean
	//		Show the character code for the entity.
	showCode: false,

	// showentityName: [public] boolean
	//		Show the entity name for the entity.
	showEntityName: false,

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
                console.warn("palette: ",this._palette);
	},
        
        _onCellMouseEnter: function(e){
		// summary:
		//		Simple function to handle updating the display at the bottom of
		//		the palette.
		// e:
		//		The event.
		// tags:
		//		private
		this._displayDetails(e.target);
	},

	postCreate: function(){
		this.inherited(arguments);

		// Show the code and entity name (if enabled to do so.)
		/*dojo.style(this.codeHeader, "display", this.showCode?"":"none");
		dojo.style(this.codeNode, "display", this.showCode?"":"none");
		dojo.style(this.entityHeader, "display", this.showEntityName?"":"none");
		dojo.style(this.entityNode, "display", this.showEntityName?"":"none");

		if(!this.showPreview){
			dojo.style(this.previewNode,"display","none");
		}*/
	},

	_setCurrent: function(/*DOMNode*/ node){
		// summary:
		//		Called when a entity is hovered or focused.
		// description:
		//		Removes highlight of the old entity, and highlights
		//		the new entity.
		// tags:
		//		protected
		this.inherited(arguments);
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
                        console.warn("Greek help: ",dye._alias);
			//this.previewNode.innerHTML=ehtml;
			//this.codeNode.innerHTML="&amp;#"+parseInt(ehtml.charCodeAt(0), 10)+";";
			//this.entityNode.innerHTML="&amp;"+ename+";";
			//var i18n = dojo.i18n.getLocalization("dojox.editor.plugins", "latinEntities");
			//this.descNode.innerHTML=i18n[ename].replace("\n", "<br>");

		}else{
			//this.previewNode.innerHTML="";
			//this.codeNode.innerHTML="";
			//this.entityNode.innerHTML="";
			//this.descNode.innerHTML="";
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