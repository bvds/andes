dojo.provide("drawing.defaults");

drawing.defaults = {
	// summary:
	//	Styles and defaults used for Drawing stencils and text.
	// description:
	//	This object contains defaults for objects used in Drawing.
	//	To change one item's style, use item.attr();
	//	To change all these styles, create a copy of this file
	//	and point to it in the Drawing properties:
	//	|	<div dojoType="drawing.Drawing" id="drawing" defaults="MyCustom.defaults"></div>
	//
	// current will point to either null or selected
	current:null,
	
	//currentHit will point to either hitNorm or hitSelected
	currentHit:null,
	
	// line, arrows, vector and axes will all snap to this angle on mouse up
	// shown angle also reflects the snap
	// currently cannot accept less than 1 degree
	angleSnap:1,
	
	norm:{
		// normal style of all shapes
		// will get overridden by
		// above andes styles
		width:1,
		color:"#000000",
		style:"Solid",
		cap:"round", // square, butt, round
		fill:"#CCCCCC"
	},
	
	selected:{
		// selected style of all shapes
		width:6,
		color:"#00FF00",
		style:"Solid",
		cap:"round",
		fill:"#E11EBB"
	},
	
	highlighted:{
		// highlighted style of all shapes
		width:6,
		color:"#FF00FF",
		style:"Solid",
		cap:"round",
		fill:"#E11EBB"
	},
	
	disabled:{
		// disabled or "locked" or "fade" style of all shapes
		width:1,
		color:"#666666",
		style:"solid",
		cap:"round",
		fill:"#cccccc"
	},
	
	// "hit" refers to the hidden object below the shape
	// that is usually larger than the object to give a
	// larger 'target' to click upon. These hit objects
	// double as highlights.
	//
	hitNorm:{
		width:6,
		color:{r:0, g:255, b:255, a:0},
		style:"Solid",
		cap:"round",
		fill:{r:255, g:255, b:255, a:0}
	},
	hitSelected:{
		width:6,
		color:"#FF9900",
		style:"Solid",
		cap:"round",
		fill:{r:255, g:255, b:255, a:0}
	},
	hitHighlighted:{
		width:6,
		color:"#FFFF00",
		style:"Solid",
		cap:"round",
		fill:{r:255, g:255, b:255, a:0}
	},

	anchors:{
		// style for the anchor resize-points 
		size:10,
		width:2,
		color:"#999",
		style:"solid",
		fill:"#fff",
		cap:"square",
		minSize:10,
		marginZero:5 // not really an anchor prop
	},
	arrows:{
		// size of arrows on vectors.
		// length is in pixels
		// width is actually an angle
		// but is close to pixels in size
		length:30,
		width:16
	},
	text:{
		// style of text
		minWidth:300,
		deleteEmptyCreate:true,
		deleteEmptyModify:true,
		pad:3,
		size:"12px",
		family:"sans-serif",
		weight:"normal",
		color:"#000000"
	},
	textDisabled:{
		// style of disabled text
		size:"12px",
		family:"sans-serif",
		weight:"normal",
		color:"#cccccc"
	},
	
	textMode:{
		// These styles apply to the containing
		//	text box (edit mode), and not the text itself
		create:{
			width:2,
			style:"dotted",
			color:"#666666",
			fill:null
		},
		edit:{
			width:1,
			style:"dashed",
			color:"#666",
			fill:null
		}
	
	},
	
	copy: function(){
		// summary
		//	Each shape gets its own copy
		//	of these styles so that instances
		// do not change each other's styles
		//
		var cpy = function(obj){
			var o = {};
			for(var nm in obj){
				if(nm!="copy"){
					if(typeof(obj[nm])=="object"){
						o[nm] = cpy(obj[nm]);
					}else{
						o[nm] = obj[nm]
					}
				}
			}
			return o;
		}
		var o = cpy(this);
		o.current = o.norm;
		o.currentHit = o.hitNorm;
		o.currentText = o.text;
		return o;
	}
	
};
