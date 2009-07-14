dojo.provide("drawing.defaults");


// TODO
// mix styles based on primary (borders, text) and secondary (shape fills) colors
//
drawing.defaults = {
	
	// current will point to either null or selected
	current:null,
	
	//currentHit will point to either hitNorm or hitSelected
	currentHit:null,
	
	// line, arrows, vector and axes will all snap to this angle on mouse up
	// shown angle also reflects the snap
	// currently cannot accept less than 1 degree
	angleSnap:1,
	
	norm:{
		width:1,
		color:"#000000",
		style:"Solid",
		cap:"round", // square, butt, round
		fill:"#CCCCCC"
	},
	selected:{
		width:6,
		color:"#00FF00",
		style:"Solid",
		cap:"round",
		fill:"#E11EBB"
	},
	highlighted:{
		width:6,
		color:"#FF00FF",
		style:"Solid",
		cap:"round",
		fill:"#E11EBB"
	},
	disabled:{
		width:1,
		color:"#666666",
		style:"solid",
		cap:"round",
		fill:"#cccccc"
	},
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
		//fill:"#FFFF99"
		fill:{r:255, g:255, b:255, a:0}
	},

	anchors:{
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
		length:30,
		width:16
	},
	text:{
		minWidth:150,
		pad:3,
		size:"20px",
		family:"sans-serif",
		weight:"normal",
		color:"#000000"
	},
	textDisabled:{
		size:"12px",
		family:"sans-serif",
		weight:"normal",
		color:"#cccccc"
	},
	/*textSelected:{
		size:"12px",
		family:"sans-serif",
		weight:"normal",
		color:"#000000"
	},
	textHighlighted:{
		size:"12px",
		family:"sans-serif",
		weight:"normal",
		color:"#000000"
	},
	*/
		// The following styles apply to the containing
		//	text box, and not the text itself
	textMode:{
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
