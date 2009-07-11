dojo.provide("drawing.defaults");

drawing.defaults = {
	
	// current will point to either null or selected
	current:null,
	
	//currentHit will point to either hitNorm or hitSelected
	currentHit:null,
	
	norm:{
		width:1,
		color:"#0000FF",
		style:"Solid",
		cap:"round", // square, butt, round
		fill:"#BCE4FE"
	},
	selected:{
		width:3,
		color:"#00FF00",
		style:"Solid",
		cap:"round",
		fill:"#E11EBB"
	},
	highlighted:{
		width:1,
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
		width:1,
		color:{r:255, g:255, b:255, a:0},
		style:"Solid",
		cap:"round",
		fill:{r:255, g:255, b:255, a:0}
	},
	hitSelected:{
		width:1,
		color:{r:255, g:255, b:0, a:1},
		style:"Solid",
		cap:"round",
		fill:{r:255, g:255, b:255, a:0}
	},
	hitHighlighted:{
		width:1,
		color:{r:255, g:0, b:0, a:1},
		style:"solid",
		cap:"round",
		fill:{r:255, g:255, b:255, a:0}
	},
	outline:{ //used??
		width:1,
		color:"#666666",
		style:"Dash"
	},
	anchors:{
		size:10,
		width:2,
		color:"#999",
		style:"solid",
		fill:"#ccc",
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
		size:"12px",
		family:"sans-serif",
		weight:"normal",
		color:"#000000"
	},
	textSelected:{
		size:"12px",
		family:"sans-serif",
		weight:"normal",
		fill:"#000000"
	},
	textHighlighted:{
		size:"12px",
		family:"sans-serif",
		weight:"normal",
		fill:"#000000"
	},
	textDisabled:{
		size:"12px",
		family:"sans-serif",
		weight:"normal",
		fill:"#cccccc"
	},
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
		},
		
		// NOT USED::::
		selected:{
			width:2,
			style:"solid",
			color:"#ffff00",
			fill:null
		},
		highlighted:{
			width:3,
			color:"#000000",
			style:"solid",
			fill:null
		},
		norm:{
			width:0,
			style:"solid",
			color:null,
			fill:"#000000"
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
