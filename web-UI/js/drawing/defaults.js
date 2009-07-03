dojo.provide("drawing.defaults");

drawing.defaults = {
	
	// current will point to either null or selected
	current:null,
	
	//currentHit will point to either hitNorm or hitSelected
	currentHit:null,
	
	norm:{
		width:3,
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
	hitNorm:{
		width:10,
		color:{r:0, g:255, b:255, a:0},
		style:"Solid",
		cap:"round",
		fill:{r:255, g:255, b:255, a:0}
	},
	hitSelected:{
		width:10,
		color:{r:255, g:255, b:0, a:1},
		style:"Solid",
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
		minSize:10
	},
	arrows:{
		length:30,
		width:16
	},
	text:{
		minWidth:150,
		size:12,
		pad:3,
		fontFamily:"sans-serif",
		mode:{
			create:{
				width:2,
				style:"dotted",
				color:"#ff0000",
				fill:null
			},
			edit:{
				width:3,
				style:"dashed",
				color:"#666",
				fill:null
			},
			selected:{
				width:8,
				style:"solid",
				color:"#ffff00",
				fill:null
			},
			norm:{
				width:0,
				style:"solid",
				color:null,
				fill:null
			}
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
		return o;
	}
	
};
