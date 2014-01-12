// Pre-AMD version had a function wrapper.
// pre-AMD version had no requires
define([
    "dojo/_base/lang"
],function(lang){

    console.info("andes/defaults.js:  defining andes default object.");
    // This allows defaults to be set on its own, without Andes.
	lang.setObject("window.andes.defaults",{
		// summary:
		//	The style used for Andes3.
		// description:
		//	DojoX Drawing comes with a defaults file. This file
		//	overwrites that one to help protect intended styles from
		// 	future changes made in Dojox.
		
		//  Determines whether in draw or edit mode (whether stencils
		//  are clickable.  If clickMode is false, the original
		//  functionality of silently switching between select modes
		//  is enabled.  If clickMode is true, it allows powerpoint-
		//  like functionality.  Clickable is used by powerpoint to
		//  distinguish when things can be selected and when they can't
		clickMode:true,
		clickable:true,

		//  zAxis:  Boolean
		// 		If true, draw current object in z-direction.
		// zAxisEnabled: Boolean
		// 		If true, render axes with Z-axis included, allow objects drawn in z-direction.
		zAxis: false,
		zAxisEnabled: true,
		zAngle: 225,

		// current: Object
		//  current will point to either null or selected
		current:null,

		// currentHit: Object
		//	currentHit will point to either hitNorm or hitSelected
		currentHit:null,

		// angleSnap: Number
		// line, arrows, vector and axes will all snap to this angle on mouse up
		// shown angle also reflects the snap
		// currently cannot accept less than 1 degree
		angleSnap:1,

		// renderHitLines: Boolean
		//	If true, renders a second, larger layer for lines to make
		// 	them more easily clickable.
		renderHitLines: true,
		//
		// renderHitLayer:
		// 	If true, renders a second layer for each Stencil, one
		// 	acting as a 'hit' object for a wider mouse-click area.
		// 	It also doubles as a hilight. If true, overrides
		//	renderHitLines setting.
		renderHitLayer:true,

		// labelSameColor:
		//	If true, the label text color will be the same as the
		//	Stencil's line color.
		labelSameColor:true,

		// object states
		locked:{
			fill:  "#262626",
			color: "#000000"
		},
		correct:{
			fill:  "#CCFFCC",
			color: "#009900"
		},
		incorrect:{
			fill:  "#FE7070",
			color: "#D20202"
		},
		unknown:{
			fill:  "#cccccc",
			color: "#000000"
		},

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
			// disabled or "locked" style of all shapes
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
			width:10,
			color:{r:0, g:255, b:255, a:.0001},
			style:"Solid",
			cap:"round",
			fill:{r:255, g:255, b:255, a:.0001}
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
			//
			// there's no code to keep this from going off the stage
			// or over the Tutor pane. So it should be kept small.
			minWidth:150,
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
 			// This should match helpContentPane text-box in
		        // andes.css
			edit:{
				width:1,
				style:"dashed",
				color:"#666",
				fill:null
			}

		},
		
		// BvdS:  I thought andes.defaults is supposed to override of dojox.drawing.defaults
		//        Why does dojox.drawing.defaults.button not show up if following is not included?
		button:{
			radioButtonRadius:15,
			checkboxWidth:15,
			norm:{
				"color": "#cccccc", 
				"fill": {
					type:"linear",
					x1:0, x2:0, y1:0, y2:100,
					colors:[
						{offset:.5, color:"#ffffff"},
						{offset:1, color:"#e5e5e5"}
					]
				}
			},
			over:{
				"fill": {
					type:"linear",
					x1:0, x2:0, y1:0, y2:100,
					colors:[{offset:.5, color:"#ffffff"}, {offset:1, color:"#e1eaf5"}]
				}, 
				"color": "#92a0b3" 
			},
			down:{
				"fill": {
					type:"linear",
					x1:0, x2:0, y1:0, y2:100, 
					colors:[{offset:0, color:"#e1eaf5"}, {offset:1, color:"#ffffff"}]
				}, 
				"color": "#92a0b3"
			},
			selected:{
				"fill": {
				type:"linear",
					x1:0, x2:0, y1:0, y2:100, 
					colors:[{offset:0, color:"#97b4bf"}, {offset:1, color:"#c8dae1"}]
				}, 
				"color": "#92a0b3"
			},
			icon:{
				norm:{
					fill:null,
					color:"#92a0b3"
				},
				selected:{
					fill:"#ffffff",
					color:"#92a0b3"
				}
			}
		},
		
		copy: function(){
			// summary
			//		Each shape gets its own copy
			//		of these styles so that instances
			// 		do not change each other's styles
			//
			var cpy = function(obj){
				if(typeof(obj)!="object" || obj===null || obj===undefined){
					return obj;
				}
				var o;
				if(obj.push){
					o = [];
					for(var i=0; i<obj.length;i++){
						o.push(cpy(obj[i]));
					}
					return o;
				}
				o = {};
				for(var nm in obj){
					if(nm!="copy"){
						if(typeof(obj[nm])=="object"){
							o[nm] = cpy(obj[nm]);
						}else{
							o[nm] = obj[nm];
						}
					}
				}
				return o;
			};
			var o = cpy(this);
			o.current = o.norm;
			o.currentHit = o.hitNorm;
			o.currentText = o.text;
			return o;
		}
		
	});
	
	// change Drawing defaults to andes defaults
	var a = window.andes.defaults;
	a.norm.fill = a.unknown.fill;
	a.norm.color = a.unknown.color;
	a.disabled.color = a.locked.color;
	a.disabled.fill = a.locked.fill;
	a.textDisabled.color = a.locked.fill;

});
