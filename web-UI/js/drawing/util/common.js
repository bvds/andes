dojo.provide("drawing.util.common");
dojo.require("dojox.math.round");

(function(){
	var uidMap = {};

	drawing.util.common	= {
		// MAFF
		radToDeg: function(n) {
			//	summary
			//	Convert the passed number to degrees.
			return (n*180)/Math.PI;	//	Number
		},
		
		degToRad: function(n) {
			//	summary
			//	Convert the passed number to radians.
			return (n*Math.PI)/180;	// Number
		},
		
		angle: function(obj, /* ? Float */snap){
			// summary:
			//	Return angle based on mouse object
			// snap:
			//	Returns nearest angle within snap limits
			//obj = this.argsToObj.apply(this, arguments);
			if(snap){
				snap = snap/180;
				var radians = this.radians(obj),
					radius = this.length(obj),
					seg = Math.PI * snap,
					rnd = dojox.math.round(radians/seg),
					new_radian = rnd*seg;
				return dojox.math.round(this.radToDeg(new_radian)); // Whole Number
			
			}else{
				return this.radToDeg(this.radians(obj)); // Float
			}
		},
		
		radians: function(o){
			//var o = this.argsToObj.apply(this, arguments);
			return Math.atan2(o.start.y-o.y,o.start.x-o.x);
		},
		
		length: function(o){
			return Math.sqrt(Math.pow(o.start.x-o.x, 2)+Math.pow(o.start.y-o.y, 2));
		},
		
		lineSub: function(x1, y1, x2, y2, amt){
			var len = this.distance(this.argsToObj.apply(this, arguments));
			len = len < amt ? amt : len;
			var pc = (len-amt)/len;
			var x = x1 - (x1-x2) * pc;
			var y = y1 - (y1-y2) * pc;
			return {x:x, y:y}
		},
		
		argsToObj: function(){
			var a = arguments;
			if(a.length < 4){ return a[0]; }
			return {
				start:{
					x:a[0],
					y:a[1]
				},
				x:a[2],
				y:a[3]//,
				//snap:a[4]
			};
		},
		
		distance: function(){
			var o = this.argsToObj.apply(this, arguments);
			return Math.abs(Math.sqrt(Math.pow(o.start.x-o.x, 2)+Math.pow(o.start.y-o.y, 2)));
		},
		
		slope:function(p1, p2){
			if(!(p1.x-p2.x)){ return 0; }
			return ((p1.y-p2.y)/(p1.x-p2.x));
		},
		pointOnCircle: function(cx, cy, radius, a){
			radians =  a * Math.PI / 180.0;
			var x = radius * Math.cos(radians) * -1;
			var y = radius * Math.sin(radians) * -1;
			return {
				x:cx+x,
				y:cy+y
			}
		},
		
		constrainAngle: function(obj, ca){
			// summary:
			//	Snaps a line to the nearest angle
			//		obj: Mouse object (see drawing.Mouse)
			//		ca: Fractional amount to snap to
			//			A decimal number fraction of a half circle
			//			.5 would snap to 90 degrees
			//			.25  would snap to 45 degrees
			//			.125 would snap to 22.5 degrees, etc.
			//
			var radians = this.radians(obj),
				angle = this.angle(obj),
				radius = this.length(obj),
				seg = Math.PI * ca,
				rnd = Math.round(radians/seg),
				new_radian = rnd*seg,
				new_angle = this.radToDeg(new_radian),
				pt = this.pointOnCircle(obj.start.x,obj.start.y,radius,new_angle);
			return pt;
		},
		
		// graphics
		
		arrowHead: function(x1, y1, x2, y2, style){
			var obj = {
				start:{
					x:x1,
					y:y1
				},
				x:x2,
				y:y2
			}
			var angle = this.angle(obj);
			
			//var angle = this.angle(x1, y1, x2, y2);
				
			var lineLength = this.length(obj); 
			var al = style.arrows.length;
			var aw = style.arrows.width/2;
			if(lineLength<al){
				al = lineLength/2;
			}
			var p1 = this.pointOnCircle(x2, y2, -al, angle-aw);
			var p2 = this.pointOnCircle(x2, y2, -al, angle+aw);
			
			return [
				{x:x2, y:y2},
				p1,
				p2
			];
		},
		
		// helpers
		uid: function(str){
			str = str || "shape";
			uidMap[str] = uidMap[str]===undefined ? 0 : uidMap[str] + 1;
			return str + uidMap[str];
		},
		
		objects:{}, //private?
		register: function(obj){
			this.objects[obj.id] = obj;	
		},
		byId: function(id){
			return this.objects[id];
		},
		attr: function(/* Object */ elem, /* property */ prop, /* ? value */ value, squelchErrors){
			if(!elem) { return false; }
			try{
				
				if(dojox.gfx.renderer=="silverlight"){
					
					var t;
					if(elem.superTarget){
						t = elem.superTarget;
					}else if(elem.superClass){
						t = elem.superClass; 
					}else if(elem.target){
						t = elem.target;
					}else{
						t = elem;
					}
					
					if(value!==undefined){
						elem[prop] = value;
						return value;
					}
					
					if(t.tagName){
						if(prop=="drawingType" && t.tagName.toLowerCase()=="object"){
							return "surface";
						}
						var r =  dojo.attr(t, prop);
					}
					var r = t[prop];
					return r
				}
				
				// NOT silverlight - can set atts on nodes
			
				// util is a crappy check, but we need to tell the diff
				// between a Drawing shape and a GFX shape
				if(elem.shape && elem.util){
					elem = elem.shape;
				}
				
				if(!value && prop=="id" && elem.target){
					var n = elem.target;
					while(!dojo.attr(n, "id")){
						n = n.parentNode;
					}
					return dojo.attr(n, "id");
				}
				
				if(elem.rawNode || elem.target){
					var args = Array.prototype.slice.call(arguments);
					args[0] = elem.rawNode || elem.target;
					return dojo.attr.apply(dojo, args);	
				}		
				return dojo.attr(elem, "id");
				
				
				
			}catch(e){
				if(!squelchErrors){
					console.error("BAD ATTR: prop:", prop, "el:", elem)
					console.error(e)
					console.trace();
				}
				return false;
			}
		}
	};
	
})();