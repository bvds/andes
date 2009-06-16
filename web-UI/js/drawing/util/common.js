dojo.provide("drawing.util.common");

(function(){
	var uidMap = {};
	drawing.util.common	= {
		// MAFF
		radToDeg: function(angle) {
			// 0 degrees is center to left.
			// 90 degrees is center straight up.
			var d = ((angle*180) / Math.PI);
			if(d < 0){
				d = 180 + (180 + d);
			}
			return d;
		},
		angle: function(obj){
			return this.radToDeg(this.radians(obj));
		},
		radians: function(obj){
			return Math.atan2(obj.last.y-obj.y,obj.last.x-obj.x);
		},
		length: function(x1,y1,x2,y2){
			return Math.sqrt(Math.pow(x1-x2, 2)+Math.pow(y1-y2, 2));
		},
		distance: function(x1,y1,x2,y2){
			return Math.abs(Math.sqrt(Math.pow(x1-x2, 2)+Math.pow(y1-y2, 2)));
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
		attr: function(/* Object */ elem, /* property */ prop, /* ? value */ value){
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
				
				if(arguments.length==2 && prop=="id" && elem.target){
					return dojo.attr(elem.target, "id") || dojo.attr(elem.target.parentNode, "id");
				}
				
				if(elem.rawNode || elem.target){
					var args = Array.prototype.slice.call(arguments);
					args[0] = elem.rawNode || elem.target;
					return dojo.attr.apply(dojo, args);	
				}
					
				return dojo.attr(elem, "id");
				
				
				
			}catch(e){
				console.error("BAD ATTR: prop:", prop, "el:", elem)
				console.error(e)
				console.trace();
				return false;
			}
		}
	};
	
})();