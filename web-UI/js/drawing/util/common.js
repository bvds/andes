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
			return uidMap[str];
		}
	};
	
})();