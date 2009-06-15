dojo.provide("drawing.stencil.Ellipse");


drawing.stencil.Ellipse = drawing.util.oo.declare(
	drawing.stencil.Stencil,
	function(options){
		if(options.data || options.points){
			this.points = options.points || this.dataToPoints(options.data);
			this.render();
		}
	},
	{
		anchorType: "group",
		dataToPoints: function(obj){
			var x = obj.cx - obj.rx,
				y = obj.cy - obj.ry,
				w = obj.rx*2,
				h = obj.ry*2
			
			return [
				{x:x, y:y}, 	// TL
				{x:x+w, y:y},	// TR
				{x:x+w, y:y+h},	// BR
				{x:x, y:y+h}	// BL
			];
		},
		pointsToData: function(){
			var s = this.points[0];
			var e = this.points[2];
			
			return {
				cx: s.x + (e.x - s.x)/2,
				cy: s.y + (e.y - s.y)/2,
				rx: (e.x - s.x)*.5,
				ry: (e.y - s.y)*.5
			};
		
		},
		render: function(){
			//console.info("render", this._onRender.toString())
			this.remove();
			var d = this.pointsToData()
			//console.log("pts:", dojo.toJson(this.pointsToData()))
			
			this.shape = this.parent.createEllipse(d)
				.setStroke(this.style.line)
				.setFill(this.style.fill);
			
		},
		
		onDrag: function(obj){
			var s = obj.start, e = obj;
			var	x = s.x < e.x ? s.x : e.x,
				y = s.y < e.y ? s.y : e.y,
				w = s.x < e.x ? e.x-s.x : s.x-e.x,
				h = s.y < e.y ? e.y-s.y : s.y-e.y;
			
			if(this.keys.shift){ w = h = Math.max(w,h); }
			if(!this.keys.alt){	x+=w/2; y+=h/2; w/=2; h/=2; } // ellipse is normally on center
			
			this.points = [
				{x:x-w, y:y-h}, 	// TL
				{x:x+w, y:y-h},		// TR
				{x:x+w, y:y+h},		// BR
				{x:x, y:y+h}		// BL
			];
			this.render();
		},
		
		onUp: function(obj){
			if(this.created || !this.shape){ return; }
			
			// if too small, need to reset
			var o = this.pointsToData();
			if(o.rx*2<this.minimumSize && o.ry*2 < this.minimumSize){
				this.remove();
				return;
			}
			
			this.onRender(this);
			
		}
		
	}
);