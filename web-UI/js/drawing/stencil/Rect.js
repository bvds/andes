dojo.provide("drawing.stencil.Rect");


drawing.stencil.Rect = drawing.util.oo.declare(
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
			return [
				{x:obj.x, y:obj.y}, 						// TL
				{x:obj.x + obj.width, y:obj.y},				// TR
				{x:obj.x + obj.width, y:obj.y + obj.height},// BR
				{x:obj.x, y:obj.y + obj.height}				// BL
			];
		},
		pointsToData: function(){
			var s = this.points[0];
			var e = this.points[2];
			
			return {
				x: s.x,
				y: s.y,
				width: e.x-s.x,
				height: e.y-s.y
			}
			
			
		},
		render: function(){
			this.remove();
			var d = this.pointsToData();
			//console.log(this.id, "pts:", dojo.toJson(this.points))
			//console.log(this.id, "data:", dojo.toJson(d))
				
			this.shape = this.parent.createRect(d)
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
			
			if(this.keys.alt){
				x-=w; y-=h; w*=2; h*=2;
			}
			this.points = [
				{x:x, y:y}, // TL
				{x:x+w, y:y},		// TR
				{x:x+w, y:y+h},				// BR
				{x:x, y:y+h}		// BL
			];
			this.render();
		},
		
		onUp: function(obj){
			if(this.created || !this.shape){ return; }
			
			// if too small, need to reset
			var o = this.pointsToData();
			if(o.width<this.minimumSize && o.height < this.minimumSize){
				this.remove();
				return;
			}
			
			this.onRender(this);
			
		}
		
	}
);