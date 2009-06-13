dojo.provide("drawing.stencil.Rect");


drawing.stencil.Rect = drawing.util.oo.declare(
	drawing.stencil.Stencil,
	function(options){
		// NAME! data? properties? description?
		// data AND OR points
		if(options.data || options.points){
			this.points = options.points || this.dataToPoints(options.data);
			this.render();
		}
	},
	{
		dataToPoints: function(obj){
			return [
				{x:obj.x, y:obj.y}, 			// TL
				{x:obj.x + obj.width, y:obj.y},		// TR
				{x:obj.x + obj.width, y:obj.y + obj.height},				// BR
				{x:obj.x, y:obj.y + obj.height}		// BL
			];
		},
		pointsToData: function(){
			var s = this.points[0];
			var e = this.points[2];
			return {
				x: s.x < e.x ? s.x : e.x,
				y: s.y < e.y ? s.y : e.y,
				width: s.x < e.x ? e.x-s.x : s.x-e.x,
				height: s.y < e.y ? e.y-s.y : s.y-e.y
			};
		},
		render: function(){
			//console.info("render", this._onRender.toString())
			this.remove();
			var d = this.pointsToData()
			//console.log("pts:", dojo.toJson(this.pointsToData()))
			
			// prevent IE8 Infinity bug
			//if(!d.width || !d.height){
			//	return; //////////////////////////////////// check into dojo
			//}
			
			this.shape = this.parent.createRect(d)
				.setStroke(this.style.line)
				.setFill(this.style.fill);
			
		},
		onDrag: function(obj){
			this.points = [
				{x:obj.start.x, y:obj.start.y}, // TL
				{x:obj.x, y:obj.start.y},		// TR
				{x:obj.x, y:obj.y},				// BR
				{x:obj.start.x, y:obj.y}		// BL
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
			
		},
		
		onDown: function(obj){
			dojo.disconnect(this._postRenderCon);
			this._postRenderCon = null;
		}
		
	}
);