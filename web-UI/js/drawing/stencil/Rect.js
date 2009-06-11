dojo.provide("drawing.stencil.Rect");


drawing.stencil.Rect = drawing.util.oo.declare(
	drawing.stencil.Stencil,
	function(options){
		
	},
	{
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
		render: function(data){
			this.remove();
			this.shape = this.parent.createRect(this.pointsToData())
				.setStroke(this.style.line)
				.setFill(this.style.fill);
			
			this._onRender(data);
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
		
		onDown: function(obj){
			this._onRender = function(){}
		},
		
		onUp: function(obj){
			if(this.created || !this.shape){ return; }
			
			// if too small, need to reset
			
			this.created = true;
			this.onRender(this);
			this._onRender = this.onRender;
		}
		
	}
);