dojo.provide("drawing.stencil.Line");


drawing.stencil.Line = drawing.util.oo.declare(
	drawing.stencil.Stencil,
	function(options){
		
	},
	{
		pointsToData: function(){
			return {
				x1: this.points[0].x,
				y1: this.points[0].y,
				x2: this.points[1].x,
				y2: this.points[1].y
			};
		},
		render: function(data){
			this.remove(this.hit, this.shape);
			this.shape = this.parent.createLine(this.pointsToData())
				.setStroke(this.style.line);
			
			this._onRender(data);
		},
		createHitline: function(){
			this.hit = this.parent.createLine(this.pointsToData())
				.setStroke(this.style.hitline);
				
			this.shape.moveToFront();	
			
		},
		
		onDrag: function(obj){
			if(this.created){ return; }
			// TODO
			// Check SHIFT and constrain angle
			
			this.points = [
				{x:obj.start.x, y:obj.start.y},
				{x:obj.x, y:obj.y}
			];
			
			this.render();
		},
		
		onDown: function(obj){
			this._onRender = function(){}
		},
		
		onUp: function(obj){
			if(this.created || !this.shape){ return; }
			
			// if too small, need to reset
			
			// SHOULD anchors and renderData be the same?
			// For native shapes maybe?
			
			this.createHitline();
			this.created = true;
			this.onRender(this);
			this._onRender = this.onRender;
		}
	}
);