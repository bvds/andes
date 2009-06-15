dojo.provide("drawing.stencil.Line");


drawing.stencil.Line = drawing.util.oo.declare(
	drawing.stencil.Stencil,
	function(options){
		if(options.data || options.points){
			this.points = options.points || this.dataToPoints(options.data);
			this.render();
		}
	},
	{
		anchorType: "single",
		dataToPoints: function(obj){
			this.points = [
				{x:obj.x1, y:obj.y1},
				{x:obj.x2, y:obj.y2}
			];
		},
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
		},
		createSelectionOutline: function(){
			this.hit = this.parent.createLine(this.pointsToData())
				.setStroke(this.style.hitline);
			dojo.attr(this.hit.rawNode, "drawingType", "stencil");
			this.hit.moveToBack();
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
		
		onUp: function(obj){
			if(this.created || !this.shape){ return; }
			
			// if too small, need to reset
			var o = this.pointsToData();
			if(Math.abs(o.x2-o.x1)<this.minimumSize && Math.abs(o.y2-o.y1)<this.minimumSize){
				this.remove();
				return;
			}
			this.onRender(this);
		}
	}
);