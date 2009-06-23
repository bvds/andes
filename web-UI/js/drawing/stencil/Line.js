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
				.setStroke(this.currentStyle);
		},
		createSelectionOutline: function(){
			this.hit = this.parent.createLine(this.pointsToData())
				.setStroke(this.currentHitStyle);
			this.util.attr(this.hit, "drawingType", "stencil");
			this.hit.moveToBack();
		},
		
		onDrag: function(obj){
			if(this.created){ return; }
			
			var x1 = obj.start.x,
				y1 = obj.start.y,
				x2 = obj.x,
				y2 = obj.y;
			
			if(this.keys.shift){
				var pt = this.util.constrainAngle(obj, .25, this.keys.alt);
				x2 = pt.x;
				y2 = pt.y;
			}
			
			if(this.keys.alt){
				// FIXME:
				//	should double the length of the line
				var dx = x2>x1 ? ((x2-x1)/2) : ((x1-x2)/-2);
				var dy = y2>y1 ? ((y2-y1)/2) : ((y1-y2)/-2);
				//dx*=2;
				//dy*=2;
				x1 -= dx;
				x2 -= dx;
				y1 -= dy;
				y2 -= dy;
			}
			
			this.points = [
				{x:x1, y:y1},
				{x:x2, y:y2}
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