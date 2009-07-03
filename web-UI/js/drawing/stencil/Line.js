dojo.provide("drawing.stencil.Line");

drawing.stencil.Line = drawing.util.oo.declare(
	drawing.stencil._Base,
	function(options){
		
	},
	{
		type:"drawing.stencil.Line",
		anchorType: "single",
		dataToPoints: function(o){
			o = o || this.data;
			this.points = [
				{x:o.x1, y:o.y1},
				{x:o.x2, y:o.y2}
			];
			return this.points;
		},
		pointsToData: function(p){
			p = p || this.points;
			this.data = {
				x1: p[0].x,
				y1: p[0].y,
				x2: p[1].x,
				y2: p[1].y
			};
			return this.data;
		},
		_create: function(shp, d, sty){
			this.remove(this[shp]);
			this[shp] = this.parent.createLine(d)
				.setStroke(sty);
			this.util.attr(this[shp], "drawingType", "stencil");
		},
		
		render: function(){
			var d = this.data;
			this.onBeforeRender(this);
			this._create("hit", d, this.style.currentHit);
			//if(!this.annotation)
			this._create("shape", d, this.style.current);
		}		
		
	}
);
drawing.stencil.Line.name = "Line";
drawing.stencil.Line.drawable = true;