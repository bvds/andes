dojo.provide("drawing.stencil.Rect");


drawing.stencil.Rect = drawing.util.oo.declare(
	drawing.stencil._Base,
	function(options){

	},
	{
		type:"drawing.stencil.Rect",
		anchorType: "group",
		dataToPoints: function(o){
			o = o || this.data;
			this.points = [
				{x:o.x, y:o.y}, 						// TL
				{x:o.x + o.width, y:o.y},				// TR
				{x:o.x + o.width, y:o.y + o.height},	// BR
				{x:o.x, y:o.y + o.height}				// BL
			];
			return this.points;
		},
		
		pointsToData: function(p){
			p = p || this.points;
			var s = p[0];
			var e = p[2];
			this.data = {
				x: s.x,
				y: s.y,
				width: e.x-s.x,
				height: e.y-s.y
			};
			return this.data;
			
		},
		
		_create: function(shp, d, sty){
			this.remove(this[shp]);
			this[shp] = this.parent.createRect(d)
				.setStroke(sty)
				.setFill(sty.fill);
			
			this._setNodeAtts(this[shp]);
		},
		
		render: function(){
			this.onBeforeRender(this);
			this._create("hit", this.data, this.style.currentHit);
			this._create("shape", this.data, this.style.current);
		}
	}
);
