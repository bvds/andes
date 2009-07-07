dojo.provide("drawing.stencil.Ellipse");


drawing.stencil.Ellipse = drawing.util.oo.declare(
	drawing.stencil._Base,
	function(options){
		if(options.data || options.points){
			//this.points = options.points || this.dataToPoints(options.data);
			//this.render();
		}
	},
	{
		anchorType: "group",
		dataToPoints: function(o){
			o = o || this.data;
			var x = o.cx - o.rx,
				y = o.cy - o.ry,
				w = o.rx*2,
				h = o.ry*2
			this.points = [
				{x:x, y:y}, 	// TL
				{x:x+w, y:y},	// TR
				{x:x+w, y:y+h},	// BR
				{x:x, y:y+h}	// BL
			];
			return this.points;
		},
		pointsToData: function(p){
			// TODO: Should we handle 2 points? aka, like rect(pt,pt)
			p = p || this.points;
			var s = p[0];
			var e = p[2];
			this.data = {
				cx: s.x + (e.x - s.x)/2,
				cy: s.y + (e.y - s.y)/2,
				rx: (e.x - s.x)*.5,
				ry: (e.y - s.y)*.5
			};
			return this.data;
		
		},
		
		_create: function(shp, d, sty){
			this.remove(this[shp]);
			this[shp] = this.parent.createEllipse(d)
				.setStroke(sty)
				.setFill(sty.fill);
			this.util.attr(this[shp], "drawingType", "stencil");
		},
		
		render: function(){
			this.onBeforeRender(this);
			this._create("hit", this.data, this.style.currentHit);
			this._create("shape", this.data, this.style.current);
		}
		
	}
);