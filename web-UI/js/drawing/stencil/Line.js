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
			if(o.radius || o.angle){
				// instead of using x1,x2,y1,y1,
				// it's been set as x,y,angle,radius
				
				// forward angle:
				// 90 -> 90
				// 180 -> 0
				// 0 -> 180
				// 270 -> 270
				// 120 -> 60
				// 40 -> 140
				// 315 -> 220
				// 200 -> 340
				
				// reversing the angle for display: 0 -> 180, 90 -> 270
				//angle = 180 - angle; angle = angle==360 ? 0 : angle;
				
				var was = o.angle
				o.angle = (180-o.angle)<0 ? 180-o.angle+360 : 180-o.angle;
				
				//console.log(" ---- angle:", was, "to:", o.angle)
				var pt = this.util.pointOnCircle(o.x,o.y,o.radius,o.angle);
				//console.log(" ---- pts:", pt.x, pt.y);
				this.data = o = {
					x1:o.x,
					y1:o.y,
					x2:pt.x,
					y2:pt.y
				}
				
			}
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
			this._setNodeAtts(this[shp]);
		},
		
		render: function(){
			this.onBeforeRender(this);
			this._create("hit", this.data, this.style.currentHit);
			//if(!this.annotation)
			this._create("shape", this.data, this.style.current);
		}		
		
	}
);
drawing.stencil.Line.name = "Line";
drawing.stencil.Line.drawable = true;