dojo.provide("drawing.custom.Axes");

drawing.custom.Axes = drawing.util.oo.declare(
	drawing.stencil.Path,
	drawing.custom.Base,
	function(options){
		//this.style.norm.fill = null;
		//this.style.selected.fill = null;
		this.closePath = false;
		
		this.slaves = new drawing.util.SubStencil(this);
		this.xArrow = this.slaves.add(drawing.stencil.Path);
		this.yArrow = this.slaves.add(drawing.stencil.Path);
		
		this.connect(this.xArrow, "onBeforeRender", this, function(){
			var o = this.points[0];
			var c = this.points[1];
			this.xArrow.points = this.util.arrowHead(c.x, c.y, o.x, o.y, this.style);
		});
		
		this.connect(this.yArrow, "onBeforeRender", this, function(){
			var o = this.points[2];
			var c = this.points[1];
			this.yArrow.points = this.util.arrowHead(c.x, c.y, o.x, o.y, this.style);
		});
	},
	{
		type:"drawing.custom.Axes",
		
		onTransform: function(anchor){
			// the xaxis point has change - the center will not.
			// need to find the yaxis point.
			var o = this.points[0];
			var c = this.points[1];
			var ox = c.x - (c.y - o.y);
			var oy = c.y - (o.x - c.x);
			this.points[2] = {x:ox, y:oy, noAnchor:true};
			this.render();	
		},
		pointsToData: function(){
			var d = this.points;
			return {
				x1:d[1].x,
				y1:d[1].y,
				x2:d[0].x,
				y2:d[0].y
			};
		},
		
		onDrag: function(obj){
			
			this.points = [{x:obj.x, y:obj.y}, {x:obj.start.x, y:obj.start.y, noAnchor:true}]
			var ox = obj.start.x - (obj.start.y - obj.y)
			var oy = obj.start.y - (obj.x - obj.start.x)
			this.points.push({x:ox, y:oy, noAnchor:true});
			this.render();
		},
		onUp:function(){
			this.onRender(this);
		}
	}
)