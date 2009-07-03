dojo.provide("drawing.tools.custom.Axes");
dojo.require("drawing.stencil.Path");
dojo.require("drawing.stencil._Slave");
dojo.require("drawing.tools.custom._Base");


drawing.tools.custom.Axes = drawing.util.oo.declare(
	drawing.stencil.Path,
	drawing.tools.custom._Base,
	function(options){
		//this.style.norm.fill = null;
		//this.style.selected.fill = null;
		this.closePath = false;
		
		this.slaves = new drawing.stencil._Slave(this);
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
		draws:true,
		type:"drawing.custom.Axes",
		
		onTransformEnd: function(anchor){
			//	tell anchor to go to prev point if wrong
			// called from anchor point up mouse up
			this.isBeingModified = false;
			
			var o = this.points[0];
			var c = this.points[1];
			var pt = this.util.constrainAngle({start:{x:c.x, y:c.y}, x:o.x, y:o.y}, 91, 180);
			if(pt.x==o.x && pt.y == o.y){
				return;
			}
			// careful changing these points. The anchors are
			// linked as an instance to the objects
			this.points[0].x = pt.x
			this.points[0].y = pt.y;
			o = this.points[0];
			
			var ox = c.x - (c.y - o.y);
			var oy = c.y - (o.x - c.x);
			
			this.points[2] = {x:ox, y:oy, noAnchor:true};
			this.setPoints(this.points);
			this.render();	
			anchor.reset();
			this.showLabel();
		},
		
		onTransform: function(anchor){
			// the xaxis point has changed - the center will not.
			// need to find the yaxis point.
			
			var o = this.points[0];
			var c = this.points[1];
			var ox = c.x - (c.y - o.y);
			var oy = c.y - (o.x - c.x);
			
			this.points[2] = {x:ox, y:oy, noAnchor:true};
			this.setPoints(this.points);
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
			
			var pt = this.util.constrainAngle(obj, 91, 180);
			obj.x = pt.x;
			obj.y = pt.y;
			this.points = [{x:obj.x, y:obj.y}, {x:obj.start.x, y:obj.start.y, noAnchor:true}]
			var ox = obj.start.x - (obj.start.y - obj.y);
			var oy = obj.start.y - (obj.x - obj.start.x);
			
			this.points.push({x:ox, y:oy, noAnchor:true});
			this.render();
		},
		onUp:function(){
			this.onRender(this);
		}
	}
);

drawing.tools.custom.Axes.setup = {
	name:"drawing.tools.custom.Axes",
	tooltip:"Axes Tool",
	iconClass:"iconAxes"
};
