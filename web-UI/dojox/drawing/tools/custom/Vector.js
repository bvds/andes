dojo.provide("dojox.drawing.tools.custom.Vector");
dojo.require("dojox.drawing.tools.Arrow");
dojo.require("dojox.drawing.util.positioning");

dojox.drawing.tools.custom.Vector = dojox.drawing.util.oo.declare(
	// summary:
	//		Creates a Vector Stencil.
	// description:
	//		Generally the same as an arrow, except that the arrow
	//		head is only at the end. There is additionaly functionality
	//		to allow for a 'zero vector' - one with no length.
	//
	// 	TODO: Zero Vectors are less than the minimumSize. But if
	//	you get the radius, it will report a length.
	//
	dojox.drawing.tools.Arrow,
	function(options){
		this.minimumSize = this.style.arrows.length;
		
		if(this.style.zAxis) {
			this.zDir = "into";
			//this.sArrow = new dojox.drawing.annotations.ZShadow({stencil:this, style:this.style.shadow, keys:this.keys});
		}
	},
	{
		draws:true,
		type:"dojox.drawing.tools.custom.Vector",
		minimumSize:30,
		showAngle:true,
		zDir:"",
		
		labelPosition: function(){
			// summary:
			//		The custom position used for the label
			//
			var d = this.data;
			var pt = dojox.drawing.util.positioning.label({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
			return {
				x:pt.x,
				y:pt.y
			}
		},
		
		_createZeroVector: function(shp, d, sty){
			// summary:
			//		Special creation function for the zero-vector shape
			//
			var s = shp=="hit" ? this.minimumSize : this.minimumSize/6;
			var f = shp=="hit" ? sty.fill : null;
			d = {
				cx:this.data.x1,
				cy:this.data.y1,
				rx:s,
				ry:s
			};
			
			this.remove(this[shp]);
			this[shp] = this.container.createEllipse(d)
				.setStroke(sty)
				.setFill(f);
			this.util.attr(this[shp], "drawingType", "stencil");
		},
		
		_create: function(/*String*/shp, /*StencilData*/d, /*Object*/sty){
			// summary:
			//		Creates a dojox.gfx.shape based on passed arguments.
			//		Can be called many times by implementation to create
			//		multiple shapes in one stencil.
			//
			this.remove(this[shp]);
			this[shp] = this.container.createLine(d)
				.setStroke(sty);
			this._setNodeAtts(this[shp]);
		},
		
		onDrag: function(/*EventObject*/obj){
			// summary: See stencil._Base.onDrag
			//
			if(this.created){ return; }
			var x1 = obj.start.x,
				y1 = obj.start.y,
				x2 = obj.x,
				y2 = obj.y;
			
			if(this.keys.shift){
				var pt = this.util.snapAngle(obj, 45/180);
				x2 = pt.x;
				y2 = pt.y;
			}
			
			if(this.keys.alt){
				// FIXME:
				//	should double the length of the line
				// FIXME:
				//	if alt dragging past ZERO it seems to work
				//	but select/deselect shows bugs
				var dx = x2>x1 ? ((x2-x1)/2) : ((x1-x2)/-2);
				var dy = y2>y1 ? ((y2-y1)/2) : ((y1-y2)/-2);
				x1 -= dx;
				x2 -= dx;
				y1 -= dy;
				y2 -= dy;
			}
			
			this.setPoints([
				{x:x1, y:y1},
				{x:x2, y:y2}
			]);/*
			if (this.style.zAxis) {
				this.zPoints();
			}*/
			this.render();
		},
		
		zPoints: function() {
			var d = this.pointsToData();
			var angle = this.getAngle();
			d.radius = this.getRadius();
			
			if (angle > 135 && angle < 315) {
				d.angle = this.zAngle;
				this.zDir = "out of";
			} else {
				d.angle = this.util.oppAngle(this.zAngle);
				this.zDir = "into";
			}
			
			var pt = this.util.pointOnCircle(d.x1, d.y1, d.radius, d.angle);
			var p = [
				{x:d.x1, y:d.y1},
				{x:pt.x, y:pt.y}
			];			
			this.setPoints(p);
		},
		
		render: function(){
			// summary:
			//		Renders the 'hit' object (the shape used for an expanded
			//		hit area and for highlighting) and the'shape' (the actual
			//		display object). Additionally checks if Vector should be
			//		drawn as an arrow or a circle (zero-length)
			//
			/*if(this.style.zAxis) {
				this.zPoints();
			}*/
			
			this.onBeforeRender(this);
			if(this.getRadius() >= this.minimumSize){
				this._create("hit", this.data, this.style.currentHit);
				this._create("shape", this.data, this.style.current);
			
			}else{
				this._createZeroVector("hit", this.data, this.style.currentHit);
				this._createZeroVector("shape", this.data, this.style.current);
			}
		},
		onUp: function(/*EventObject*/obj){
			// summary: See stencil._Base.onUp
			//
			if(this.created || !this._downOnCanvas){ return; }
			this._downOnCanvas = false;
			//Default vector for single click
			if(!this.shape){
				s = obj.start;
				obj.y = obj.start.y + 100;
				obj.x = obj.start.x
				this.setPoints([
					{x:s.x, y:s.y},
					{x:s.x, y:s.y+100}
				]);
				this.render();
			}
			
			// if too small, need to reset
			// 		This sets the zero length vector to zero within the minimum size 
			if(this.getRadius()<this.minimumSize){
				var p = this.points; 
				this.setPoints([ 
					{x:p[0].x, y:p[0].y}, 
					{x:p[0].x, y:p[0].y} 
				]); 
			} else { 			
				//ace: needed as else to avoid zero length problem in snapAngle 
				var pt = this.util.snapAngle(obj, this.angleSnap/180);
				var p = this.points;
				this.setPoints([
					{x:p[0].x, y:p[0].y},
					{x:pt.x, y:pt.y}
				]);
				
			}
			this.renderedOnce = true;
			this.onRender(this);
		}
	}
	
);

dojox.drawing.tools.custom.Vector.setup = {
	// summary: See stencil._Base ToolsSetup
	//
	name:"dojox.drawing.tools.custom.Vector",
	tooltip:"Vector Tool",
	iconClass:"iconVector"
};
dojox.drawing.register(dojox.drawing.tools.custom.Vector.setup, "tool");