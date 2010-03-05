dojo.provide("dojox.drawing.annotations.ZShadow");
dojo.require("dojox.drawing.tools.Arrow");

dojox.drawing.annotations.ZShadow = dojox.drawing.util.oo.declare(
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
		dojo.connect(this, "onPostRender", this, "moveToBack");
		dojo.connect(this.stencil, "onDelete", this, "destroy");
		dojo.connect(this.stencil, "onDrag", this, "zPoints");
		/*this.sArrow.connectMult([
		[this.stencil, "select", this, "select"],
		[this.stencil, "deselect", this, "deselect"],
		[this.sArrow, "onBeforeRender", this, "render"],
		[this.sArrow, "onDelete", this, "destroy"]
		]);*/
	},
	{
		draws:true,
		type:"dojox.drawing.tools.Arrow",
		minimumSize:30,
		
		zPoints: function(type) {
			if(this.created){ return; }
			var d = this.stencil.data;
			d.radius = this.stencil.getRadius();
			d.angle = this.zAngle - 30;
			
			var pt = this.util.pointOnCircle(d.x1, d.y1, d.radius, d.angle);
		
			p = [
				{x:d.x1, y:d.y1},
				{x:pt.x, y:pt.y}
			];
			this.setPoints(p);
			this.render();

		},
		
		onDrag: function(/*EventObject*/obj){
			// summary: See stencil._Base.onDrag
			//
			
		},
		
		render: function(){
			// summary:
			//		Renders the 'hit' object (the shape used for an expanded
			//		hit area and for highlighting) and the'shape' (the actual
			//		display object). Additionally checks if Vector should be
			//		drawn as an arrow or a circle (zero-length)
			//
			
			if(this.getRadius() >= this.minimumSize && this.stencil.zDir == "out"){
				this._create("hit", this.data, this.style.currentHit);
				this._create("shape", this.data, this.style.current);
			
			}else{
				//this.remove(this[shp]);
			}
		},
		
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