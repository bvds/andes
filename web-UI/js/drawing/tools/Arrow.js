dojo.provide("drawing.tools.Arrow");

drawing.tools.Arrow = drawing.util.oo.declare(
	drawing.tools.Line,
	function(options){
		
		
		if(this.arrowStart){
			this.begArrow = new drawing.annotations.Arrow({stencil:this, idx1:0, idx2:1});
		}
		
		if(this.arrowEnd){
			this.endArrow = new drawing.annotations.Arrow({stencil:this, idx1:1, idx2:0});
		}
		
		if(this.data || this.points && this.points.length){
			this.render();
		}
	},
	{
		draws:true,
		type:"drawing.tools.Arrow",
		arrowStart:true,
		arrowEnd:true,
		
		
		onUp: function(obj){
			if(this.created || !this.shape){ return; }
			
			// if too small, need to reset
			var p = this.points;
			var len = this.util.distance(p[0].x,p[0].y,p[1].x,p[1].y);
			if(len<this.minimumSize){
				this.remove(this.shape, this.hit);
				//this.begArrow && this.begArrow.remove(this.begArrow.shape, this.begArrow.hit);
				//this.endArrow && this.endArrow.remove(this.endArrow.shape, this.endArrow.hit);
				return;
			}
			
			var pt = this.util.snapAngle(obj, this.angleSnap/180);
			this.setPoints([
				{x:p[0].x, y:p[0].y},
				{x:pt.x, y:pt.y}
			]);
			
			this.renderedOnce = true;
			this.onRender(this);
		}
	}
);

drawing.tools.Arrow.setup = {
	name:"drawing.tools.Arrow",
	tooltip:"Arrow Tool",
	iconClass:"iconArrow"
};
