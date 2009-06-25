dojo.provide("drawing.library.Arrow");


drawing.library.Arrow = drawing.util.oo.declare(
	drawing.stencil.Line,
	function(options){
		// note: pass this.id to annotation
		this.arrow = new drawing.stencil.Path({style:this.style, id:this.id, parent:this.parent, mouse:this.mouse});
		
		this.con([
			["select", this.arrow, "select"],
			["deselect", this.arrow, "deselect"],
			[this.arrow, "select", "select"],
			[this.arrow, "deselect", "deselect"],
			["render", "renderArrow"]
		]);
		
		// if passed data, we need to re-render, since the render has
		// already happened before getting to this constructor.
		if(options.data || options.points){
			this.render();
		}

	},
	{
		type:"drawing.library.Arrow",
		renderArrow: function(){
			//
			// FIXME: if too small don't render or remove
			//
			var d = this.pointsToData();
			var angle = this.util.angle({
				start:{
					x:d.x2,
					y:d.y2
				},
				x:d.x1,
				y:d.y1
			});
			
			
			var al = this.style.arrows.length;
			var aw = this.style.arrows.width/2;
			
			var p1 = this.util.pointOnCircle(d.x2, d.y2, al, angle-aw);
			var p2 = this.util.pointOnCircle(d.x2, d.y2, al, angle+aw);
			
			this.arrow.points = [
				{x:d.x2,y:d.y2},
				p1,
				p2
			];
			
			// FIXME: something more elegant?
			//this.arrow.currentStyle = this.currentStyle;
			//this.arrow.currentHitStyle = this.currentHitStyle;	
		
			this.arrow.render();
			
		}
	}
);
