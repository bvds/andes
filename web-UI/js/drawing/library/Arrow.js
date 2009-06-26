dojo.provide("drawing.library.Arrow");


drawing.library.Arrow = drawing.util.oo.declare(
	drawing.stencil.Line,
	function(options){
		
		this.slaves = new drawing.util.SubStencil(this);
		
		if(this.arrowStart){
			this.begArrow = this.slaves.add(drawing.stencil.Path);
			this.connect(this.begArrow, "onBeforeRender", this, function(){
				var o = this.points[0];
				var c = this.points[1];
				this.begArrow.points = this.util.arrowHead(c.x, c.y, o.x, o.y, this.style);
			});
		}
		
		if(this.arrowEnd){
			this.endArrow = this.slaves.add(drawing.stencil.Path);
			this.connect(this.endArrow, "onBeforeRender", this, function(){
				var o = this.points[1];
				var c = this.points[0];
				this.endArrow.points = this.util.arrowHead(c.x, c.y, o.x, o.y, this.style);
			});
		}
		if(this.data || this.points && this.points.length){
			this.render();
		}
	},
	{
		type:"drawing.library.Arrow",
		arrowStart:false,
		arrowEnd:true
	}
);
