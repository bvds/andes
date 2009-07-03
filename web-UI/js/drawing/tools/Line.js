dojo.provide("drawing.tools.Line");

drawing.tools.Line = drawing.util.oo.declare(
	drawing.stencil.Line,
	function(){
		
	},
	{
		draws:true,
		onDrag: function(obj){
			if(this.created){ return; }
			var x1 = obj.start.x,
				y1 = obj.start.y,
				x2 = obj.x,
				y2 = obj.y;
			
			if(this.keys.shift){
				var pt = this.util.snapAngle(obj, .25, this.keys.alt);
				x2 = pt.x;
				y2 = pt.y;
			}
			
			if(this.keys.alt){
				// FIXME:
				//	should double the length of the line
				var dx = x2>x1 ? ((x2-x1)/2) : ((x1-x2)/-2);
				var dy = y2>y1 ? ((y2-y1)/2) : ((y1-y2)/-2);
				//dx*=2;
				//dy*=2;
				x1 -= dx;
				x2 -= dx;
				y1 -= dy;
				y2 -= dy;
			}
			
			this.setPoints([
				{x:x1, y:y1},
				{x:x2, y:y2}
			]);
			this.render();
		},
		
		onUp: function(obj){
			if(this.created || !this.shape){ return; }
			
			// if too small, need to reset
			var o = this.pointsToData();
			if(Math.abs(o.x2-o.x1)<this.minimumSize && Math.abs(o.y2-o.y1)<this.minimumSize){
				this.remove();
				return;
			}
			this.renderedOnce = true;
			this.onRender(this);
		}
	}
);

drawing.tools.Line.setup = {
	name:"drawing.tools.Line",
	tooltip:"Line Tool",
	iconClass:"iconLine"
};