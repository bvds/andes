dojo.provide("drawing.tools.Ellipse");

drawing.tools.Ellipse = drawing.util.oo.declare(
	drawing.stencil.Ellipse,
	function(){
		
	},
	{
		draws:true,
		onDrag: function(/*dojox.__MangerMouseEvent*/obj){
			// summary: See stencil._Base.onDrag
			//
			var s = obj.start, e = obj;
			var	x = s.x < e.x ? s.x : e.x,
				y = s.y < e.y ? s.y : e.y,
				w = s.x < e.x ? e.x-s.x : s.x-e.x,
				h = s.y < e.y ? e.y-s.y : s.y-e.y;
			
			if(this.keys.shift){ w = h = Math.max(w,h); }
			if(!this.keys.alt){ // ellipse is normally on center
				x+=w/2; y+=h/2; w/=2; h/=2;
			} else{
				if(y - h < 0){ h = y; }
				if(x - w < 0){ w = x; }
			}
			
			this.points = [
				{x:x-w, y:y-h}, 	// TL
				{x:x+w, y:y-h},		// TR
				{x:x+w, y:y+h},		// BR
				{x:x-w, y:y+h}		// BL
			];
			this.render();
		},
		
		onUp: function(/*dojox.__MangerMouseEvent*/obj){
			// summary: See stencil._Base.onUp
			//
			if(this.created || !this.shape){ return; }
			
			// if too small, need to reset
			var o = this.pointsToData();
			if(o.rx*2<this.minimumSize && o.ry*2 < this.minimumSize){
				this.remove(this.shape, this.hit);
				return;
			}
			
			this.onRender(this);
			
		}
	}
);

drawing.tools.Ellipse.setup = {
	// summary: See stencil._Base dojox.__ToolsSetup
	//	
	name:"drawing.tools.Ellipse",
	tooltip:"Ellipse Tool",
	iconClass:"iconEllipse"
};