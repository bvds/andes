dojo.provide("drawing.tools.Rect");

drawing.tools.Rect = drawing.util.oo.declare(
	// summary:
	// Class for a drawable rectangle
	//
	drawing.stencil.Rect,
	function(){
		// summary: constructor
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
			
			if(this.keys.alt){
				x-=w; y-=h; w*=2; h*=2;
				x = Math.max(x, 0);
				y = Math.max(y, 0);
			}
			this.setPoints ([
				{x:x, y:y}, 	// TL
				{x:x+w, y:y},	// TR
				{x:x+w, y:y+h},	// BR
				{x:x, y:y+h}	// BL
			]);
			this.render();
		},
		
		onUp: function(/*dojox.__MangerMouseEvent*/obj){
			// summary: See stencil._Base.onUp
			//
			if(this.created || !this.shape){ return; }
			
			// if too small, need to reset
			var o = this.data;
			if(o.width<this.minimumSize && o.height < this.minimumSize){
				this.remove(this.shape, this.hit);
				return;
			}
			
			this.onRender(this);
			
		}
	}
);

drawing.tools.Rect.setup = {
	// summary: See stencil._Base dojox.__ToolsSetup
	//
	name:"drawing.tools.Rect",
	tooltip:"Rectangle Tool",
	iconClass:"iconRect"
}