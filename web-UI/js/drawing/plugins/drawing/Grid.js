dojo.provide("drawing.plugins.drawing.Grid");
dojo.require("drawing.plugins._Plugin");

drawing.plugins.drawing.Grid = drawing.util.oo.declare(
	drawing.plugins._Plugin,
	function(options){
		var vrl;
		dojo.connect(this.canvas, "resize", this, function(){
			clearTimeout(vrl);
			vrl = setTimeout(dojo.hitch(this, function(){
				this.setGrid();
			}),100);
		})
		this.setGrid();	
	},
	{
		gap:100,
		type:"drawing.plugins.drawing.Grid",
		setGrid: function(options){
			return
			console.log("SET GRID")
			//
			// TODO: Shift grid for scroll
			// TODO: major minor lines
			//	minors dont show on zoom out
			//	draw minors first
			//
			var d = this.gap 
			
			this.grid && this.grid.removeShape();
			
			var x1,x2,y1,y2;
			var s = this.canvas.underlay.createGroup();
			var w = this.canvas.width;
			var h = this.canvas.height;
			var b = 1;
			var c = "#A3ECFE";
			
			
			var createGridLine = function(x1,y1,x2,y2){
				s.createLine({x1: x1, y1: y1, x2: x2, y2: y2}).setStroke({style: "Solid", width: b, cap: "round", color:c});
			}
			// horz
			for(var i=1,len = h/d; i<len; i++){
				x1 = 0, x2 = w;
				y1 = d*i, y2 = y1;
				createGridLine(x1,y1,x2,y2);
			}
			// vert
			for(var i=1,len = w/d; i<len; i++){
				y1 = 0, y2 = h;
				x1 = d*i, x2 = x1;
				createGridLine(x1,y1,x2,y2);
			}
			s.moveToBack();
			this.grid = s;
			this.util.attr(s, "id", "grid");
			return s;
		}
	}
);