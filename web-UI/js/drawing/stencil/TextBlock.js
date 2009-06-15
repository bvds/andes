dojo.provide("drawing.stencil.TextBlock");


drawing.stencil.TextBlock = drawing.util.oo.declare(
	drawing.stencil.Stencil,
	function(options){
		if(options.data || options.points){
			this.points = options.points || this.dataToPoints(options.data);
			this.render();
		}
	},
	{
		onUp: function(obj){
			this.textblock = this.parent.createText({x: obj.x, y: obj.y, text: "Two\nLine", align: "start"})
			.setFont({family: "Times", size: "36pt", weight: "bold"})
			.setFill("black")
			.setStroke("red");
			
			console.time("text meas")
			console.warn("width:", this.textblock.getTextWidth())
			console.timeEnd("text meas")
			
			console.time("text meas")
			var b = this.textblock.getTextBoundingBox()
			console.warn("bbox:", b.width," ",b.height," ",b.x," ",b.y)
			
			console.timeEnd("text meas")
			
			console.warn("shape:", this.textblock)
		},
		onDown: function(obj){
			
		}
	}
);