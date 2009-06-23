dojo.provide("drawing.stencil.Text");

(function(){
	
	drawing.stencil.Text = drawing.util.oo.declare(
		drawing.stencil.Stencil,
		function(options){
			this._lineHeight = this.style.text.size * 1.5;
		},
		{
			anchorType:"none",
			render: function(/* String | Array */text){
				this.remove(this.shape, this.hit);
			
				if(text){
					this._text = text;
					this._textArray = text.split("\n");	
				}
				console.log("render", this._text);
					
				var d = this.pointsToData();
				var x = d.x + this.style.text.pad*2;
				var y = d.y + this._lineHeight - (this.style.text.size*.3);
				var h = this._lineHeight;
				this.shape = this.parent.createGroup();
				
				dojo.forEach(this._textArray, function(txt, i){
					var tb = this.shape.createText({x: x, y: y+(h*i), text: txt, align: "start"})
						.setFont({family: this.style.text.fontFamily, size: this.style.text.size+"pt", weight: "normal"})
						.setFill("black");
					
					this.util.attr(tb, "drawingType", "stencil");
					
				}, this);
				this.util.attr(this.shape, "drawingType", "stencil");
			},
			createSelectionOutline: function(){
				var d = this.pointsToData();
				this.hit = this.parent.createRect(d)
					.setStroke(this.currentHitStyle)
					.setFill(this.currentHitStyle.fill);
				
				this.util.attr(this.hit, "drawingType", "stencil");
				this.hit.moveToBack();
			},
			
			dataToPoints: function(obj){
				return [
					{x:obj.x, y:obj.y}, 						// TL
					{x:obj.x + obj.width, y:obj.y},				// TR
					{x:obj.x + obj.width, y:obj.y + obj.height},// BR
					{x:obj.x, y:obj.y + obj.height}				// BL
				];
			},
			pointsToData: function(){
				var s = this.points[0];
				var e = this.points[2];
				return {
					x: s.x,
					y: s.y,
					width: e.x-s.x,
					height: e.y-s.y
				}
			}
		}
	);
	
})();