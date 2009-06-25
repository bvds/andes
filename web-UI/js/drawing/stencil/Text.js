dojo.provide("drawing.stencil.Text");

(function(){
	
	drawing.stencil.Text = drawing.util.oo.declare(
		drawing.stencil.Stencil,
		function(options){
			this._lineHeight = this.style.text.size * 1.5;
		},
		{
			//
			// TODO - width:auto
			//
			type:"drawing.stencil.Text",
			anchorType:"none",
			render: function(/* String | Array */text, /* ? String */align){
				//console.time("render text");
				this.remove(this.shape, this.hit);
				this.renderOutline();
				if(text){
					this._text = text;
					this._textArray = text.split("\n");	
				}
				align = align || "start";
				//console.log("render text:", this._text);
					
				var d = this.pointsToData();
				var x = d.x + this.style.text.pad*2;
				var y = d.y + this._lineHeight - (this.style.text.size*.3);
				var h = this._lineHeight;
				this.shape = this.parent.createGroup();
				
				dojo.forEach(this._textArray, function(txt, i){
					console.log("align:", align)
					var tb = this.shape.createText({x: x, y: y+(h*i), text: txt, align: "start"})
						.setFont({family: this.style.text.fontFamily, size: this.style.text.size+"pt", weight: "normal", align:align})
						.setFill("black");
					this.util.attr(tb, "drawingType", "stencil");
					
				}, this);
				if(!this.annotation){
					this.util.attr(this.shape, "drawingType", "stencil");
				}
				//this.util.attr(this.shape, "id", this.id);
				//console.timeEnd("render text");
			},
			renderOutline: function(){
				var d = this.pointsToData();
				this.hit = this.parent.createRect(d)
					.setStroke(this.style.currentHit)
					.setFill(this.style.currentHit.fill);
				if(!this.annotation){
					this.util.attr(this.hit, "drawingType", "stencil");
				}
				this.hit.moveToBack();
			},
			
			dataToPoints: function(obj){
				return [
					{x:obj.x, y:obj.y}, 						// TL
					{x:obj.x + obj.width, y:obj.y},				// TR
					{x:obj.x + obj.width, y:obj.y + (obj.height || this._lineHeight)},// BR
					{x:obj.x, y:obj.y + (obj.height || this._lineHeight)}				// BL
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