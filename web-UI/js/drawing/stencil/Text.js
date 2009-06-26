dojo.provide("drawing.stencil.Text");

(function(){
	
	drawing.stencil.Text = drawing.util.oo.declare(
		drawing.stencil.Stencil,
		function(options){
			
			this.align = options.align || this.align;
			this.valign = options.valign || this.valign;
			this._lineHeight = this.style.text.size * 1.5;
		},
		{
			type:"drawing.stencil.Text",
			anchorType:"none",
			align:"start", 	// start, middle, end
			valign:"top",	// top, middle, bottom (TODO: bottom )
			
			render: function(/* String | Array */text){
				
				this.remove(this.shape, this.hit);
				this.renderOutline();
				if(text){
					this._text = text;
					this._textArray = text.split("\n");	
				}
				var d = this.pointsToData();
				var w = d.width;
				var h = this._lineHeight;
				var x = d.x + this.style.text.pad*2;
				var y = d.y + this._lineHeight - (this.style.text.size*.3);
				if(this.valign=="middle"){
					y -= h/2;
				}
				this.shape = this.parent.createGroup();
				
				dojo.forEach(this._textArray, function(txt, i){
					var tb = this.shape.createText({x: x, y: y+(h*i), text: txt, align: this.align})
						.setFont({family: this.style.text.fontFamily, size: this.style.text.size+"pt", weight: "normal"})
						.setFill("black");
					this.util.attr(tb, "drawingType", "stencil");
					
				}, this);
				if(!this.annotation){
					this.util.attr(this.shape, "drawingType", "stencil");
				}
			},
			renderOutline: function(){
				if(this.annotation){ return; }
				var d = this.pointsToData();
				
				if(this.align=="middle"){
					d.x -= d.width/2 - this.style.text.pad * 2;
				}else if(this.align=="start"){
					d.x += this.style.text.pad;
				}else if(this.align=="end"){
					d.x -= d.width - this.style.text.pad * 3;
				}
				
				if(this.valign=="middle"){
					d.y -= (this._lineHeight )/2 - this.style.text.pad;
				}
				
				d.y -= (this.style.text.size*.3)
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