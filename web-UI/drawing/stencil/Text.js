dojo.provide("drawing.stencil.Text");

(function(){
	
	drawing.stencil.Text = drawing.util.oo.declare(
		drawing.stencil._Base,
		function(options){
			
		},
		{
			type:"drawing.stencil.Text",
			anchorType:"none",
			align:"start", 	// start, middle, end
			valign:"top",	// top, middle, bottom (TODO: bottom )
			_lineHeight:1,
			
			setText: function(text){
				this._text = text;
				this._textArray = [];
				this.render(text);
			},
			
			getText: function(){
				return this._text;	
			},
			
			render: function(/* String | Array */text){
				//console.trace();
				this.remove(this.shape, this.hit);
				!this.annotation && this.renderOutline();
				if(text){
					this._text = text;
					this._textArray = this._text.split("\n");	
				}
				
				var d = this.pointsToData();
				//console.log("Y:", d.y, "TS:", this.textSize, "LH:", this._lineHeight)
				var w = d.width;
				var h = this._lineHeight;
				var x = d.x + this.style.text.pad*2;
				var y = d.y + this._lineHeight - (this.textSize*.4);
				if(this.valign=="middle"){
					y -= h/2;
				}
				this.shape = this.container.createGroup();
				//console.info("render text:", y, " ... ", this._text, "enabled:", this.enabled);
				
				dojo.forEach(this._textArray, function(txt, i){
					var tb = this.shape.createText({x: x, y: y+(h*i), text: unescape(txt), align: this.align})
						.setFont(this.style.currentText)
						.setFill(this.style.currentText.color);
					
					this._setNodeAtts(tb);
				
				}, this);
				
				this._setNodeAtts(this.shape);
				
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
				
				this.hit = this.container.createRect(d)
					.setStroke(this.style.currentHit)
					.setFill(this.style.currentHit.fill);
				
				this._setNodeAtts(this.hit);
				this.hit.moveToBack();
			},
			
			dataToPoints: function(o){
				o = o || this.data;
				var w = o.width =="auto" ? 1 : o.width;
				var h = o.height || this._lineHeight;
				this.points = [
					{x:o.x, y:o.y}, 						// TL
					{x:o.x + w, y:o.y},				// TR
					{x:o.x + w, y:o.y + h},	// BR
					{x:o.x, y:o.y + h}				// BL
				];
				return this.points;
			},
			pointsToData: function(p){
				p = p || this.points;
				var s = p[0];
				var e = p[2];
				this.data = {
					x: s.x,
					y: s.y,
					width: e.x-s.x,
					height: e.y-s.y
				};
				return this.data;
			}
		}
	);
	
	drawing.stencil.Text.name = "Text";
	drawing.stencil.Text.drawable = false;
	
})();