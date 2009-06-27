dojo.provide("drawing.stencil.Path");


drawing.stencil.Path = drawing.util.oo.declare(
	drawing.stencil.Stencil,
	function(options){
		dojo.disconnect(this._postRenderCon);
		
		if(options.points){
			this.points = options.points;
			this.render();
		}
	},
	{
		type:"drawing.stencil.Path",
		closePath: true,
		
		_create: function(shp, sty){
			this.remove(this[shp]);
			this[shp] = this.parent.createPath({}).setStroke(sty);
			this.closePath && this[shp].setFill(sty.fill);
			dojo.forEach(this.points, function(o, i){
				if(i==0){
					this[shp].moveTo(o.x, o.y);
				}else{
					this[shp].lineTo(o.x, o.y);
				}
			}, this);
			this.closePath && this[shp].closePath();
			this.util.attr(this[shp], "drawingType", "stencil");
		},
		
		render: function(){
			this.onBeforeRender(this);
			this._create("hit", this.style.currentHit);
			this._create("shape", this.style.current);
		}		
		
	}
);