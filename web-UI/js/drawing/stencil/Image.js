dojo.provide("drawing.stencil.Image");


drawing.stencil.Image = drawing.util.oo.declare(
	drawing.stencil._Base,
	function(options){
		if(options.data){
			this.src = options.data.src;
		}
	},
	{
		type:"drawing.stencil.Image",
		anchorType: "group",
		_createHilite: function(){
			this.remove(this.hit);
			this.hit = this.parent.createRect(this.data)
				.setStroke(this.style.current)
				.setFill(this.style.current.fill);
			this._setNodeAtts(this.hit);
		},
		_create: function(shp, d, sty){
			
			this.remove(this[shp]);
			var s = this.parent.getParent();
			this[shp] = s.createImage(d)
			this.parent.add(this[shp]);
			this._setNodeAtts(this[shp]);
		},
		
		render: function(dbg){
			
			if(this.data.width == "auto" || isNaN(this.data.width)){
				this.getImageSize(true);
				return;
			}
			this.onBeforeRender(this);
			this._createHilite();
			this._create("shape", this.data, this.style.current);
		},
		getImageSize: function(render){
			if(this._gettingSize){ return; } // IE gets it twice (will need to mod if src changes)
			this._gettingSize = true;
			var img = dojo.create("img", {src:this.data.src}, dojo.body());
			var c = dojo.connect(img, "load", this, function(){
				var dim = dojo.marginBox(img);
				this.setData({
					x:this.data.x,
					y:this.data.y,
					src:this.data.src,
					width:dim.w,
					height:dim.h
				});
				dojo.disconnect(c);
				dojo.destroy(img);
				render && this.render(true);
			});
		},
		dataToPoints: function(o){
			o = o || this.data;
			this.points = [
				{x:o.x, y:o.y}, 						// TL
				{x:o.x + o.width, y:o.y},				// TR
				{x:o.x + o.width, y:o.y + o.height},	// BR
				{x:o.x, y:o.y + o.height}				// BL
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
				height: e.y-s.y,
				src: this.src || this.data.src
			};
			return this.data;
			
		}
	}
);
drawing.stencil.Image.name = "Image";
drawing.stencil.Image.drawable = false;