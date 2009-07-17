dojo.provide("drawing.stencil._Label");
dojo.require("drawing.stencil.Text");

drawing.stencil._Label = drawing.util.oo.declare(
	drawing.stencil.Text,	
	function(options){
		this.master = options.stencil;
		this.labelPosition = options.labelPosition || "BR"; // TL, TR, BR, BL, or function
		if(dojo.isFunction(this.labelPosition)){
			this.setLabel = this.setLabelCustom;	
		}
		this.setLabel(options.text || "");
		this.connect(this.master, "onTransform", this, "setLabel");
		this.connect(this.master, "destroy", this, "destroy");
	},{
		_align:"start",
		
		setLabelCustom: function(text){
			var d = dojo.hitch(this.master, this.labelPosition)();
			this.setData({
				x:d.x,
				y:d.y,
				width:d.w || this.style.text.minWidth,
				height:d.h || this._lineHeight
			});
			
			// is an event, not text:
			if(text && !text.split){ text = null; }
			
			this.render(text);
		},
		
		setLabel: function(text){
			// onTransform will pass an object here
			var x, y, box = this.master.getBounds();
			
			if(/B/.test(this.labelPosition)){
				y = box.y2 - this._lineHeight;
			}else{
				y = box.y1;
			}
			
			if(/R/.test(this.labelPosition)){
				x = box.x2;
			}else{
				y = box.y1;
				this._align = "end";
			}
			
			if(!this.labelWidth || (text && text.split && text != this._text)){ //????????????????????????????????????
				this.setData({
					x:x,
					y:y,
					height:this._lineHeight,
					width:this.style.text.minWidth
				});
			
				this.labelWidth = this.style.text.minWidth
				this.render(text);
				
				
			}else{
				
				this.setData({
					x:x,
					y:y,
					height:this.data.height,
					width:this.data.width
				});
				
				this.render();	
			}
			
			
			
			
		}
	}

);