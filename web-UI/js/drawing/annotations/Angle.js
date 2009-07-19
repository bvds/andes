dojo.provide("drawing.annotations.Angle");

drawing.annotations.Angle = drawing.util.oo.declare(
	function(options){
		this.stencil = options.stencil;
		this.util = options.stencil.util;
		this.mouse = options.stencil.mouse;
		
		this.stencil.connectMult([
			["onDrag", this, "showAngle"],
			["onUp", this, "hideAngle"],
			["onTransformBegin", this, "showAngle"],
			["onTransform", this, "showAngle"],
			["onTransformEnd", this, "hideAngle"]
		]);
	},
	{
		type:"drawing.tools.custom",
		angle:0,
		
		showAngle: function(){
			if(!this.stencil.selected && this.stencil.created){ return; }
			if(this.stencil.getRadius() < this.stencil.minimumSize){
				this.hideAngle();
				return;
			}
			var sc = this.mouse.scrollOffset();
			var node = this.getAngleNode();
			var d = this.stencil.pointsToData();
			var pt = drawing.util.positioning.angle({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
			
			// adding _offX & _offY since this is HTML
			// and we are from the page corner, not
			// the canvas corner
			dojo.style(node, {
				left:  this.stencil._offX + pt.x - sc.left + "px",
				top: this.stencil._offY + pt.y - sc.top + "px",
				align:pt.align
			});
			
			node.innerHTML = Math.ceil(this.stencil.getAngle());
		},
		
		getAngleNode: function(){
			if(!this._angleNode){
				this._angleNode = dojo.create("span", null, dojo.body());
				dojo.addClass(this._angleNode, "textAnnotation");
				dojo.style(this._angleNode, "opacity", 1);
			}
			return this._angleNode;
		},
		
		hideAngle: function(){
			if(this._angleNode && dojo.style(this._angleNode, "opacity")>0.9){
				
				dojo.fadeOut({node:this._angleNode,
					duration:500,
					onEnd: function(node){
						dojo.destroy(node);
					}
				}).play();
				this._angleNode = null;
			}
			
		}
	}
	
)