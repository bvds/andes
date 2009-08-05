dojo.provide("drawing.annotations.Angle");

drawing.annotations.Angle = drawing.util.oo.declare(
	// summary:
	//	When initiated, an HTML box will hover near the Stencil,
	//	displaying it's angle while drawn or modified. Currently
	//	only works with Vector, Line, Arrow, and Axes.
	// description:
	//	Annotation is positioned with drawing.util.positioning.angle
	//	That method should be overwritten for custom placement.
	//	Called internally. To initiaize:
	//	TODO: currently always on
	//
	function(/*Object*/options){
		// arguments:
		//	options: Object
		//		One key value: the stencil that called this.
		//
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
			//	summary:
			//		Called to display angle
			//
			if(!this.stencil.selected && this.stencil.created){ return; }
			if(this.stencil.getRadius() < this.stencil.minimumSize){
				this.hideAngle();
				return;
			}
			var sc = this.mouse.scrollOffset();
			var node = this.getAngleNode();
			var d = this.stencil.pointsToData();
			var pt = drawing.util.positioning.angle({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
			
			pt.x /= this.mouse.zoom;
			pt.y /= this.mouse.zoom;
			
			var mx = this.stencil.getTransform();
			
			// adding _offX & _offY since this is HTML
			// and we are from the page corner, not
			// the canvas corner
			dojo.style(node, {
				left:  this.stencil._offX + pt.x - sc.left + mx.dx + "px",
				top: this.stencil._offY + pt.y - sc.top + mx.dy + "px",
				align:pt.align
			});
			
			node.innerHTML = Math.ceil(this.stencil.getAngle());
		},
		
		getAngleNode: function(){
			//	summary:
			//		Gets or creates HTMLNode used for display
			if(!this._angleNode){
				this._angleNode = dojo.create("span", null, dojo.body());
				dojo.addClass(this._angleNode, "textAnnotation");
				dojo.style(this._angleNode, "opacity", 1);
			}
			return this._angleNode; //HTMLNode
		},
		
		hideAngle: function(){
			//	summary:
			//		Turns display off.
			//
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