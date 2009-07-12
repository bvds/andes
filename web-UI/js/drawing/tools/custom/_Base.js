dojo.provide("drawing.tools.custom._Base");


drawing.tools.custom._Base = drawing.util.oo.declare(
	function(options){
		this.connectMult([
			["onDrag", "showAngle"],
			["onUp", "hideAngle"],
			["onTransformBegin", "showAngle"],
			["onTransform", "showAngle"],
			["onTransformEnd", "hideAngle"]
		]);
		this.angleSnap = 1;
	},
	{
		type:"drawing.tools.custom",
		
		showAngle: function(){
			console.warn("SHOW ANGLE showing:", this._angleShowing)
			console.trace();
			var node = this.getAngleNode();
			var d = this.pointsToData();
			var obj = {
				start:{
					x:d.x1,
					y:d.y1
				},
				x:d.x2,
				y:d.y2
			};
			var angle = this.util.angle(obj, this.angleSnap);

			var pt = drawing.util.positioning.angle({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
			
			// adding _offX & _offY since this is HTML
			// and we are from the page corner, not
			// the canvas corner
			dojo.style(node, {
				left:  this._offX + pt.x + "px",
				top: this._offY + pt.y + "px",
				align:pt.align
			});
			
			// reversing the angle for display: 0 -> 180, 90 -> 270
			angle = 180 - angle; angle = angle==360 ? 0 : angle;
			
			node.innerHTML = Math.ceil(angle);
			this._angleShowing = true;
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
			console.warn("HIDE ANGLE showing:", this._angleShowing)
			if(this._angleNode && this._angleShowing){
				var self = this;
				dojo.fadeOut({node:this._angleNode,
					duration:500,
					onEnd: function(node){
						dojo.destroy(node);
						self._angleShowing = false;
					}
				}).play();
				this._angleNode = null;
			}
			
		}
	}
	
)