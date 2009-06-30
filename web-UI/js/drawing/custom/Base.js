dojo.provide("drawing.custom.Base");
dojo.require("drawing.custom.positioning");

drawing.custom.Base = drawing.util.oo.declare(
	function(options){
		this.connectMult([
			["onDrag", "showAngle"],
			["onUp", "hideAngle"],
			["onTransformBegin", "showAngle"],
			["onTransform", "showAngle"],
			["onTransformEnd", "hideAngle"]
		]);
		this.angleSnap = 1;//id:this.id,
		if(this.type=="drawing.custom.Vector"){
			this.labelText = new drawing.stencil.Text({style:this.style, util:this.util, annotation:true, parent:this.parent, mouse:this.mouse});
		}else if(this.type=="drawing.custom.Axes"){
			var props = {style:this.style, align:"middle", valign:"middle", util:this.util, annotation:true, parent:this.parent, mouse:this.mouse}
			this.labelX = new drawing.stencil.Text(props);
			this.labelY = new drawing.stencil.Text(props);
		}
	},
	{
		type:"drawing.custom",
		
		showLabel: function(){
			var d = this.pointsToData();
			
			if(this.type=="drawing.custom.Vector"){
				var pt = drawing.custom.positioning.label({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
				// do transform instead of rebuilding text
				this.labelText.points = this.labelText.dataToPoints({
					x:  pt.x,
					y:  pt.y,
					width:100
				});
				this.labelText.align = pt.align;
				this.labelText.render("My Label");
			
			}else if(this.type=="drawing.custom.Axes"){
				var ax = this.points[0];
				var c =  this.points[1];
				var ay = this.points[2];
				
				var dist = 40;
				var offdist = 20;
				var pt, px, py, pt2, d;
				
				pt = this.util.lineSub(c.x, c.y, ax.x, ax.y, dist);
				px = pt.x + (pt.y -ax.y);
				py = pt.y + (ax.x - pt.x);
				pt2 = this.util.lineSub(pt.x, pt.y, px, py, (dist-offdist));
				
				this.labelX.points = this.labelX.dataToPoints({
					x:  pt2.x,
					y:  pt2.y,
					width:20
				});
				this.labelX.render("X", "start");
				
				
				pt = this.util.lineSub(c.x, c.y, ay.x, ay.y, dist);
				px = pt.x + ( ay.y-pt.y);
				py = pt.y + (pt.x - ay.x );
				pt2 = this.util.lineSub(pt.x, pt.y, px, py, (dist-offdist));
				pt2 = this.util.lineSub(pt.x, pt.y, px, py, (dist-offdist));
				
				this.labelY.points = this.labelY.dataToPoints({
					x:  pt2.x,
					y:  pt2.y,
					width:20
				});
				this.labelY.render("Y", "start");
			}
			
		},
		
		showAngle: function(){
			this.showLabel();
			
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

			var pt = drawing.custom.positioning.angle({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
			
			// adding _offX & _offY since this is HTML
			// and we are from the page corner, not
			// the canvas corner
			dojo.style(node, {
				left:  this._offX + pt.x + "px",
				top: this._offY + pt.y + "px",
				textAlign:pt.align
			});
			
			// reversing the angle for display: 0 -> 180, 90 -> 270
			angle = 180 - angle; angle = angle==360 ? 0 : angle;
			node.innerHTML = Math.ceil(angle);
			//watch("angle", angle);
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
			console.warn("DONE");
			if(this._angleNode){
				
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