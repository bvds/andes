dojo.provide("drawing.custom.Vector");

drawing.custom.Vector = drawing.util.oo.declare(
	drawing.library.Arrow,
	function(options){
		this.con([
			["render", "showAngle"],
			["onModify", "showAngle"],
			["onRender", "onRendered"]
		]);
		this.angleSnap = 10;//id:this.id, 
		this.labelText = new drawing.stencil.Text({style:this.style, annotation:true, parent:this.parent, mouse:this.mouse});
		//this.showLabel();
	},
	{
		type:"drawing.custom.Vector",
		
		showLabel: function(){
			var d = this.pointsToData();
			var pt = drawing.custom.positioning.label({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
			
			watch("pt.align:", pt.align)
			
			//watch("label x", pt.x)
			//watch("label y", pt.y)
			
			//if(!this.labelText._text){
				//
				this.labelText.points = this.labelText.dataToPoints({
					x:  pt.x,
					y:  pt.y,
					width:100
				});
				this.labelText.render("My Label", pt.align);
			//}
			
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
			
			var real_angle = this.util.angle(obj);
			var al = 50//this.util.length(obj)/3;
			var aw = 20;
			
			var pt = drawing.custom.positioning.angle({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
			//var pt = drawing.custom.positioning.label({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
			
			
			watch("x1", d.x1)
			watch("y1", d.y1)
			watch("x2", d.x2)
			watch("y2", d.y2)
			watch("angle x", pt.x)
			watch("angle y", pt.y)
			
			
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
		
		onRendered: function(){
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