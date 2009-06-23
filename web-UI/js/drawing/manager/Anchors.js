dojo.provide("drawing.manager.Anchors");

(function(){
	
	//
	// FIXME: constrain anchors to not go past each other
	//
	drawing.manager.Anchors = drawing.util.oo.declare(
		function(options){
			this.mouse = options.mouse;
			this.undo = options.undo;
			this.util = options.util;
			this.items = {};
		},
		{
			onAddAnchor: function(a){
				// stub
			},
			add: function(item){
				this.items[item.id] = {
					item:item,
					anchors:[]
				};
				if(item.anchorType=="none"){ return; }
				var pts = item.getPoints();
				dojo.forEach(pts, function(p){
					var a = new drawing.manager.Anchor({stencil:item, point:p, mouse:this.mouse, util:this.util});
					
					if(item.anchorType=="group"){
						this.items[item.id]._con = dojo.connect(a, "onTransformPoint", this, "onTransformPoint");
					}
					
					this.items[item.id].anchors.push(a);
					this.onAddAnchor(a);
				}, this);
				
				if(item.anchorType=="group"){
					dojo.forEach(this.items[item.id].anchors, function(anchor){
						dojo.forEach(this.items[item.id].anchors, function(a){
							if(anchor.id != a.id){
								if(anchor.org.y == a.org.y){
									anchor.x_anchor = a;
								}else if(anchor.org.x == a.org.x){
									anchor.y_anchor = a;
								}
							}
						},this);	
					},this);
					
				}
			},
			
			_findConstrinPoint: function(pts, p){
				
			},
			
			onTransformPoint: function(anchor){
				var anchors = this.items[anchor.stencil.id].anchors;
				dojo.forEach(anchors, function(a){
					if(anchor.id != a.id){
						var mx = anchor.shape.getTransform();
						if(anchor.org.y == a.org.y){
							a.setPoint({
								dx: 0,
								dy: anchor.shape.getTransform().dy - a.shape.getTransform().dy
							});
						}else if(anchor.org.x == a.org.x){
							a.setPoint({
								dx: anchor.shape.getTransform().dx - a.shape.getTransform().dx,
								dy: 0
							});
						}
					}
				});
			},
			remove: function(item){
				if(!this.items[item.id]){
					return;
				}
				dojo.forEach(this.items[item.id].anchors, function(a){
					a.destroy();
				});
				dojo.disconnect(this.items[item.id]._con);
				this.items[item.id].anchors = null;
				delete this.items[item.id];
			}
		}
	);
	
	drawing.manager.Anchor = drawing.util.oo.declare(
		function(options){
			this.style = drawing.defaults.copy();
			this.id = options.id || drawing.util.common.uid("anchor");
			this.mouse = options.mouse;
			this.point = options.point;
			this.util = options.util;
			this.org = dojo.mixin({}, this.point);
			this.stencil = options.stencil;
			this.render();
			this.connectMouse();
		},
		{
			y_anchor:null,
			x_anchor:null,
			minSize:10,
			lastx:0,
			lasty:0,
			size:10,
			style:{
				line:{
					color:"#666666",
					width:1,
					style:"Solid",
					cap:"round"
				},
				fill:"#FFFFFF"
			},
			render: function(){
				var d = this.style.anchors,
					b = d.width,
					s = d.size,
					p = s/2,
					line = {
						width:b,
						style:d.style,
						color:d.color,
						cap:d.cap
					};
					
		
				var _r = {
					x: this.point.x-p,
					y: this.point.y-p,
					width: s,
					height: s
				};
				this.shape = this.stencil.parent.createRect(_r)
					.setStroke(line)
					.setFill(d.fill);
				
				this.shape.setTransform({dx:0, dy:0});
				this.util.attr(this, "drawingType", "anchor");
				this.util.attr(this, "id", this.id);
			},
			onTransformPoint: function(/* this */ anchor){
				//stub
			},
			onAnchorDown: function(obj){
				this.selected = obj.id == this.id;
			},
			onAnchorUp: function(obj){
				this.selected = false;
			},
			onAnchorDrag: function(obj){
				if(this.selected){
					var mx = this.shape.getTransform();
					this.lastx = mx.dx; ///////////////////////////////// ????
					this.lasty = mx.dy;
					
					
					var x, y;
					if(this.y_anchor){
						watch(" y:", obj.y);
						watch(" y constrain:", this.y_anchor.point.y);
						watch(" point y:", this.point.y)
						
						if(this.org.y > this.y_anchor.org.y){
							
							if(obj.y >= this.y_anchor.point.y+this.minSize){
								y = obj.y - obj.last.y;
							}else if(this.point.y > this.y_anchor.point.y + this.minSize){
								y = this.y_anchor.point.y + this.minSize - this.point.y
							}else{
								y = 0;
							}
							
						}else{
							
							if(obj.y <= this.y_anchor.point.y - this.minSize){
								y = obj.y - obj.last.y;
							}else if(this.point.y < this.y_anchor.point.y - this.minSize){
								y = this.y_anchor.point.y - this.minSize - this.point.y
							}else{
								y = 0;
							}
						}
					}else{
						y = obj.y - obj.last.y;
					}
					
					if(this.x_anchor){
						if(this.org.x>this.x_anchor.org.x){
							
							if(obj.x >= this.x_anchor.point.x+this.minSize){
								x = obj.x - obj.last.x;
							}else if(this.point.x > this.x_anchor.point.x + this.minSize){
								x = this.x_anchor.point.x + this.minSize - this.point.x
							}else{
								x = 0;
							}
							
						}else{
							
							if(obj.x <= this.x_anchor.point.x - this.minSize){
								x = obj.x - obj.last.x;
							}else if(this.point.x < this.x_anchor.point.x - this.minSize){
								x = this.x_anchor.point.x - this.minSize - this.point.x
							}else{
								x = 0;
							}
						}
					}else{
						x = obj.x - obj.last.x;
					}
					
					this.shape.applyTransform({
						dx: x,
						dy: y
					});
					this.point.x += x;
					this.point.y += y;
					this.stencil.render(); /// ------------- rendering, not transforming
					this.onTransformPoint(this);
				}
			},
			setPoint: function(mx){
				this.shape.applyTransform(mx);
				this.point.x += mx.dx;
				this.point.y += mx.dy;
			},
			connectMouse: function(){
				this._mouseHandle = this.mouse.register(this);
			},
			disconnectMouse: function(){
				this.mouse.unregister(this._mouseHandle);
			},
			destroy: function(){
				this.disconnectMouse();
				this.shape.removeShape();
			}
		}
	);
	
})();