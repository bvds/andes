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
				var pts = item.points;
				dojo.forEach(pts, function(p){
					if(p.noAnchor){ return; }
					var a = new drawing.manager.Anchor({stencil:item, point:p, mouse:this.mouse, util:this.util});
					this.items[item.id]._cons = [
						dojo.connect(a, "onRenderStencil", this, "onRenderStencil"),
						dojo.connect(a, "reset", this, "onReset")
					];
					if(item.anchorType=="group"){
						this.items[item.id]._cons.push(dojo.connect(a, "onTransformPoint", this, "onTransformPoint"));
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
			
			onReset: function(){
				for(var nm in this.items){
					dojo.forEach(this.items[nm].anchors, function(a){
						a.render();
					});
				}
			},
			
			onRenderStencil: function(){
				for(var nm in this.items){
					dojo.forEach(this.items[nm].anchors, function(a){
						a.shape.moveToFront();
					});
				}
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
						a.shape.moveToFront();
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
				dojo.forEach(this.items[item.id]._cons, dojo.disconnect, dojo);
				this.items[item.id].anchors = null;
				delete this.items[item.id];
			}
		}
	);
	
	drawing.manager.Anchor = drawing.util.oo.declare(
		function(options){
			this.style = drawing.defaults.copy();
			this.mouse = options.mouse;
			this.point = options.point;
			this.util = options.util;
			this.id = options.id || this.util.uid("anchor");
			this.org = dojo.mixin({}, this.point);
			this.stencil = options.stencil;
			this.render();
			this.connectMouse();
		},
		{
			y_anchor:null,
			x_anchor:null,
			render: function(){
				this.shape && this.shape.removeShape();
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
			onRenderStencil: function(){
				//stub
			},
			onTransformPoint: function(/* this */ anchor){
				//stub
			},
			onAnchorDown: function(obj){
				this.selected = obj.id == this.id;
			},
			onAnchorUp: function(obj){
				this.selected = false;
				this.stencil.onTransformEnd(this);
			},
			onAnchorDrag: function(obj){
				if(this.selected){
					var mx = this.shape.getTransform();
					
					var x, y, s = this.style.anchors.minSize;
					if(this.y_anchor){
						
						if(this.org.y > this.y_anchor.org.y){
							
							if(obj.y >= this.y_anchor.point.y + s){
								y = obj.y - obj.last.y;
							}else if(this.point.y > this.y_anchor.point.y + s){
								y = this.y_anchor.point.y + s - this.point.y
							}else{
								y = 0;
							}
							
						}else{
							
							if(obj.y <= this.y_anchor.point.y - s){
								y = obj.y - obj.last.y;
							}else if(this.point.y < this.y_anchor.point.y - s){
								y = this.y_anchor.point.y - s - this.point.y
							}else{
								y = 0;
							}
						}
					}else{
						y = obj.y - obj.last.y;
					}
					
					if(this.x_anchor){
						if(this.org.x>this.x_anchor.org.x){
							
							if(obj.x >= this.x_anchor.point.x+s){
								x = obj.x - obj.last.x;
							}else if(this.point.x > this.x_anchor.point.x + s){
								x = this.x_anchor.point.x + s - this.point.x
							}else{
								x = 0;
							}
							
						}else{
							
							if(obj.x <= this.x_anchor.point.x - s){
								x = obj.x - obj.last.x;
							}else if(this.point.x < this.x_anchor.point.x - s){
								x = this.x_anchor.point.x - s - this.point.x
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
					this.onTransformPoint(this);
					this.stencil.onTransform(this); /// ------------- rendering, not transforming
					this.onRenderStencil();
					//this.shape.moveToFront();
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
			reset: function(){
				// stub
			},
			destroy: function(){
				this.disconnectMouse();
				this.shape.removeShape();
			}
		}
	);
	
})();