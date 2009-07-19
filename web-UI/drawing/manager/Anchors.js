dojo.provide("drawing.manager.Anchors");

(function(){

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
				dojo.forEach(pts, function(p, i){
					if(p.noAnchor){ return; }
					var a = new drawing.manager.Anchor({stencil:item, point:p, pointIdx:i, mouse:this.mouse, util:this.util});
					this.items[item.id]._cons = [
						dojo.connect(a, "onRenderStencil", this, "onRenderStencil"),
						dojo.connect(a, "reset", this, "onReset"),
						dojo.connect(a, "onAnchorUp", this, "onAnchorUp"),
						dojo.connect(a, "onAnchorDown", this, "onAnchorDown"),
						dojo.connect(a, "onAnchorDrag", this, "onAnchorDrag")
					];
					//if(item.anchorType=="group"){
						this.items[item.id]._cons.push(dojo.connect(a, "onTransformPoint", this, "onTransformPoint"));
					//}
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
			
			onReset: function(stencil){
				// a desperate hack in order to get the anchor point to reset.
				var st = this.util.byId("drawing").stencils;
				st.onDeselect(stencil);
				st.onSelect(stencil);
			},
			
			onRenderStencil: function(){
				for(var nm in this.items){
					dojo.forEach(this.items[nm].anchors, function(a){
						a.shape.moveToFront();
					});
				}
			},
			
			onTransformPoint: function(anchor){
				// summary:
				//		Fired on anchor drag
				//		If anchors are a "group", it's corresponding anchor
				//		is set. All anchors then moved to front.
				var anchors = this.items[anchor.stencil.id].anchors;
				var item = this.items[anchor.stencil.id].item
				var pts = [];
				dojo.forEach(anchors, function(a, i){
					
					
					if(anchor.id == a.id || anchor.stencil.anchorType!="group"){
						// nothing ?
					}else{
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
					
					var mx = a.shape.getTransform();
					pts.push({x:mx.dx + a.org.x, y:mx.dy+ a.org.y});
					
				}, this);
				item.setPoints(pts);
				item.onTransform(anchor);
				this.onRenderStencil();
			},
			
			onAnchorUp: function(anchor){
				//stub
			},
			onAnchorDown: function(anchor){
				//stub
			},
			onAnchorDrag: function(anchor){
				//stub
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
			this.defaults = drawing.defaults.copy();
			this.mouse = options.mouse;
			this.point = options.point;
			this.pointIdx = options.pointIdx;
			this.util = options.util;
			this.id = options.id || this.util.uid("anchor");
			this.org = dojo.mixin({}, this.point);
			this.stencil = options.stencil;
			if(this.stencil.anchorPositionCheck){
				this.anchorPositionCheck = dojo.hitch(this.stencil, this.stencil.anchorPositionCheck);
			}
			this.render();
			this.connectMouse();
		},
		{
			y_anchor:null,
			x_anchor:null,
			render: function(){
				
				this.shape && this.shape.removeShape();
				var d = this.defaults.anchors,
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
				this.shape = this.stencil.container.createRect(_r)
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
			
			anchorPositionCheck: function(x, y, anchor){
				// summary:
				//	To be over written by tool!
				//	Add a anchorPositionCheck method to the tool
				//	and it will automatically overwrite this stub.
				//	Should return x and y coords. Success is both
				//	being greater than zero, fail is if one or both
				//	are less than zero. 
				return {x:1, y:1};
			},
			
			onAnchorDrag: function(obj){
				if(this.selected){
					// mx is the original transform from when the anchor
					// was created. It does not change
					var mx = this.shape.getTransform();
					
					var pmx = this.shape.getParent().getParent().getTransform();
					
					var marginZero = this.defaults.anchors.marginZero;
					
					var orgx = pmx.dx + this.org.x,
						orgy = pmx.dy + this.org.y,
						x = obj.x - orgx;
						y = obj.y - orgy;
						s = this.defaults.anchors.minSize;
					
					var conL, conR, conT, conB;
					
					var chk = this.anchorPositionCheck(x, y, this);
					if(chk.x<0){
						console.warn("X<0 Shift");
						while(this.anchorPositionCheck(x, y, this).x<0){
							this.shape.getParent().getParent().applyTransform({dx:2, dy:0});
						}
					}
					if(chk.y<0){
						console.warn("Y<0 Shift");
						while(this.anchorPositionCheck(x, y, this).y<0){
							this.shape.getParent().getParent().applyTransform({dx:0, dy:2});
						}
					}
					
					if(this.y_anchor){
						// prevent y overlap of opposite anchor
						if(this.org.y > this.y_anchor.org.y){
							// bottom anchor
							
							conT = this.y_anchor.point.y + s - this.org.y;
							conB = Infinity;
							
							if(y < conT){
								// overlapping other anchor
								y = conT
							}
							
							
						}else{
							// top anchor
							
							conT = -orgy + marginZero;
							conB = this.y_anchor.point.y - s - this.org.y;
							
							if(y < conT){
								// less than zero
								y = conT;
							}else if(y > conB){
								// overlapping other anchor
								y = conB; 
							}
						}
					}else{
						// Lines - check for zero
						conT = -orgy + marginZero;
						if(y < conT){
							// less than zero
							y = conT
						}
					}
					
					
					
					
					if(this.x_anchor){
						// prevent x overlap of opposite anchor
						
						if(this.org.x>this.x_anchor.org.x){
							// right anchor
							
							conL = this.x_anchor.point.x + s - this.org.x;
							conR = Infinity;
							
							if(x < conL){
								// overlapping other anchor
								x = conL
							}							
							
						}else{
							// left anchor
							
							conL = -orgx + marginZero;
							conR = this.x_anchor.point.x - s - this.org.x;
							
							if(x < conL){
								x = conL;
							}else if(x > conR){
								// overlapping other anchor
								x = conR; 
							}
						}
					}else{
						// Lines check for zero
						conL = -orgx + marginZero;
						if(x < conL){
							x = conL;
						}
					}
					
					
					this.shape.setTransform({
						dx:x,
						dy:y
					});
					
					this.onTransformPoint(this);
				}
			},
			setPoint: function(mx){
				this.shape.applyTransform(mx);
			},
			connectMouse: function(){
				this._mouseHandle = this.mouse.register(this);
			},
			disconnectMouse: function(){
				this.mouse.unregister(this._mouseHandle);
			},
			reset: function(stencil){
				// stub
			},
			destroy: function(){
				this.disconnectMouse();
				this.shape.removeShape();
			}
		}
	);
	
})();