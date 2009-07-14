dojo.provide("drawing.manager.Stencil");

(function(){
	var surface, surfaceNode;
	drawing.manager.Stencil = drawing.util.oo.declare(
		function(options){
			//
			// TODO: mixin props
			//
			surface = options.surface;
			this.canvas = options.canvas;
			
			this.defaults = drawing.defaults.copy();
			this.undo = options.undo;
			this.mouse = options.mouse;
			this.keys = options.keys;
			this.anchors = options.anchors;
			this.items = {};
			this.selectedItems = {};
			this._mouseHandle = this.mouse.register(this);
			var _scrollTimeout;
			dojo.connect(this.keys, "onArrow", this, "onArrow");
			dojo.connect(this.keys, "onEsc", this, "deselect");
			dojo.connect(this.keys, "onDelete", this, "onDelete");
			
		},
		{
			_dragBegun: false,
			_wasDragged:false,
			_secondClick:false,
			
			
			register: function(item){
				console.log("Selection.register ::::::", item.id, "TEXT:", item._text)
				this.items[item.id] = item;
				if(item.execText){
					if(item._text){
						this.selectItem(item);
					}
					item.connect("execText", this, function(){
						this.selectItem(item);
					});
				}
				return item;
			},
			unregister: function(item){
				console.log("Selection.unregister ::::::", item.id)
				if(item){
					item.selected && this.onDeselect(item);
					delete this.items[item.id];
				}
			},
			
			onArrow: function(evt){
				// summary:
				// 	Moves selection based on keyboard arrow keys
				//
				// FIXME: Check constraints
				if(this.hasSelected()){
					this.saveThrottledState();
					this.group.applyTransform({dx:evt.x, dy: evt.y});
				}
			},
			
			
			_throttleVrl:null,
			_throttle: false,
			throttleTime:400,
			_lastmxx:-1,
			_lastmxy:-1,
			saveMoveState: function(){
				var mx = this.group.getTransform();
				if(mx.dx == this._lastmxx && mx.dy == this._lastmxy){ return; }
				this._lastmxx = mx.dx;
				this._lastmxy = mx.dy;
				//console.warn("SAVE MOVE!", mx.dx, mx.dy);
				this.undo.add({
					before:dojo.hitch(this.group, "setTransform", mx)
				});
			},
			
			saveThrottledState: function(){
				clearTimeout(this._throttleVrl);
				clearInterval(this._throttleVrl);
				this._throttleVrl = setTimeout(dojo.hitch(this, function(){
					this._throttle = false;
					this.saveMoveState();
				}), this.throttleTime)
				if(this._throttle){ return; }
				this._throttle = true;
				
				this.saveMoveState();					
				
			},
			unDelete: function(items){
				console.log("unDelete:", items);
				for(var s in items){
					items[s].render();
					this.onSelect(items[s]);
				}
			},
			onDelete: function(noundo){
				console.log("onDelete", noundo)
				if(noundo!==true){
					this.undo.add({
						before:dojo.hitch(this, "unDelete", this.selectedItems),
						after:dojo.hitch(this, "onDelete", true)
					});
				}
				this.withSelected(function(m){
					this.anchors.remove(m);
					m.destroy();
				});
				this.selectedItems = {};
			},
			
			setSelectionGroup: function(){
				
				this.withSelected(function(m){
					this.onDeselect(m, true);
				});
				
				if(this.group){
					surface.remove(this.group);
					this.group.removeShape();
				}
				this.group = surface.createGroup();
				this.group.setTransform({dx:0, dy: 0});
				
				this.withSelected(function(m){
					this.group.add(m.parent);
					m.select();
				});
				
			},
			setConstraint: function(){
				var t = 0; l = 0;
				this.withSelected(function(m){
					var o = m.getBounds();
					t = Math.max(o.y1, t);
					l = Math.max(o.x1, l);
				});
				this.constrain = {l:-l, t:-t};
			},
			onSelect: function(item){
				//console.log("stencil.onSelect", item);
				if(!item){
					console.error("null item is not selected:", this.items)
				}
				if(this.selectedItems[item.id]){ return; }
				this.selectedItems[item.id] = item;
				this.group.add(item.parent);
				item.select();
				if(this.hasSelected()==1){
					this.anchors.add(item, this.group);
				}
				
			},
			onDeselect: function(item, keepObject){
				
				this.anchors.remove(item);
				
				surface.add(item.parent);
				item.deselect();
				item.setTransform(this.group.getTransform());
				
				if(!keepObject){
					delete this.selectedItems[item.id];
				}
			},
			
			deselect: function(){ // all? items?
				this.withSelected(function(m){
					this.onDeselect(m);
				});
				this._dragBegun = false;
				this._wasDragged = false;
			},
			
			onStencilDoubleClick: function(obj){
				console.info("mgr.onStencilDoubleClick:", obj)
				if(this.selectedItems[obj.id]){
					console.warn("EDIT:", this.selectedItems[obj.id]);
					if(this.selectedItems[obj.id].edit){
						var m = this.selectedItems[obj.id];
						this.deselect();
						m.edit();
					}
				}
				
			},
			
			
			onAnchorUp: function(){
				this.setConstraint();
			},
			selectItem: function(/*String|Object*/ idOrItem){
				var id = typeof(idOrItem)=="string" ? idOrItem : idOrItem.id;
				var item = this.items[id];
				this.setSelectionGroup();
				this.onSelect(item);
				this.group.moveToFront();
				this.setConstraint();
			},
			
			onStencilDown: function(obj){
				console.info("onStencilDown:", obj.id, this.keys.meta)
				if(this.selectedItems[obj.id] && this.keys.meta){
					
					console.log("shift remove");
					this.onDeselect(this.selectedItems[obj.id]);
					if(this.hasSelected()==1){
						this.withSelected(function(m){
							this.anchors.add(m, this.group);
						});
					}
					this.group.moveToFront();
					this.setConstraint();
					return;
				
				}else if(this.selectedItems[obj.id]){
					// RESET OFFSETS
					var mx = this.group.getTransform();
					this._offx = obj.x - mx.dx; 
					this._offy = obj.y - mx.dy;
					return;
				
				}else if(!this.keys.meta){
					
					console.log("deselect all");
					this.deselect();
				
				}else{
					console.log("reset sel and add item")
				}
				
				// add an item
				this.selectItem(obj.id);
				
				var mx = this.group.getTransform();
				this._offx = obj.x - mx.dx; 
				this._offy = obj.y - mx.dx;
				
				this.orgx = obj.x;
				this.orgy = obj.y;
				
				// TODO:
				//  dojo.style(surfaceNode, "cursor", "pointer");
				
				// TODO:
				this.undo.add({
					before:function(){
						
					},
					after: function(){
						
					}
				});
			},
			
			onDown: function(obj){
				this.deselect();
			},
			
			onStencilUp: function(obj){
				
			},
			
			onStencilDrag: function(obj){
				if(!this._dragBegun){
					// bug, in FF anyway - first mouse move shows x=0
					// the 'else' fixes it
					this.onBeginDrag(obj);
					this._dragBegun = true;
				}else{
					this.saveThrottledState();
					
					var x = obj.x - obj.last.x,
						y = obj.y - obj.last.y,
						mx = this.group.getTransform(),
						c = this.constrain,
						mz = this.defaults.anchors.marginZero;
					
					
					x = obj.x - this._offx;
					y = obj.y - this._offy;
					
					if(x < c.l + mz){
						x = c.l + mz;
					}
					if(y < c.t + mz){
						y = c.t + mz;
					}
					
					this.group.setTransform({
						dx: x,
						dy: y
					});
					
					
				}
			},
			
			onDragEnd: function(obj){
				this._dragBegun = false;
			},
			onBeginDrag: function(obj){
				this._wasDragged = true;
			},
			
			onStencilOver: function(evt, item){
				console.log("OVER", surface)
				dojo.style(surfaceNode, "cursor", "move");
			},
			onStencilOut: function(evt, item){
				console.log("OUT")
				dojo.style(surfaceNode, "cursor", "crosshair");
			},
			
			withSelected: function(func){
				// convenience function
				var f = dojo.hitch(this, func);
				for(var m in this.selectedItems){
					f(this.selectedItems[m]);
				}
			},
			withUnselected: function(func){
				// convenience function
				var f = dojo.hitch(this, func);
				for(var m in this.items){
					!this.items[m].selected && f(this.items[m]);
				}
			},
			withItems: function(func){
				// convenience function
				var f = dojo.hitch(this, func);
				for(var m in this.items){
					f(this.items[m]);
				}
			},
			hasSelected: function(){
				// returns number of selected - should be areSelected?
				var ln = 0;
				for(var m in this.selectedItems){ ln++; }
				return ln;
			}
		}
		
	);
})();
