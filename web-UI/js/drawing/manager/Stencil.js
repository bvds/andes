dojo.provide("drawing.manager.Stencil");

(function(){
	var surface, surfaceNode;
	drawing.manager.Stencil = drawing.util.oo.declare(
		function(options){
			surface = options.surface;
			//used for cursor:
			//surfaceNode = surface.rawNode.parentNode;
			if(options.grid){
				this.setGrid(options.grid);
			}
			this.undo = options.undo;
			this.mouse = options.mouse;
			this.keys = options.keys;
			this.anchors = options.anchors
			this.items = {};
			this.selectedItems = {};
			this._mouseHandle = this.mouse.register(this);
			dojo.connect(this.keys, "onArrow", this, "onArrow");
			dojo.connect(this.keys, "onEsc", this, "deselect");
			dojo.connect(this.keys, "onDelete", this, "onDelete");
		},
		{
			_dragBegun: false,
			_wasDragged:false,
			_secondClick:false,
			register: function(item){
				//console.log("Selection.register ::::::", item.id)
				this.items[item.id] = item;
			},
			
			onArrow: function(evt){
				if(this.isSelected()){
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
			onSelect: function(item){ 
				//console.trace();
				console.warn("SELECT:", item)
				if(this.selectedItems[item.id]){ return; }
				this.selectedItems[item.id] = item;
				this.group.add(item.parent);
				if(this.isSelected()==1){
					this.anchors.add(item, this.group);
				}
				item.select();
			},
			onDeselect: function(item, keepObject){
				
				this.anchors.remove(item);
				
				surface.add(item.parent);
				
				item.setTransform(this.group.getTransform());
				item.deselect();
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
			onStencilDown: function(obj){
				console.info("onStencilDown:", obj)
				if(this.selectedItems[obj.id] && this.keys.meta){
					
					console.log("shift remove");
					this.onDeselect(this.selectedItems[obj.id]);
					if(this.isSelected()==1){
						this.withSelected(function(m){
							this.anchors.add(m, this.group);
						});
					}
					this.group.moveToFront();
					return;
				
				}else if(this.selectedItems[obj.id]){
					
					console.log("same shape(s)");
					return;
				
				}else if(!this.keys.meta){
					
					console.log("deselect all");
					this.deselect();
				
				}else{
					console.log("reset sel and add item")
				}
				
				// add an item
				this.setSelectionGroup();
				var item = this.items[obj.id];
				this.onSelect(item);
				this.group.moveToFront();
				
				
				//  dojo.style(surfaceNode, "cursor", "pointer");
				
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
					this.onBeginDrag(obj);
					this._dragBegun = true;
				}else{
					this.saveThrottledState();
					// bug, in FF anyway - first mouse move shows x=0
					// the 'else' fixes it
					var x = obj.x - obj.last.x;
					var y = obj.y - obj.last.y;
					
					this.group.applyTransform({
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
			isSelected: function(){
				// returns number of selected
				var ln = 0;
				for(var m in this.selectedItems){ ln++; }
				return ln;
			}
		}
		
	);
})();
