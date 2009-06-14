dojo.provide("drawing.manager.Stencil");

(function(){
	var surface, surfaceNode;
	drawing.manager.Stencil = drawing.util.oo.declare(
		function(options){
			surface = options.surface;
			surfaceNode = surface.rawNode.parentNode;
			if(options.grid){
				this.setGrid(options.grid);
			}
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
					this.group.applyTransform({dx:evt.x, dy: evt.y});
				}
			},
			
			onDelete: function(){
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
				console.log("------> meta", this.keys.meta)
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
				dojo.style(surfaceNode, "cursor", "pointer");
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
					// bug, in FF anyway - first mouse move shows x=0
					// the 'else' fixes it
					var x = obj.x - obj.last.x;
					var y = obj.y - obj.last.y;
					//console.log("drag:::", obj.x, ", ", obj.start.x, "===>", x)
					watch("drag x:", x)
					watch("last x:", obj.last.x)
					
					this.group.applyTransform({
						dx: x,
						dy: y
					});

					watch("trans x:", this.group.getTransform().dx)
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
			},
			
			
			// TODO put this somewhere else
			setGrid: function(options){
				
				var x1,x2,y1,y2;
				var d = options.gap;
				var dim = surface.getDimensions();
				var s = surface.createGroup();
				var w = dim.width;
				var h = dim.height;
				var b = 1;
				var c = "#A3ECFE";
				
				
				var createGridLine = function(x1,y1,x2,y2){
					s.createLine({x1: x1, y1: y1, x2: x2, y2: y2}).setStroke({style: "Solid", width: b, cap: "round", color:c});
				}
				// horz
				for(var i=1,len = h/d; i<len; i++){
					x1 = 0, x2 = w;
					y1 = d*i, y2 = y1;
					createGridLine(x1,y1,x2,y2);
				}
				// vert
				for(var i=1,len = w/d; i<len; i++){
					y1 = 0, y2 = h;
					x1 = d*i, x2 = x1;
					createGridLine(x1,y1,x2,y2);
				}
				
				return s;
			}
		}
		
	);
})();
