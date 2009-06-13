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
			//this.anchors = options.anchors;
			this.items = {};
			this.selectedItems = {};
			this._mouseHandle = this.mouse.register(this);
			dojo.connect(this.keys, "onArrow", this, "onArrow");
			dojo.connect(this.keys, "onEsc", this, "deselect");
		},
		{
			_dragBegun: false,
			_wasDragged:false,
			_secondClick:false,
			register: function(item){
				console.log("Selection.register ::::::", item.id)
				this.items[item.id] = item;
			},
			
			onArrow: function(evt){
				if(this.isSelected()){
					this.group.applyTransform({dx:evt.x, dy: evt.y});
				}
			},
			setSelectionGroup: function(){
				
				for(var n in this.selectedItems){
					this.onDeselect(this.selectedItems[n], true);
				}
				
				if(this.group){
					surface.remove(this.group);
					this.group.removeShape();
				}
				this.group = surface.createGroup();
				this.group.setTransform({dx:0, dy: 0});
				
				for(var n in this.selectedItems){
					this.group.add(this.selectedItems[n].shape);
				}
			},
			onSelect: function(item){
				if(this.selectedItems[item.id]){ return; }
				this.selectedItems[item.id] = item;
				this.group.add(item.shape);
				//this.anchors.add(item, this.group);
				
			},
			onDeselect: function(item, keepObject){
				
				//this.anchors.remove(item);
				item.setParent();
				item.transformPoints(this.group.getTransform()); // which re-renders item in new loc
				//item.shape.applyTransform(this.group.getTransform()); // which doesn't
				if(!keepObject){
					delete this.selectedItems[item.id];
				}
			},
			deselect: function(){ // all? items?
				this.withSelected(function(m){
					m.deselect();
					this.onDeselect(m);
					console.log("ITEM:", m)
				});
				this._dragBegun = false;
				this._wasDragged = false;
			},
			onShapeDown: function(obj){
				
				//
				// - check items if this is in there
				//
				if(this.selectedItems[obj.id]){
					//console.log("same shape(s)");
					return;
				}else if(!this.keys.meta){
					this.deselect();
				}
				this.setSelectionGroup();
				
				var item = this.items[obj.id];
				item.select();
				this.onSelect(item);
				dojo.style(surfaceNode, "cursor", "pointer");
			},
			onDown: function(obj){
				this.deselect();
			},
			
			onShapeUp: function(obj){

			},
			
			
			onShapeDrag: function(obj){
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
			
			onShapeOver: function(evt, item){
				console.log("OVER", surface)
				dojo.style(surfaceNode, "cursor", "move");
			},
			onShapeOut: function(evt, item){
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
				// at least one item is selected
				var ln = 0;
				for(var m in this.selectedItems){
					ln++;
				}
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
