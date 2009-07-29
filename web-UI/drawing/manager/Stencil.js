dojo.provide("drawing.manager.Stencil");

(function(){
	var surface, surfaceNode;
	drawing.manager.Stencil = drawing.util.oo.declare(
		// summary:
		//	The main class for tracking Stencils that are cretaed, added,
		//	selected, or deleted. Also handles selections, multiple
		//	selections, adding and removing from selections, and dragging
		//	selections. It's this class that triggers the anchors to
		//	appear on a Stencil and whther there are anchor on a multiple
		//	select or not (currently not)
		//
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
			this.stencils = {};
			this.selectedStencils = {};
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
			_isBusy:false,
			
			register: function(/*Object*/stencil){
				// summary:
				//	Key method for adding Stencils. Stencils
				//	can be added to the canvas without adding
				//	them to this, but they won't have selection
				//	or drag ability.
				//
			console.log("Selection.register ::::::", stencil.id, "TEXT:", stencil._text)
				if(stencil.isText && !stencil.editMode && stencil.deleteEmptyCreate && !stencil.getText()){
					// created empty text field
					// defaults say to delete
					console.warn("EMPTY CREATE DELETE", stencil)
					stencil.destroy();
					return false;
				}
				this.stencils[stencil.id] = stencil;
				if(stencil.execText){
					if(stencil._text && !stencil.editMode){
						this.selectItem(stencil);
					}
					stencil.connect("execText", this, function(){
						if(stencil.isText && stencil.deleteEmptyModify && !stencil.getText()){
							console.warn("EMPTY MOD DELETE", stencil)
							// text deleted
							// defaults say to delete
							this.deleteItem(stencil);
						}else if(stencil.selectOnExec){
							this.selectItem(stencil);
						}
					});
				}
				
				stencil.connect("deselect", this, function(){
					if(!this._isBusy && this.isSelected(stencil)){
						// called from within stencil. do action.
						this.deselectItem(stencil);
					}
				});
				
				stencil.connect("select", this, function(){
					if(!this._isBusy && !this.isSelected(stencil)){
						// called from within stencil. do action.
						this.selectItem(stencil);
					}
				});
				
				return stencil;
			},
			unregister: function(/*Object*/stencil){
				// summary:
				//	Method for removing Stencils from the manager.
				//	This doesn't delete them, only removes them from
				// the list.
				//
				console.log("Selection.unregister ::::::", stencil.id)
				if(stencil){
					stencil.selected && this.onDeselect(stencil);
					delete this.stencils[this.selectedStencils.id];
				}
			},
			
			onArrow: function(/*Key Event*/evt){
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
				// summary:
				//	Internal. Used for the prototype undo stack.
				// Saves selection position.
				//
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
				// summary:
				//	Internal. Used for the prototype undo stack.
				//	Prevents an undo point on every mouse move.
				//	Only does a point when the mouse hesitates.
				//
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
			unDelete: function(/*Array*/stencils){
				// summary:
				//	Undeletes a stencil. Used in undo stack.
				//
				console.log("unDelete:", stencils);
				for(var s in stencils){
					stencils[s].render();
					this.onSelect(stencils[s]);
				}
			},
			onDelete: function(/*Boolean*/noundo){
				// summary:
				//	Event fired on deletion of a stencil
				//
				console.log("onDelete", noundo)
				if(noundo!==true){
					this.undo.add({
						before:dojo.hitch(this, "unDelete", this.selectedStencils),
						after:dojo.hitch(this, "onDelete", true)
					});
				}
				this.withSelected(function(m){
					this.anchors.remove(m);
					m.destroy();
				});
				this.selectedStencils = {};
			},
			
			deleteItem: function(/*Object*/stencil){
				// summary:
				//	Deletes a stencil.
				//	NOTE: supports limited undo.
				//
				// manipulating the selction to fire onDelete properly
				if(this.hasSelected()){
					// there is a selection
					var sids = [];
					for(var m in this.selectedStencils){
						if(this.selectedStencils.id == stencil.id){
							if(this.hasSelected()==1){
								// the deleting stencil is the only one selected
								this.onDelete();
								return;
							}
						}else{
							sids.push(this.selectedStencils.id);
						}
					}
					// remove selection, delete, restore selection
					this.deselect();
					this.selectItem(stencil);
					this.onDelete();
					dojo.forEach(sids, function(id){
						this.selectItem(id);
					}, this);
				}else{
					// there is not a selection. select it, delete it
					this.selectItem(stencil);
					// now delete selection
					this.onDelete();
				}
			},
			
			setSelectionGroup: function(){
				// summary:
				//	Internal. Creates a new selection group
				//	used to hold selected stencils.
				//
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
					this.group.add(m.container);
					m.select();
					console.log('reselect:', m)
				});
			},
			
			setConstraint: function(){
				// summary:
				//	Internal. Gets all selected stencils' coordinates
				//	and determines how far left and up the selection
				//	can go without going below zero
				//
				var t = 0; l = 0;
				this.withSelected(function(m){
					var o = m.getBounds();
					t = Math.max(o.y1, t);
					l = Math.max(o.x1, l);
				});
				this.constrain = {l:-l, t:-t};
			},
			
			onSelect: function(/*Object*/stencil){
				// summary:
				//	Event fired on selection of a stencil
				//
				//console.log("stencil.onSelect", stencil);
				if(!stencil){
					console.error("null stencil is not selected:", this.stencils)
				}
				if(this.selectedStencils[stencil.id]){ return; }
				this.selectedStencils[stencil.id] = stencil;
				this.group.add(stencil.container);
				stencil.select();
				if(this.hasSelected()==1){
					this.anchors.add(stencil, this.group);
				}
			},
			
			onDeselect: function(stencil, keepObject){
				// summary:
				//	Event fired on deselection of a stencil
				//
				if(!keepObject){
					delete this.selectedStencils[stencil.id];
				}
				console.log('onDeselect, keep:', keepObject, "stencil:", stencil.type)
				this.anchors.remove(stencil);
				
				surface.add(stencil.container);
				stencil.deselect();
				stencil.applyTransform(this.group.getTransform());
			},
			
			deselectItem: function(/*Object*/stencil){
				// summary:
				//	Deselect passed stencil
				//
				// note: just keeping with standardized methods
				this.onDeselect(stencil);
			},
			
			deselect: function(){ // all stencils
				// summary:
				//	Deselect all stencils
				//
				this.withSelected(function(m){
					this.onDeselect(m);
				});
				this._dragBegun = false;
				this._wasDragged = false;
			},
			
			onStencilDoubleClick: function(/*dojox.__MangerMouseEvent*/obj){
				// summary:
				//	Event fired on the double-click of a stencil
				//
				console.info("mgr.onStencilDoubleClick:", obj)
				if(this.selectedStencils[obj.id]){
					if(this.selectedStencils[obj.id].edit){
						console.info("Mgr Stencil Edit -> ", this.selectedStencils[obj.id]);
						var m = this.selectedStencils[obj.id];
						// deselect must happen first to set the transform
						// then edit knows where to set the text box
						m.editMode = true;
						this.deselect();
						m.edit();
					}
				}
				
			},
			
			onAnchorUp: function(){
				// summary:
				//	Event fire on mouseup off of an anchor point
				this.setConstraint();
			},
			
			selectItem: function(/*String|Object*/ idOrItem){
				// summary:
				//	Method used to select a stencil.
				//
				var id = typeof(idOrItem)=="string" ? idOrItem : idOrItem.id;
				var stencil = this.stencils[id];
				this.setSelectionGroup();
				this.onSelect(stencil);
				this.group.moveToFront();
				this.setConstraint();
			},
			
			onStencilDown: function(/*dojox.__MangerMouseEvent*/obj){
				// summary:
				//	Event fired on mousedown on a stencil
				//
				//console.info("onStencilDown:", obj.id, this.keys.meta)
				this._isBusy = true;
				if(this.selectedStencils[obj.id] && this.keys.meta){
					
					//console.log("shift remove");
					this.onDeselect(this.selectedStencils[obj.id]);
					if(this.hasSelected()==1){
						this.withSelected(function(m){
							this.anchors.add(m, this.group);
						});
					}
					this.group.moveToFront();
					this.setConstraint();
					return;
				
				}else if(this.selectedStencils[obj.id]){
					// clicking on same selected item(s)
					// RESET OFFSETS
					var mx = this.group.getTransform();
					this._offx = obj.x - mx.dx; 
					this._offy = obj.y - mx.dy;
					return;
				
				}else if(!this.keys.meta){
					
					console.log("deselect all");
					this.deselect();
				
				}else{
					// meta-key add
					//console.log("reset sel and add stencil")
				}
				
				// add a stencil
				this.selectItem(obj.id);
				
				var mx = this.group.getTransform();
				this._offx = obj.x - mx.dx; 
				this._offy = obj.y - mx.dx;
				
				this.orgx = obj.x;
				this.orgy = obj.y;
				
				this._isBusy = false;
				
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
			
			onStencilUp: function(/*dojox.__MangerMouseEvent*/obj){
				// summary:
				//	Event fired on mouseup off of a stencil
				//
			},
			
			onStencilDrag: function(/*dojox.__MangerMouseEvent*/obj){
				// summary:
				//	Event fired on every mousemove of a stencil drag
				//	
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
			
			onDragEnd: function(/*dojox.__MangerMouseEvent*/obj){
				// summary:
				//	Event fired at the end of a stencil drag
				//
				this._dragBegun = false;
			},
			onBeginDrag: function(/*dojox.__MangerMouseEvent*/obj){
				// summary:
				//	Event fired at the beginning of a stencil drag
				//
				this._wasDragged = true;
			},
			
			onDown: function(/*dojox.__MangerMouseEvent*/obj){
				// summary:
				//	Event fired on mousedown on the canvas
				//
				this.deselect();
			},
			
			/*
			onStencilOver: function(evt, stencil){
				// summary:
				//	TODO: This is currently not supported.
				console.log("OVER", surface)
				dojo.style(surfaceNode, "cursor", "move");
			},
			
			onStencilOut: function(evt, stencil){
				// summary:
				//	TODO: This is currently not supported.
				console.log("OUT")
				dojo.style(surfaceNode, "cursor", "crosshair");
			},
			*/
			
			withSelected: function(/*Function*/func){
				// summary:
				//	Convenience function calls function on
				//	all selected stencils
				var f = dojo.hitch(this, func);
				for(var m in this.selectedStencils){
					f(this.selectedStencils[m]);
				}
			},
			
			withUnselected: function(/*Function*/func){
				// summary:
				//	Convenience function calls function on
				//	all stencils that are not selected
				var f = dojo.hitch(this, func);
				for(var m in this.stencils){
					!this.stencils[m].selected && f(this.stencils[m]);
				}
			},
			
			withStencils: function(/*Function*/func){
				// summary:
				//	Convenience function calls function on
				//	all stencils
				var f = dojo.hitch(this, func);
				for(var m in this.stencils){
					f(this.stencils[m]);
				}
			},
			
			hasSelected: function(){
				// summary:
				// returns number of selected (generally used
				//	as truthy or falsey)
				//
				// FIXME: should be areSelected?
				var ln = 0;
				for(var m in this.selectedStencils){ ln++; }
				return ln; // Number
			},
			
			isSelected: function(/*Object*/stencil){
				// summary:
				//	Returns if passed stencil is selected or not
				//	based on internal collection, not on stencil
				//	boolean
				return !!this.selectedStencils[stencil.id]; // Boolean
			}
		}
		
	);
})();
