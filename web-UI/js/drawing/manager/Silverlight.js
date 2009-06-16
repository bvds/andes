dojo.provide("drawing.manager.Silverlight");

drawing.manager.Silverlight = drawing.util.oo.declare(
	function(options){
		if(dojox.gfx.renderer != "silverlight"){ return; }
		this.mouse = options.mouse;
		this.stencils = options.stencils;
		this.anchors = options.anchors;
		this.canvas = options.canvas;
	
		
		dojo.connect(this.stencils, "register", this, function(item){
			var c1;
			c1 = item.shape.connect("onmousedown", this.mouse, function(evt){
				evt.superTarget = item;
				this.down(evt)
			});
			
			var c2 = dojo.connect(item, "setTransform", this, function(){
				dojo.disconnect(c1);
			});
			
			var c3 = dojo.connect(item, "deselect", this, function(){
				c1 = item.shape.connect("onmousedown", this.mouse, function(evt){
					evt.superTarget = item;
					this.down(evt)
				});
			});
			
			var c4 = dojo.connect(item, "destroy", this, function(){
				dojo.forEach([c1,c2,c3,c4], dojo.disconnect, dojo);
			});
		});
		
		dojo.connect(this.anchors, "onAddAnchor", this, function(anchor){
			var c1 = anchor.shape.connect("onmousedown", this.mouse, function(evt){
				evt.superTarget = anchor;
				this.down(evt)
			});
			var c2 = dojo.connect(anchor, "disconnectMouse", this, function(){
				dojo.disconnect(c1);
				dojo.disconnect(c2);
			});
			
		});
		
		
		this.mouse._down = function(evt){
			var dim = this._getXY(evt);
			var x = dim.x - this.origin.x;
			var y = dim.y - this.origin.y;
			
			x*= this.zoom;
			y*= this.zoom;
			
			this.origin.startx = x;
			this.origin.starty = y;
			this._lastx = x;
			this._lasty = y;
			
			this.drawingType = this.util.attr(evt, "drawingType") || "";
			var id = this._getId(evt);
			
			console.log(" > > > id:", id, "drawingType:", this.drawingType, "evt:", evt)
		
			this.onDown({x:x,y:y, id:id});
			
			// throws errors in IE silverlight. Oddness.
			//dojo.stopEvent(evt);
		}
		
		this.mouse.down = function(evt){
			clearTimeout(this.__downInv);
			if(this.util.attr(evt, "drawingType")=="surface"){
				this.__downInv = setTimeout(dojo.hitch(this, function(){
					this._down(evt);		
				}),10);
				return;
			}
			this._down(evt);
		}
		
		this.mouse._getXY =  function(evt){
			
			if(evt.pageX){
				return {x:evt.pageX, y:evt.pageY, cancelBubble:true};
			}
			console.log("EVT", evt)
			console.log("EVT", evt.pageX)
			for(var nm in evt){
				console.log("..."+nm+"="+evt[nm]);
			}
			console.log("EVTX", evt.x)
			if(evt.x !== undefined){
				return {
					x:evt.x + this.origin.x,
					y:evt.y + this.origin.y
				};
			}else{
				return {x:evt.pageX, y:evt.pageY};
			}
		}
		
		this.mouse._getId = function(evt){
			return this.util.attr(evt, "id");
		}
		
	},
	{
		
	}
);