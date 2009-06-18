dojo.provide("drawing.manager.Mouse");

drawing.manager.Mouse = drawing.util.oo.declare(
	//
	// singleton? Would need to:
	// track multiple containers & drawings
	// comm back only to that drawing
	// more difficult to maintain selection
	//
	function(options){
		this.container = options.container;
		this.util = options.util;
		var pos = dojo.coords(this.container);
		this.origin.x = pos.x;
		this.origin.y = pos.y;
		
		var c;
		var _isDown = false;
		dojo.connect(this.container, "mousedown", this, function(evt){
			this.down(evt);
			_isDown = true;
			c = dojo.connect(document, "mousemove", this, "drag");
		});
		// FIXME: connect on surface hover
		dojo.connect(document, "mouseup", this, function(evt){
			dojo.disconnect(c);
			_isDown = false;
			this.up(evt);
		});
		dojo.connect(document, "mousemove", this, function(evt){
			if(!_isDown){
				this.move(evt);
			}
		});
		//dojo.connect(this.container, "mouseup", this, function(evt){
			//if(evt.target.id.indexOf("surface")>-1){
				//this.onCanvasUp(evt);
			//}
		//});
	},
	
	{
		registerd:{},
		_lastx:0,
		_lasty:0,
		__reg:0,
		_shapeClick:false,
		register: function(scope){
			var handle = "reg_"+(this.__reg++);
			this.registerd[handle] = scope;
			return handle;
		},
		unregister: function(handle){
			if(!this.registerd[handle]){ return; }
			delete this.registerd[handle];
		},
		
		_broadcastEvent:function(strEvt, obj){
			for(var nm in this.registerd){
				if(this.registerd[nm][strEvt]) this.registerd[nm][strEvt](obj);
			}
		},
		
		
		
		onCanvasUp: function(evt){
			//console.log("mouse onCanvasUp")
			this._shapeClick = false;	
		},
		
		onDown: function(obj){
			console.info(this.eventName("down"))
			this._broadcastEvent(this.eventName("down"), obj);			
		},
		onDrag: function(obj){
			//console.info(this.eventName("drag"))
			this._broadcastEvent(this.eventName("drag"), obj);	
		},
		onMove: function(obj){
			this._broadcastEvent("onMove", obj);
		},
		onUp: function(obj){
			this._broadcastEvent(this.eventName("up"), obj);
		},
		
		zoom: 1,
		setZoom: function(zoom){
			this.zoom = 1/zoom;
		},
		
		eventName: function(name){
			name = name.charAt(0).toUpperCase() + name.substring(1);
			var dt = !this.drawingType || this.drawingType=="surface" || this.drawingType=="canvas" ? "" : this.drawingType;
			var t = dt ? "" : dt.charAt(0).toUpperCase() + dt.substring(1);
			return "on"+t+name;
		},
		origin:{},
		up: function(evt){
			this.onUp(this.create(evt));
			this._shapeClick = false;
		},
		down: function(evt){
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
		
		console.log("evt:", evt);
		console.log("this.drawingType:", this.drawingType)
		
			this.onDown({x:x,y:y, id:this._getId(evt)});
			dojo.stopEvent(evt);
		},
		move: function(evt){
			//this.onMove(this.create(evt));
		},
		drag: function(evt){
			this.onDrag(this.create(evt));
		},
		create: function(evt){
			var dim = this._getXY(evt);
			var x = dim.x - this.origin.x;
			var y = dim.y - this.origin.y;
			
			x*= this.zoom;
			y*= this.zoom;
			
			
			var ret = {
				x:x,
				y:y,
				pageX:dim.x,
				pageY:dim.y,
				orgX:this.origin.x,
				orgY:this.origin.y,
				last:{
					x: this._lastx,
					y: this._lasty
				},
				start:{
					x: this.origin.startx,
					y: this.origin.starty
				},
				id:this._getId(evt)
			};
			this._lastx = x;
			this._lasty = y;
			dojo.stopEvent(evt);
			return ret;
		},
		_getId: function(evt){
				return this.util.attr(evt, "id") || this.util.attr(evt.target.parentNode, "id");
		},
		_getXY: function(evt){
			return {x:evt.pageX, y:evt.pageY};
		}
	}
);
