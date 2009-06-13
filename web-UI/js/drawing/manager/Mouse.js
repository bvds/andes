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
		dojo.connect(this.container, "mouseup", this, function(evt){
			if(evt.target.id.indexOf("surface")>-1){
				this.onCanvasUp(evt);
			}
		});
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
			this._broadcastEvent(this._shapeClick ? "onShapeDown" : "onDown", obj);			
		},
		onDrag: function(obj){
			this._broadcastEvent(this._shapeClick ? "onShapeDrag" : "onDrag", obj);	
		},
		onMove: function(obj){
			this._broadcastEvent("onMove", obj);
		},
		onUp: function(obj){
			this._broadcastEvent(this._shapeClick ? "onShapeUp" : "onUp", obj);
		},
		
		origin:{},
		up: function(evt){
			var o = this.create(evt);
			o.id = evt.target.id;
			this.onUp(o);
			this._shapeClick = false;
		},
		down: function(evt){
			var dim = this._getXY(evt);
			var x = dim.x - this.origin.x;
			var y = dim.y - this.origin.y;
			
			var t = dojo.attr(evt.target, "drawingType")
			//console.log("MSEDWN EVT: ", evt.target.tagName, " ",t, " ",evt.target.id, " pagex:", evt.pageX)
			
			
			this.origin.startx = x;
			this.origin.starty = y;
			this._lastx = x;
			this._lasty = y;
			
			var t = dojo.attr(evt.target, "drawingType");
			if(t=="stencil"){
				this._shapeClick = true;
			}
			
			
			this.onDown({x:x,y:y, id:evt.target.id});
			dojo.stopEvent(evt);
		},
		move: function(evt){
			this.onMove(this.create(evt));
		},
		drag: function(evt){
			this.onDrag(this.create(evt));
		},
		create: function(evt){
			var dim = this._getXY(evt);
			var x = dim.x - this.origin.x;
			var y = dim.y - this.origin.y;
			watch("mse x:", x)
			watch("mse last x:", this._lastx)
			var ret = {
				x:x,
				y:y,
				last:{
					x: this._lastx,
					y: this._lasty
				},
				start:{
					x: this.origin.startx,
					y: this.origin.starty
				}
			};
			this._lastx = x;
			this._lasty = y;
			dojo.stopEvent(evt);
			return ret;
		},
		
		_getXY: function(evt){
			if(dojo.isIE){
				return {x:evt.pageX, y:evt.pageY};
				return {x:evt.screenX, y:evt.screenY};
			}else{
				return {x:evt.pageX, y:evt.pageY};
			}
		}
	}
);
