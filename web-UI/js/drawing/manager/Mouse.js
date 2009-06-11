dojo.provide("drawing.manager.Mouse");

drawing.manager.Mouse = drawing.util.oo.declare(
	
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
			if(!this._shapeClick){
				this.onCanvasUp(evt);
			}
		});
	},
	
	{
		registerd:{},
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
			if(strEvt!="onMove"){
				//console.log("mse----> ", strEvt);
			}
			if(this._shapeClick){
				if(strEvt=="onUp"){
					this._shapeClick = false;
				}
			}else{
				for(var nm in this.registerd){
					this.registerd[nm][strEvt](obj);
				}
			}
		},
		
		onShapeDown: function(shape){
			//console.log("mouse onShapeDown")
			this._shapeClick = true;	
		},
		onShapeUp: function(shape){
			//console.log("mouse onShapeUp")
			//this._shapeClick = false;	
		},
		
		onCanvasUp: function(evt){
			//
		},
		onDown: function(obj){
			this._broadcastEvent("onDown", obj);			
		},
		onDrag: function(obj){
			this._broadcastEvent("onDrag", obj);	
		},
		onMove: function(obj){
			this._broadcastEvent("onMove", obj);
		},
		onUp: function(obj){
			this._broadcastEvent("onUp", obj);
		},
		
		origin:{},
		up: function(evt){
			var o = this.create(evt);
			this.onUp(o);
			this.origin.lastx = o.x;
			this.origin.lasty = o.y;
			dojo.stopEvent(evt);
		},
		down: function(evt){
			dojo.fixEvent(evt, evt.target)
			var x = evt.pageX - this.origin.x;
			var y = evt.pageY - this.origin.y;
			this.origin.startx = x;
			this.origin.starty = y;
			this.origin.lastx = x;
			this.origin.lasty = y;
			this.onDown({x:x,y:y});
			dojo.stopEvent(evt);
		},
		move: function(evt){
			var o = this.create(evt);
			this.onMove(o);
			this.origin.lastx = o.x;
			this.origin.lasty = o.y;
			dojo.stopEvent(evt);
		},
		drag: function(evt){
			var o = this.create(evt);
			this.onDrag(o);
			this.origin.lastx = o.x;
			this.origin.lasty = o.y;
			dojo.stopEvent(evt);
		},
		create: function(evt){
			dojo.fixEvent(evt, evt.target);
			
			var ox = this.origin.startx;
			var oy = this.origin.starty;
			
			var x = evt.pageX - this.origin.x;
			var y = evt.pageY - this.origin.y;
			var w = x - ox;
			var h = y - oy;
			
			var last = {};
			last.x = this.origin.lastx;
			last.y = this.origin.lasty;
			
			var start = {};
			start.x = this.origin.startx;
			start.y = this.origin.starty;
			
			var amtx = x - last.x;
			var amty = y - last.y;
			
			return {
				x:x,
				y:y,
				w:w,
				h:h,
				last:last,
				start:start,
				amtx:amtx,
				amty:amty
			};
			
		},
		calc: function(evt){
			dojo.fixEvent(evt, evt.target);
			
			var ox = this.origin.startx;
			var oy = this.origin.starty;
			
			var x = evt.pageX - this.origin.x;
			var y = evt.pageY - this.origin.y;
			var w = x - ox;
			var h = y - oy;
			
			var last = {};
			last.x = this.origin.lastx;
			last.y = this.origin.lasty;
			
			var start = {};
			start.x = this.origin.startx;
			start.y = this.origin.starty;
			
			return {
				x:x,
				y:y,
				w:w,
				h:h,
				last:last,
				start:start
			};
		}
	}
);
