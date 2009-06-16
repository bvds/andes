dojo.provide("drawing.manager.Canvas");

(function(){
	
	var _surface;
	//dojox.gfx.renderer
	
	drawing.manager.Canvas = drawing.util.oo.declare(
		function(options){
			//dojo.mixin(this, options);
			this.domNode = options.node;
			this.id = options.id || drawing.util.common.uid("surface");
			this.width = options.width || options.w;
			this.height = options.height || options.h;
			
			_surface = dojox.gfx.createSurface(this.domNode, this.width, this.height);
			_surface.whenLoaded(this, function(){
				setTimeout(dojo.hitch(this, function(){
					if(dojo.isIE){
						//_surface.rawNode.parentNode.id = this.id;
					}else if(dojox.gfx.renderer == "silverlight"){
						this.id = this.domNode.firstChild.id
					}else{
						//_surface.rawNode.id = this.id;
					}
					//console.log("SURFACE", _surface.rawNode)
					this.surface = _surface.createGroup();
					
					
					this.surface.setTransform({dx:0, dy:0,xx:1,yy:1});
					this.surface.getDimensions = dojo.hitch(_surface, "getDimensions");
					if(options.callback){
						options.callback();
					}
					/*
					
					_surface.connect("onmousedown", this, function(evt){
						console.warn("------------------------------------- click")
					});
					
					dojo.connect(dojo.byId("drawingNode"), "mousedown", this, function(evt){
						console.warn("------------------------------------- click")
					});
					*/
								
				}),0);
			});
		},
		{
			id:"",
			width:0,
			height:0,
			domNode:null,
			
			setDimensions: function(width, height){
				//TODO		
			},
			
			setZoom: function(zoom){
				this.surface.setTransform({xx:zoom, yy:zoom});
				this.setGrid(zoom);
			},
			
			setGrid: function(options){
				var d;
				if(typeof(options)=="number"){
					if(!this.grid){ return false; }
					d = this.gap * options;
					this.grid.removeShape();
				}else{
					d = this.gap = options.gap;
				}
				var x1,x2,y1,y2;
				var s = _surface.createGroup();
				var w = this.width;
				var h = this.height;
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
				s.moveToBack();
				this.grid = s;
				return s;
			}
		}
	);
	
})();