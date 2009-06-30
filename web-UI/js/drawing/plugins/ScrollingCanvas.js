dojo.provide("drawing.plugins.ScrollingCanvas");

drawing.plugins.ScrollingCanvas = drawing.util.oo.declare(
	function(options){
		dojo.mixin(this, options);
		console.warn("drawing.plugins.ScrollingCanvas", options);
		dojo.connect(this.anchors, "onAnchorUp", this, "checkBounds");
		dojo.connect(this.canvas, "onScroll", this, function(){
			if(this._blockScroll){
				this._blockScroll = false;
				return;
			}
			_scrollTimeout && clearTimeout(_scrollTimeout);
			_scrollTimeout = setTimeout(dojo.hitch(this, "checkBounds"), 200);
		});
		this._mouseHandle = this.mouse.register(this);
		
		// This HAS to be called after setting initial objects or things get screwy.
		
		setTimeout(dojo.hitch(this, function(){
		
		}),0)
			
	},{
		util:null,
		keys:null,
		mouse:null,
		drawing:null,
		stencils:null,
		anchors:null,
		canvas:null,
		onSurfaceReady: function(){
			console.warn("READY...........................")
			this.checkBounds();	
		},
		onStencilUp: function(obj){
			// this gets called even on click-off because of the
			// issues with TextBlock deselection
			this.checkBounds();
		},
		checkBounds: function(){
			// summary:
			//	Scans all items on the canvas and checks if they are out of
			// bounds. If so, a scroll bar (in Canvas) is shown. If the position
			// is left or top, the canvas is scrolled all items are relocated
			// the distance of the scroll. Ideally, it should look as if the
			// items do not move.
			
			// logging stuff here so it can be turned on and off. This method is
			// very high maintenance.
			var log = function(){
				//console.log.apply(console, arguments);
			}
			var warn = function(){
				//console.warn.apply(console, arguments);
			}
			//console.clear();
			console.time("check bounds");
			var t=Infinity, r=-Infinity, b=-Infinity, l=Infinity,
				sx=0, sy=0, dy=0, dx=0,
				mx = this.stencils.group ? this.stencils.group.getTransform() : {dx:0, dy:0},
				sc = this.mouse.scrollOffset(),
				// scY, scX: the scrollbar creates the need for extra dimension
				scY = sc.left ? 10 : 0, 
				scX = sc.top ? 10 : 0,
				// ch, cw: the current size of the canvas
				ch = this.canvas.height,
				cw = this.canvas.width,
				// pch, pcw: the normal size of the canvas (not scrolled)
				// these could change if the container resizes.
				pch = this.canvas.parentHeight,
				pcw = this.canvas.parentWidth;
			
			
			this.stencils.withSelected(function(m){
				var o = m.getBounds();
				warn("SEL BOUNDS:", o);
				t = Math.min(o.y1 + mx.dy, t);
				r = Math.max(o.x2 + mx.dx, r);
				b = Math.max(o.y2 + mx.dy, b);
				l = Math.min(o.x1 + mx.dx, l);
			});
			
			this.stencils.withUnselected(function(m){
				var o = m.getBounds();
				warn("UN BOUNDS:", o);
				t = Math.min(o.y1, t);
				r = Math.max(o.x2, r);
				b = Math.max(o.y2, b);
				l = Math.min(o.x1, l);
			});
			
			if(t < -sc.top){
				// TOP - moved off screen, create scroll
				warn("Y - create scroll...")
				log("   ITEM Y (trans + item) :", t)
				log("   TRANS y:", mx.dy)
				log("   SCROLL:", sc.top);
				log("   ALL:", t-sc.top);	
				
				ch = this.canvas.height - t  + scY;
				sy = sc.top-t;
				dy = -t;
				log("Y - Start Scroll:", sy, "ch:", ch)
			}else if(sc.top>0){
				// TOP - was already scrolling
				warn("Y - change scroll...")
				log("Y - SHRINK: sc.t", sc.top, "t:", t, "DIF:", sc.top - t, "H:", this.canvas.height);
				if(t>sc.top){
					// TOP - moved within bounds - remove scroll
					dy = -sc.top;
					sy = 0;
					ch = pch;
					log("SHRINK NOSCROLL Y dy:", dy)
				}else{
					// TOP - the scroll amount has changed (but still scrolls)
					ch = pch + sc.top - t + scY;
					sy = sc.top - t
					dy = -t;
					log("Y SHRINK TO - sy:", sy, "dy:", dy, "ch:", ch)
				}
			}
			
			if(l < -sc.left){
				// LEFT - moved off screen, create scroll
				warn("X - checkSelectBounds...")
				log("   ITEM X (trans + item) :", l)
				log("   TRANS x:", mx.dx)
				log("   SCROLL:", sc.left);
				log("   ALL:", l-sc.left);	
				
				cw = this.canvas.width - l + scX;
				sx = sc.left-l;
				dx = -l;
				log("X Start Scroll:", sx, "cw:", cw)
			}else if(sc.left>0){
				// LEFT - was already scrolling
				
				log("X - SHRINK: sc.left", sc.left, "l:", l, "DIF:", sc.left- l, "W:", this.canvas.width);
				if(l>sc.left){
					// LEFT - moved within bounds - remove scroll
					log("X - REM SCROLL")
					dx -= sc.left;
					cw = pcw;
					sx = 0;
				}else{
					// LEFT - the scroll amount has changed (but still scrolls)
					cw =  pcw + sc.left - l + scX;
					sx = sc.left - l;
					dx = -l;
					log("X - SHRINK TO - dx", dx, "sx:", sx, "cw:", cw)
				}
			}
			
				
			if(b > pch ){ 
				warn("BOTTOM SCROLL:", "b:", b, "ch:", ch, "pcj:", pch, "top:", sc.top, "sy:", sy);
				// item off bottom
				ch = Math.max(b, pch+sy);
				warn("BOTTOM SCROLL RES:", ch);
			}else if(!sy && ch>pch){
				warn("BOTTOM REMOVE", "b:", b, "ch:", ch, "pcj:", pch, "top:", sc.top, "sy:", sy);
				// item moved from bottom
				ch = pch
			}
			
			if(r > pcw ){
				warn("RIGHT SCROLL");
				// item off right
				cw = Math.max(r, pcw + sx);
			}else if(!sx && cw>pcw){
				warn("RIGHT REMOVE");
				// item moved from right
				cw = pcw
			}
			
			
			
			this._blockScroll = true;
			
			// selected items are not transformed. The selection itself is
			// and the items are on de-select
			this.stencils.group && this.stencils.group.applyTransform({dx:dx, dy:dy});
			
			// non-selected items are transformed
			this.stencils.withUnselected(function(m){
				m.transformPoints({dx:dx, dy:dy});
			});
			
			this.canvas.setDimensions(cw, ch, sx, sy);
			
			console.timeEnd("check bounds");
		}
	}
);