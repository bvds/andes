define([
    "andes/drawing",
    "andes/startup",
    "dojo/on",
    "dijit/registry",
    "dojo/ready"
],function(drawing,andes,on,registry,ready){
    // New to AMD version:  this was pulled out of drawing.js

    ready(function(){
	        _drawing = registry.byId("drawing");
	        console.log("got drawing widget:  ",_drawing);
		var cn = on(_drawing, "onSurfaceReady", function(){
		        cn.remove();
			andes.WordTip.add(_drawing);
			drawing.onSurfaceReady();
			if(_drawing.stencils){
				console.warn("Label double click connected");
				on(_drawing.stencils, "onLabelDoubleClick", drawing, "onLabelDoubleClick");
			}
		});
		on(_drawing, "onRenderStencil", drawing, "onRenderStencil");
		
		// Track user's focus on Andes.  So far only whether they are using the window/tab
		// or have left to use another program
		if(dojo.isIE){
			on(dojo.global, "onfocus", drawing, "onWindowFocus");
			// on(dojo.global, "onfocusin", drawing, "onWindowFocus");
			on(dojo.doc, "onfocusout", this, function() {
				if (this._activeElement != document.activeElement){
					this._activeElement = document.activeElement;
				}else{
					drawing.onWindowBlur();
				}
			});
		}else if(dojo.isSafari){
			on(window, "onblur", drawing, "onWindowBlur");
			on(window, "onfocus", drawing, "onWindowFocus");
		}else{
			on(dojo.doc, "onblur", drawing, "onWindowBlur");
			on(dojo.doc, "onfocus", drawing, "onWindowFocus");
		}
    });
});
