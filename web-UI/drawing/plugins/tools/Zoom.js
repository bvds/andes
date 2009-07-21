dojo.provide("drawing.plugins.tools.Zoom");
dojo.require("drawing.plugins._Plugin");

drawing.plugins.tools.Zoom = drawing.util.oo.declare(
	drawing.plugins._Plugin,
	function(options){
		console.warn("-------------------ZOOM")
		var cls = options.node.className;
		var txt = options.node.innerHTML;
		this.domNode = dojo.create("div", {id:"btnZoom", "class":"toolCombo"}, options.node, "replace");
		
		this.makeButton("ZoomIn", this.topClass);
		this.makeButton("Zoom100", this.midClass);
		this.makeButton("ZoomOut", this.botClass);
		
	},
	{
		zoomInc:.1,
		maxZoom:10,
		minZoom:.1,
		zoomFactor:1,
		baseClass:"drawingButton",
		topClass:"toolComboTop",
		midClass:"toolComboMid",
		botClass:"toolComboBot",
		type:"drawing.plugins.tools.Zoom",
		makeButton: function(name, cls){
			
			var node = dojo.create("div", {id:"btn"+name, "class":this.baseClass+" "+cls,
				innerHTML:'<div title="Zoom In" class="icon icon'+name+'"></div>'}, this.domNode);
			
			dojo.connect(document, "mouseup", function(evt){
				dojo.stopEvent(evt);
				dojo.removeClass(node, "active");
			});
			dojo.connect(node, "mouseup", this, function(evt){
				dojo.stopEvent(evt);
				dojo.removeClass(node, "active");
				this["on"+name]();
			});
			dojo.connect(node, "mouseover", function(evt){
				dojo.stopEvent(evt);
				dojo.addClass(node, "hover");
			});
			dojo.connect(node, "mousedown", this, function(evt){
				dojo.stopEvent(evt);
				dojo.addClass(node, "active");
			});
			
			dojo.connect(node, "mouseout", this, function(evt){
				dojo.stopEvent(evt);
				dojo.removeClass(node, "hover");
			});
		
		},
		
		onZoomIn: function(){
			this.zoomFactor += this.zoomInc;
			this.zoomFactor = Math.min(this.zoomFactor, this.maxZoom);
			this.canvas.setZoom(this.zoomFactor);
			this.mouse.setZoom(this.zoomFactor);
		},
		onZoom100: function(){
			this.zoomFactor = 1;
			this.canvas.setZoom(this.zoomFactor);
			this.mouse.setZoom(this.zoomFactor);
		},
		onZoomOut: function(){
			this.zoomFactor -= this.zoomInc;
			this.zoomFactor = Math.max(this.zoomFactor, this.minZoom);
			this.canvas.setZoom(this.zoomFactor);
			this.mouse.setZoom(this.zoomFactor);
		}
	}
);