dojo.provide("drawing.plugins.Zoom");
dojo.require("drawing.plugins._Plugin");

drawing.plugins.Zoom = drawing.util.oo.declare(
	drawing.plugins._Plugin,
	function(options, node){
		console.dir(options);
		var cls = options.node.className;
		var txt = options.node.innerHTML;
		this.domNode = dojo.create("span", {id:"btnZoom"}, options.node, "replace");
		this.btnZoomIn = dojo.create("button", {id:"btnZoomIn", "class":cls, innerHTML:"Zoom In"}, this.domNode);
		this.btnZoom100 = dojo.create("button", {id:"btnZoom100", "class":cls, innerHTML:"100%"}, this.domNode);
		this.btnZoomOut = dojo.create("button", {id:"btnZoomOut", "class":cls, innerHTML:"Zoom Out"}, this.domNode);
		dojo.connect(this.btnZoomIn, "click", this, "zoomIn");
		dojo.connect(this.btnZoom100, "click", this, "zoom100");
		dojo.connect(this.btnZoomOut, "click", this, "zoomOut");
	},
	{
		zoomInc:.1,
		maxZoom:10,
		minZoom:.1,
		zoomFactor:1,
		zoomIn: function(evt){
			console.warn("ZOOM")
			dojo.stopEvent(evt);
			this.zoomFactor += this.zoomInc;
			this.canvas.setZoom(this.zoomFactor);
			this.mouse.setZoom(this.zoomFactor);
			watch("zoom:", this.zoomFactor);
		},
		zoom100: function(evt){
			dojo.stopEvent(evt);
			this.zoomFactor = 1;
			this.canvas.setZoom(this.zoomFactor);
			this.mouse.setZoom(this.zoomFactor);
			watch("zoom:", this.zoomFactor);
		},
		zoomOut: function(evt){
			dojo.stopEvent(evt);
			this.zoomFactor -= this.zoomInc;
			this.canvas.setZoom(this.zoomFactor);
			this.mouse.setZoom(this.zoomFactor);
			watch("zoom:", this.zoomFactor);
		}
	}
);