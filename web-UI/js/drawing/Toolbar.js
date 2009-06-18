dojo.provide("drawing.Toolbar");

(function(){
	
	dojo.declare("drawing.Toolbar", [], {
		
		width:0,
		height:0,
		grid:"",
		baseClass:"drawingToolbar",
		buttonClass:"drawingButton",
		constructor: function(props, node){
			
			dojo.addOnLoad(this, function(){
				this.domNode = dojo.byId(node);
				this.buildRendering();
				this.postCreate();
			});
		},
		buildRendering: function(){
			var drawingId = dojo.attr(this.domNode, "drawingId");
			console.warn("drawingId::", drawingId)
		},
		postCreate: function(){
			this.domNode.className = this.baseClass;
			this.drawing = drawing.util.common.byId(dojo.attr(this.domNode, "drawingId"));
			this.toolNodes = {};
			var _sel;
			dojo.query("[tool]", this.domNode).forEach(function(node, i){
				node.className = this.buttonClass;
				var type = dojo.attr(node, "tool");
				if(i==0 || dojo.attr(node, "selected")=="true"){
					_sel = type;
				}
				console.log("TYPE:", type, dojo.attr(node, "selected"))
				this.toolNodes[type] = node;
				this.drawing.registerTool(type, dojo.getObject(type));
				dojo.connect(node, "click", this, function(evt){
					dojo.stopEvent(evt);
					this.onClick(type);
				});
			}, this);
			
			dojo.connect(this.drawing, "setTool", this, "onSetTool");	
			this.drawing.setTool(_sel);
			
			
			dojo.query("[action]", this.domNode).forEach(function(node, i){
				node.className = this.buttonClass;
				var action = dojo.attr(node, "action");
				
				dojo.connect(node, "click", this.drawing, action);
			}, this);
		},
		onClick: function(type){
			console.log("click:", type);
			this.drawing.setTool(type);
		},
		onSetTool: function(type){
			for(var n in this.toolNodes){
				if(n == type){
					dojo.addClass(this.toolNodes[type], "selected")	
					this.toolNodes[type].blur();
				}else{
					dojo.removeClass(this.toolNodes[n], "selected")
				}
				
			}
		}
	});
	
})();