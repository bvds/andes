dojo.provide("drawing.Toolbar");

console.warn("drawing.Toolbar LOADED");

(function(){
	
	dojo.declare("drawing.Toolbar", [], {
		//
		// TODO: Toolbar works in markup only. Need programmatic.
		//
		width:0,
		height:0,
		grid:"",
		baseClass:"drawingToolbar",
		buttonClass:"drawingButton",
		constructor: function(props, node){
			
			dojo.addOnLoad(this, function(){
				this.domNode = dojo.byId(node);
				this.buildRendering();
				this.parse();
			});
		},
		
		buildRendering: function(){
			var drawingId = dojo.attr(this.domNode, "drawingId");
			console.warn("drawingId::", drawingId)
		},
		
		createTool: function(node){
			var type = dojo.attr(node, "tool");
			this.toolNodes[type] = node;
			this.drawing.registerTool(type, dojo.getObject(type));
			dojo.connect(node, "click", this, function(evt){
				dojo.stopEvent(evt);
				this.onClick(type);
			});
		},
		
		createAction: function(node){
			dojo.connect(node, "click", this.drawing, function(evt){
				var sel = dojo.attr(node, "selected");
				var action = dojo.attr(node, "action");
				if(sel=="false"){
					dojo.attr(node, "selected", "true");
					dojo.addClass(node, "selected");
					sel = true;
				}else if(sel=="true"){
					dojo.attr(node, "selected", "false");
					dojo.removeClass(node, "selected");
					sel = false;
				}
				this[action](evt, sel);
			});
		},
		
		parse: function(){
			this.domNode.className = this.baseClass;
			this.drawing = drawing.util.common.byId(dojo.attr(this.domNode, "drawingId"));
			!this.drawing && console.error("Drawing not found based on 'drawingId' in Toolbar. ");
			this.toolNodes = {};
			var _sel;
			dojo.query(">", this.domNode).forEach(function(node, i){
				node.className = this.buttonClass;
				var tool = dojo.attr(node, "tool");
				var action = dojo.attr(node, "action");
				var plugin = dojo.attr(node, "plugin");
				if(tool){
					if(i==0 || dojo.attr(node, "selected")=="true"){
						_sel = tool;
					}
					this.createTool(node);
				}else if(action){
					// are actions deprecated?
					this.createAction(node);
				}else if(plugin){
					var p = {name:plugin, options:{}},
						opt = dojo.attr(node, "options");
					if(opt){
						p.options = eval("("+opt+")");
					}
					p.options.node = node;
					
					this.drawing.addPlugin(p);
				}
			}, this);
			
			dojo.connect(this.drawing, "setTool", this, "onSetTool");	
			this.drawing.setTool(_sel);
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