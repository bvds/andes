dojo.provide("andes.widget.ExpandoPane");

dojo.require("dojox.layout.ExpandoPane");

dojo.declare("andes.widget.ExpandoPane", dojox.layout.ExpandoPane, {
	minSize: 160,

	startup: function(){
		this.inherited(arguments);
		this.openButtonNode = dojo.create("div", {id:"helpPaneOpenButton", innerHTML:"&lt;&lt;"});
		dojo.style(this.openButtonNode, "display", "none");
		dojo.place(this.openButtonNode, dojo.byId("drawingPane"), "last");
		dojo.connect(this.openButtonNode, "onclick", this, "openHelp");

		var scoreContainerNode = dojo.create("span", {className:"helpScore", innerHTML:"Score: <span>0</span>%"}, this.titleWrapper, "last");
		this.scoreNode = dojo.query("span", scoreContainerNode)[0];
		
		dojo.connect(this, "toggle", this, function(arg){
			this._setCookie();
		});
		
		dojo.addOnLoad(this, "initLayout");
	},
	
	_setCookie: function(){
		var _isOpen = this._showing ? "open" : "closed";
		dojo.cookie("helpPane", _isOpen, { expires: 999 });
	},
	
	initLayout: function(){
		var ck = dojo.cookie("helpPane");
		if(ck && ck == "open"){
			this.toggle();
		}
		
		dojo.connect(dijit.byId("helpInput"), "onKeyUp", this, function(evt){
			if(evt.keyCode==13){
				dijit.byId("helpSubmit").onClick();
			}
		});
		
		dojo.connect(this, "resize", this, function(){
			dojo.style(dijit.byId("helpInput").domNode, "width", this._contentBox.w - 52 + "px");
		});
	},
	
	_setupAnims: function(){
		this._closedSize = 0;
		this.inherited(arguments);
	},

	_showEnd: function(){
		dojo.style(this.openButtonNode, "display", "none");
		this.inherited(arguments);
	},

	_hideEnd: function(){
		this.inherited(arguments);
		dojo.style(this.openButtonNode, {display:"block", opacity:0});
		dojo.fadeIn({node:this.openButtonNode}).play();
	},

	score: function(value){
		if(typeof value != "undefined"){
			this.scoreNode.innerHTML = value;
		}
		return this.scoreNode.innerHTML;
	},

	openHelp: function(){
		dijit.byId("helpInput").attr("value", "");
		if(dijit.byId("helpContentPane").attr("content").length == 0){
			dijit.byId("helpSubmit").onClick();
		}
		this.toggle();
	}
});
