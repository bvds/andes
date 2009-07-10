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
		// FIXME: probably want to fetch something here
		this.toggle();
	}
});
