dojo.provide("andes.widget.ExpandoPane");

dojo.require("dojox.layout.ExpandoPane");

dojo.declare("andes.widget.ExpandoPane", dojox.layout.ExpandoPane, {
	startup: function(){
		this.inherited(arguments);
		this._openButtonNode = dojo.create("div", {id:"helpPaneOpenButton", innerHTML:"&lt;&lt;"});
		dojo.style(this._openButtonNode, "display", "none");
		dojo.place(this._openButtonNode, dojo.byId("drawingPane"), "last");
		dojo.connect(this._openButtonNode, "onclick", this, "toggle");
	},

	_setupAnims: function(){
		this._closedSize = 0;
		this.inherited(arguments);
	},

	_showEnd: function(){
		dojo.style(this._openButtonNode, "display", "none");
		this.inherited(arguments);
	},

	_hideEnd: function(){
		this.inherited(arguments);
		dojo.style(this._openButtonNode, {display:"block", opacity:0});
		dojo.fadeIn({node:this._openButtonNode}).play();
	}
});
