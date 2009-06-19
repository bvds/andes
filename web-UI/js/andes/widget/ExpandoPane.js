dojo.provide("andes.widget.ExpandoPane");

dojo.require("dojox.layout.ExpandoPane");

dojo.declare("andes.widget.ExpandoPane", dojox.layout.ExpandoPane, {
	splitter:false,
	postCreate: function(){
		this.inherited(arguments);
		dojo.style(this.domNode, "overflow", "visible");
	}
});
