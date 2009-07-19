dojo.provide("andes.rpc");
dojo.require("dojox.rpc.Service");
dojo.require("dojox.rpc.JsonRPC");
dojo.require("dojox.json.schema");

	// summary:
	//	This stub is in place to make andes.rpc a swappable oject
	//	for other potential server connections, such as CometD.
	
	andes.rpc = new dojox.rpc.Service(dojo.moduleUrl("andes", "andes3.smd"));
