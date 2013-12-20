define([
	"dojox/rpc/Service",
	"dojox/rpc/JsonRPC",
	"dojox/json/schema"
],function(){

	// summary:
	//	This stub is in place to make andes.rpc a swappable oject
	//	for other potential server connections, such as CometD.
	
	andes.rpc = new dojox.rpc.Service(require.toUrl("andes", "andes3.smd"));
});
