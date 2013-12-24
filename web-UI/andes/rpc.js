define([
    "andes/startup",
	"dojox/rpc/Service",
	"dojox/rpc/JsonRPC",
	"dojox/json/schema"
],function(andes,rpc){

	// summary:
	//	This stub is in place to make andes.rpc a swappable oject
	//	for other potential server connections, such as CometD.
	
	return new rpc(require.toUrl("andes/andes3.smd"));
});
