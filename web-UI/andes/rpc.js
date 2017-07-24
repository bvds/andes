/*global define, require */ 
define([
    "dojox/rpc/Service",
    // pre-AMD included these
    "dojox/rpc/JsonRPC",
    "dojox/json/schema"
],function(rpc){

    // summary:
    //	This stub is in place to make andes.rpc a swappable oject
    //	for other potential server connections, such as CometD.

    window.andes.rpc = rpc(require.toUrl("andes/andes3.smd"));
});
