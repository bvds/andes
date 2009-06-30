dojo.provide("andes.main");
dojo.require("andes.menu");
dojo.require("andes.help");
dojo.require("andes.api");
dojo.require("andes.error");

(function(){

	var query = dojo.queryToObject(window.location.search.substring(1));
	if(!query.u || !query.p){
		console.error("FIXME: do a dialog and send the user back to the WebAssign page");
	}

	andes.userId = query.u;
	andes.projectId = query.p;

})();

console.log("andes.main loaded");
