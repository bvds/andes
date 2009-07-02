dojo.provide("andes.main");
dojo.require("andes.menu");
dojo.require("andes.help");
dojo.require("andes.api");
dojo.require("andes.error");

(function(){

	var query = dojo.queryToObject(window.location.search.substring(1));
	if(!query.u || !query.p){
		dojo.addOnLoad(function(){
			console.error("FIXME: Finalize the error message for needing to return to WebAssign.");
			andes.error({
				title: "Fatal Error",
				message: "No user and/or problem data was provided; cannot continue. Please return to the WebAssign page.",
				dialogType: andes.error.FATAL
			});
		});
	}

	andes.userId = query.u;
	andes.projectId = query.p;

})();

console.log("andes.main loaded");
