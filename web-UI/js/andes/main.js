dojo.provide("andes.main");
dojo.require("andes.drawing");
dojo.require("andes.menu");
dojo.require("andes.help");
dojo.require("andes.api");
dojo.require("andes.error");
dojo.require("andes.variablename");

(function(){
	var devMode = true, query;
	if(devMode){
		query = {
			p:"32500",
			u:"joe1"
		}
	}else{
		query = dojo.queryToObject(window.location.search.substring(1));
	}
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
	andes.drawing.load(query);
	
	dojo.addOnLoad(function(){
		var splashNode = dojo.byId("splashOverlay"),
		    anim = dojo.fadeOut({node:dojo.byId("splashOverlay")}),
		    _h = dojo.connect(anim, "onEnd", function(){
		    	dojo.disconnect(_h);
		    	dojo.style(splashNode, "display", "none");
		    	console.log("andes.main loaded");
		    });
		anim.play();
	});

})();

