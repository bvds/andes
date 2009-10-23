dojo.provide("andes.main");


(function(){
	// summary:
	//	Handles loading of app and the timing of how items load.
	
	var devMode = true, query;
	if(!window.location.search){
		query = {
			p:"s2e",
			//p:"s2esolved",
			u:"joe1"
		};
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
	var setCookie = function(){
		andes.sessionId = andes.userId + andes.projectId + new Date().getTime();
		 var andesCookie = {
			u:andes.userId,
			p:andes.projectId,
			sid:andes.sessionId,
			closed:false
		};
		dojo.cookie("andes", dojo.toJson(andesCookie), { expires: 999 });
	}
	
	andes.closeFirst = false;
	andes.userId = query.u;
	andes.projectId = query.p;
	andes.sectionId = query.s || 1234;
	andes.extra = query.e || "";
	//extra field for Raj
	var ck = dojo.cookie("andes");
	if(ck && ck.u){
		// There was already a cookie here
		if(ck.u==andes.userId && ck.p==andes.projectId){
			// we can continue the same session
			andes.sessionId = ck.sid;
		}else{
			andes.closeFirst = true;
			console.warn("Closing previous session", ck.u, andes.userId, ck.p, andes.projectId)
			setCookie();
		}
	}else{
		setCookie();	
	}
	
	dojo.addOnUnload(function(){
		andes.api.close({});
		// but don't clear cookie
	});
	
	dojo.addOnLoad(function(){
		dojo.connect(dojo.byId("submitButton"), "click", function(){
			andes.api.close({});
			dojo.cookie("andes", null, { expires: -1 });
			document.location.href = "login.html";
		});
	
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

dojo.require("andes.defaults");
dojo.require("andes.drawing");
dojo.require("andes.menu");
dojo.require("andes.help");
dojo.require("andes.api");
dojo.require("andes.error");
dojo.require("andes.variablename");
dojo.require("andes.convert");

andes.drawing.load();