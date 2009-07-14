dojo.provide("andes.main");


(function(){
	
	var devMode = true, query;
	if(!window.location.search){
		query = {
			p:"s2e",
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
	//dojo.cookie("andes", null, { expires: -1 });
	andes.closeFirst = false;
	andes.userId = query.u;
	andes.projectId = query.p;
	andes.sectionId = query.s || 1234;
	var ck = dojo.cookie("andes");
	if(ck){
		// There was already a cookie here
		if(ck.u==andes.userId && ck.p==andes.projectId){
			// we can continue the same session
			andes.sessionId = ck.sid;
		}else{
			andes.closeFirst = true;
			setCookie();
		}
	}else{
		setCookie();	
	}
	
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

dojo.require("andes.drawing");
dojo.require("andes.menu");
dojo.require("andes.help");
dojo.require("andes.api");
dojo.require("andes.error");
dojo.require("andes.variablename");

andes.drawing.load();