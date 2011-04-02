dojo.provide("dashboard.api");
dojo.provide("dashboard.rpc");
dojo.require("dojox.rpc.Service");
dojo.require("dojox.rpc.JsonRPC");
dojo.require("dojox.json.schema");

dashboard.rpc = new dojox.rpc.Service(dojo.moduleUrl("dashboard", "dashboard.smd"));

dashboard.api = {
	test: function(params){
		//console.info("dashboard.api.test", params);
		var dfd = queueRequest("test", params);
		
		dfd.addCallback(function(result){
			// look for help embedded in the returned result, so we can
			// queue it up in case the user opens the Tutor pane
			andes.help.processStep(result);
		});
		return dfd;
	}
}

function sendRequest(req){
	requestInFlight = true;
	dashboard.rpc[req.method](request).addCallbacks(
	function(result){
		requestInFlight = false;
		req.dfd.callback(result);
	};
}
