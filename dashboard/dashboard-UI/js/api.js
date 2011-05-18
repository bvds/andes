dojo.provide("dashboard.rpc");
dojo.require("dojox.rpc.Service");
dojo.require("dojox.rpc.JsonRPC");
dojo.require("dojox.json.schema");

dashboard.rpc = new dojox.rpc.Service("js/dashboard.smd");
json = {"version": 1, "section": "andestutor.org"};

dojo.addOnLoad(
	function() {
		var dfd = dashboard.rpc["dashboard"](json);
		console.log("sent request")
		dfd.addCallback(
			function(result) {
    				console.log(result);
			},
			function(error) {
				console.log(error);
			}
		);
	}
);

