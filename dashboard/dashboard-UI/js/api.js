dojo.provide("dashboard.api");
dojo.require("dojox.rpc.Service");
dojo.require("dojox.rpc.JsonRPC");
dojo.require("dojox.json.schema");

json = {"version": 1, "section": "andestutor.org"};

dashboard.rpc = new dojox.rpc.Service("dashboard.smd");

var response;

dojo.addOnLoad(
	function() {
		var dfd = dashboard.rpc["dashboard-test"]();
		console.log("sent request")
		dfd.addCallback(
			function(result) {
   				console.log(result);
				response = result;
			},
			function(error) {
				console.log(error);
			}
		);
	}
);
