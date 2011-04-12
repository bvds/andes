dojo.provide("dashboard.rpc");
dojo.require("dojox.rpc.Service");
dojo.require("dojox.rpc.JsonRPC");
dojo.require("dojox.json.schema");

dashboard.rpc = new dojox.rpc.Service("js/dashboard.smd");
json = {"method": "test", "params": {"param1": 1, "param2": 2}};

dojo.addOnLoad(
	function() {
		var dfd = dashboard.rpc["test"](json);
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

