dojo.provide("dashboard.rpc");
dojo.require("dojox.rpc.Service");
dojo.require("dojox.rpc.JsonRPC");
dojo.require("dojox.json.schema");

dashboard.rpc = new dojox.rpc.Service(dojo.moduleUrl("dashboard", "file:///home/brian/andes/dashboard/dashboard-UI/js/dashboard.smd"));

function sendRequest(req){
	dashboard.rpc[req.method](req);
}

request = {"method": "test", 
			"params": {"param1": 1, "param2": 2},
			"jsonrpc": "2.0"
		};

document.onload = sendRequest(request);