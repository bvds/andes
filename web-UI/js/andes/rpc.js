dojo.provide("andes.rpc");
dojo.provide("dojox.rpc.Client");
dojo.require("dojox.rpc.Service");
dojo.require("dojox.rpc.JsonRPC");
dojo.require("dojox.json.schema");

andes.rpc = new dojox.rpc.Service(dojo.moduleUrl("andes", "andes3.smd"));

console.warn("SMD:", andes.rpc);
