Ext.define('Ext.space.invoke.Connection', {
    constructor: function(receiverId) {
        this.receiverId = receiverId;
        this.proxyMap = {};
    },

    send: function(message, foreground) {
        return Ext.space.Invoke.send(this.receiverId, message, foreground);
    },

    get: function(name){
    	var proxy = this.proxyMap[name],
    	connection = this;
    	if (!proxy) {
    		proxy = this.proxyMap[name] = Ext.space.Invoke.send(this.receiverId, {"$control": {"action": 'getProxy', 'name':  name}}, false).then(function(obj){
    			 return new Ext.space.invoke.Proxy(connection,name,obj.methods);
    		})
    	}
    	return proxy;
    },

    receive: function(message) {}
});
