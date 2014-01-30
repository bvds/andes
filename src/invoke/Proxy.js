Ext.define('Ext.space.invoke.Proxy', {
    constructor: function(conection,ObjectName,methods) {
    	this.connection = conection;
    	this.remoteObjectName =ObjectName;
    	var self = this;

    	for (var i = methods.length - 1; i >= 0; i--) {
    		var method = methods[i];
    		this[method] = function(options, foreground){
    			return self.connection.send({"$control": {"type": 'callProxy','name' : this.remoteObjectName, options: options} },foreground);
    		}
    	}

    }
});
