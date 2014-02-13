Ext.define('Ext.space.invoke.Proxy', {
    constructor: function(conection,ObjectName,methods) {
    	this.connection = conection;
    	this.remoteObjectName =ObjectName;

    	for (var i = methods.length - 1; i >= 0; i--) {
    		var method = methods[i];
    		this[method] = this._makeMethod(method);
    	}

    },

    _makeMethod: function(method) {
    	var self = this;

    	return function(options, foreground){
    		return self.connection.send({"$control": {"action": 'callProxy','name' : this.remoteObjectName, 'method': method, options: options} },foreground);
    	}

    }
});
