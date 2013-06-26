Ext.define('Ext.space.invoke.Connection', {
    constructor: function(receiverId) {
        this.receiverId = receiverId;
    },

    send: function(message, foreground) {
        return Ext.space.Invoke.send(this.receiverId, message, foreground);
    },

    receive: function(message) {}
});
