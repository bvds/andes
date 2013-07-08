/**
* @private
*/
Ext.define('Ext.space.Observable', {
    constructor: function() {
        this.listeners = [];
    },

    isWatching: false,

    startWatching: function() {},

    invokeListeners: function() {
        var listeners = this.listeners,
            ln = listeners.length,
            i = 0,
            listener;

        for (; i < ln; i++) {
            listener = listeners[i];
            listener[0].apply(listener[1], arguments);
        }
    },

    addListener: function(callback, scope) {
        if (!this.isWatching) {
            this.isWatching = true;
            this.startWatching();
        }

        this.listeners.push(arguments);
    }
});
