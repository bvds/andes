(function(global) {
    var Ext = global.Ext;

    if (typeof Ext == 'undefined') {
        global.Ext = Ext = {};
    }

    var Base = function() {};

    Base.prototype = {
        constructor: function() {}
    };

    Ext.define = function(name, members) {
        var Class = function() {
                return this.constructor.apply(this, arguments);
            },
            root = global,
            parts = name.split('.'),
            ln = parts.length - 1,
            leaf = parts[ln],
            statics = members.statics,
            extend = members.extend || Base,
            prototype, key, value, part, i;

        delete members.extend;
        Class.prototype = prototype = Object.create(extend.prototype);
        prototype.superclass = extend.prototype;

        delete members.statics;

        if (statics) {
            for (key in statics) {
                value = statics[key];
                Class[key] = value;
            }
        }

        for (key in members) {
            value = members[key];
            prototype[key] = value;
        }

        if (members.singleton) {
            Class = new Class();
        }

        for (i = 0; i < ln; i++) {
            part = parts[i];
            root = root[part] || (root[part] = {});
        }

        root[leaf] = Class;

        return Class;
    };

    var match = typeof window != 'undefined' && window.navigator.userAgent.match(/SenchaSpace\/([0-9\.]+)/),
        readyListeners = [];

    if (match) {
        Ext.isSpace = true;
        Ext.spaceVersion = match[1];
    }

    Ext.isSpaceReady = false;
    Ext.onSpaceReady = function(callback, scope) {
        if (!Ext.isSpace) {
            return;
        }

        if (!Ext.isSpaceReady) {
            readyListeners.push(arguments);
        }
        else {
            callback.call(scope);
        }
    };
    Ext.setSpaceReady = function() {
        var ln = readyListeners.length,
            i = 0,
            listener;

        for (; i < ln; i++) {
            listener = readyListeners[i];
            listener[0].call(listener[1]);
        }

        readyListeners.length = 0;
    }
})(this);
