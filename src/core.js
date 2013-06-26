(function(global) {
    var Ext = global.Ext;

    if (typeof Ext == 'undefined') {
        global.Ext = Ext = {};
    }

    Ext.define = function(name, members) {
        var Class = function() {
                return this.constructor && this.constructor.apply(this, arguments);
            },
            prototype = Class.prototype,
            root = global,
            parts = name.split('.'),
            ln = parts.length - 1,
            leaf = parts[ln],
            extend = members.extend,
            key, value, part, i;

        if (extend) {
            delete members.extend;
            Class.prototype = prototype = Object.create(extend.prototype);
            prototype.superclass = extend.prototype;
        }

        for (key in members) {
            if (members.hasOwnProperty(key)) {
                value = members[key];
                prototype[key] = value;
            }
        }

        if (members.singleton) {
            Class = new Class();
        }

        for (i = 0; i < ln; i++) {
            part = parts[i];
            root = root[part] || (root[part] = {});
        }

        root[leaf] = value;

        return Class;
    };

    var match = window.navigator.userAgent.match(/SenchaSpace\/([0-9\.]+)/),
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
