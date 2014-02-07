(function(global) {
    var Ext = global.Ext;

    if (typeof Ext == 'undefined') {
        global.Ext = Ext = {};
    }

    var Base = function() {};

    Base.prototype = {
        constructor: function() {}
    };

    if (!('define' in Ext)) {
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
            Class.superclass = prototype.superclass = extend.prototype;

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
    }

    var match = typeof window != 'undefined' && window.navigator.userAgent.match(/SenchaSpace\/([0-9\.]+)/),
        readyListeners = [],
        spaceReady = null; // lazy init because Ext.Promise isn't defined yet

    if (match) {
        Ext.isSpace = true;
        Ext.spaceVersion = match[1];
    }

    if (!('apply' in Ext)) {
        Ext.apply = function(object, config) {
            var key, value;

            for (key in config) {
                value = config[key];
                object[key] = value;
            }

            return object;
        };
    }

    Ext.isSpaceReady = false;
    Ext.onSpaceReady = function(callback, scope) {
        if (!spaceReady) {
            spaceReady = new Ext.Promise();
        }
        if (Ext.spaceIsWindowsPhone) {
            // windows phone might not be ready yet
            setTimeout(function() {
                if (!Ext.isSpace) {
                    spaceReady.reject("Not in Space");
                }
            }, 100);
        } else {
            if (!Ext.isSpace) {
                return spaceReady.reject("Not in Space");
            }
        }
        return callback ? spaceReady.then(callback.bind(scope)) : spaceReady;
    };
    Ext.setSpaceReady = function() {
        if (!spaceReady) {
            spaceReady = new Ext.Promise();
        }
        Ext.isSpaceReady = true;
        spaceReady.fulfill();
    };

    Ext.spaceIsIos = /(iPad|iPhone|iPod)/i.test(window.navigator.userAgent);
    Ext.spaceIsAndroid = /Android/i.test(window.navigator.userAgent);
    Ext.spaceIsBlackBerry = /(BlackBerry|BB10)/i.test(window.navigator.userAgent);
    Ext.spaceIsWindowsPhone = /Windows Phone/i.test(window.navigator.userAgent);
})(this);
