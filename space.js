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

/**
 * @private
 *
 * This object handles communication between the WebView and Sencha's native shell.
 * Currently it has two primary responsibilities:
 *
 * 1. Maintaining unique string ids for callback functions, together with their scope objects
 * 2. Serializing given object data into HTTP GET request parameters
 *
 * As an example, to capture a photo from the device's camera, we use `Ext.device.Camera.capture()` like:
 *
 *     Ext.device.Camera.capture(
 *         function(dataUri){
 *             // Do something with the base64-encoded `dataUri` string
 *         },
 *         function(errorMessage) {
 *
 *         },
 *         callbackScope,
 *         {
 *             quality: 75,
 *             width: 500,
 *             height: 500
 *         }
 *     );
 *
 * Internally, `Ext.device.Communicator.send()` will then be invoked with the following argument:
 *
 *     Ext.device.Communicator.send({
 *         command: 'Camera#capture',
 *         callbacks: {
 *             onSuccess: function() {
 *                 // ...
 *             },
 *             onError: function() {
 *                 // ...
 *             }
 *         },
 *         scope: callbackScope,
 *         quality: 75,
 *         width: 500,
 *         height: 500
 *     });
 *
 * Which will then be transformed into a HTTP GET request, sent to native shell's local
 * HTTP server with the following parameters:
 *
 *     ?quality=75&width=500&height=500&command=Camera%23capture&onSuccess=3&onError=5
 *
 * Notice that `onSuccess` and `onError` have been converted into string ids (`3` and `5`
 * respectively) and maintained by `Ext.device.Communicator`.
 *
 * Whenever the requested operation finishes, `Ext.device.Communicator.invoke()` simply needs
 * to be executed from the native shell with the corresponding ids given before. For example:
 *
 *     Ext.device.Communicator.invoke('3', ['DATA_URI_OF_THE_CAPTURED_IMAGE_HERE']);
 *
 * will invoke the original `onSuccess` callback under the given scope. (`callbackScope`), with
 * the first argument of 'DATA_URI_OF_THE_CAPTURED_IMAGE_HERE'
 *
 * Note that `Ext.device.Communicator` maintains the uniqueness of each function callback and
 * its scope object. If subsequent calls to `Ext.device.Communicator.send()` have the same
 * callback references, the same old ids will simply be reused, which guarantee the best possible
 * performance for a large amount of repetitive calls.
 */
Ext.define('Ext.space.Communicator', {
    singleton: true,

    SERVER_URL: 'http://127.0.0.1:3000', // Change this to the correct server URL

    callbackDataMap: {},

    callbackIdMap: {},

    idSeed: 0,

    globalScopeId: '0',

    init: function(appId) {
        this.appId = appId;
        Ext.setSpaceReady();
    },

    generateId: function() {
        return String(++this.idSeed);
    },

    getId: function(object) {
        var id = object.$callbackId;

        if (!id) {
            object.$callbackId = id = this.generateId();
        }

        return id;
    },

    getCallbackId: function(callback, scope) {
        var idMap = this.callbackIdMap,
            dataMap = this.callbackDataMap,
            id, scopeId, callbackId, data;

        if (!scope) {
            scopeId = this.globalScopeId;
        } else if (scope.isIdentifiable) {
            scopeId = scope.getId();
        } else {
            scopeId = this.getId(scope);
        }

        callbackId = this.getId(callback);

        if (!idMap[scopeId]) {
            idMap[scopeId] = {};
        }

        if (!idMap[scopeId][callbackId]) {
            id = this.generateId();
            data = {
                callback: callback,
                scope: scope
            };

            idMap[scopeId][callbackId] = id;
            dataMap[id] = data;
        }

        return idMap[scopeId][callbackId];
    },

    getCallbackData: function(id) {
        return this.callbackDataMap[id];
    },

    invoke: function(id, args) {
        var data = this.getCallbackData(id);

        data.callback.apply(data.scope, args);
    },

    send: function(args, synchronous) {
        var callbacks, scope, name, callback, response;

        if (!args) {
            args = {};
        }
        else if (args.callbacks) {
            callbacks = args.callbacks;
            scope = args.scope;

            delete args.callbacks;
            delete args.scope;

            for (name in callbacks) {
                if (callbacks.hasOwnProperty(name)) {
                    callback = callbacks[name];

                    if (typeof callback == 'function') {
                        args[name] = this.getCallbackId(callback, scope);
                    }
                }
            }
        }

        var xhr = new XMLHttpRequest();

        xhr.open('POST', this.SERVER_URL + '?_dc=' + new Date().getTime(), !synchronous);
        xhr.setRequestHeader('Content-Type', 'text/plain');

        if (synchronous) {
            xhr.onreadystatechange = function() {
                if (xhr.readyState == 4) {
                    var status = xhr.status;

                    if (status < 200 || (status >= 300 && status != 304)) {
                        throw new Error("Failed communicating to native bridge, status code: " + status);
                    }
                }
            }
        }

        xhr.send(JSON.stringify({
            args: args,
            appId: this.appId
        }));

        if (synchronous) {
            response = xhr.responseText;
            return response && JSON.parse(response);
        }
    }
});

Ext.define('Ext.Promise', {
    statics: {
        when: function() {
            var ret = new this,
                promises = Array.prototype.slice.call(arguments),
                index = -1,
                results = [],
                promise;

            function onRejected(e) {
                ret.reject(e);
            }

            /**
             * @param [result]
             */
            function onFulfilled(result) {
                promise = promises.shift();

                if (index >= 0) {
                    results[index] = result;
                }

                index++;

                if (promise) {
                    promise.then(onFulfilled, onRejected);
                }
                else {
                    ret.fulfill.apply(ret, results);
                }
            }

            onFulfilled();

            return ret;
        },

        whenComplete: function(promises) {
            var ret = new this,
                index = -1,
                fulfilledResults = [],
                rejectedReasons = [],
                promise;

            function onRejected(reason) {
                promise = promises.shift();
                rejectedReasons.push(reason);
                next(promise);
            }

            /**
             * @param [result]
             */
            function onFulfilled(result) {
                promise = promises.shift();
                fulfilledResults.push(result);
                next(promise);
            }

            function next(promise) {
                index++;

                if (promise) {
                    promise.then(onFulfilled, onRejected);
                }
                else {
                    ret.fulfill.call(ret, {
                        fulfilled: fulfilledResults,
                        rejected: rejectedReasons
                    });
                }
            }

            next(promises.shift());

            return ret;
        },

        from: function() {
            var promise = new this;
            promise.completed = 1;
            promise.lastResults = arguments;
            return promise;
        },

        fail: function(reason) {
            var promise = new this;
            promise.completed = -1;
            promise.lastReason = reason;
            return promise;
        }
    },

    completed: 0,

    getListeners: function(init) {
        var listeners = this.listeners;

        if (!listeners && init) {
            this.listeners = listeners = [];
        }

        return listeners;
    },

    then: function(scope, success, error) {
        if (typeof scope == 'function') {
            error = success;
            success = scope;
            scope = null;
        }

        if (typeof success == 'string') {
            success = scope[success];
        }

        if (typeof error == 'string') {
            error = scope[error];
        }

        return this.doThen(scope, success, error);
    },

    doThen: function(scope, success, error) {
        var Promise = Ext.Promise,
            completed = this.completed,
            promise, result;

        if (completed === -1) {
            if (error) {
                error.call(scope, this.lastReason);
            }
            return this;
        }

        if (completed === 1 && !this.isFulfilling) {
            if (!success) {
                return this;
            }

            result = success.apply(scope, this.lastResults);

            if (result instanceof Promise) {
                promise = result;
            }
            else {
                promise = Promise.from(result);
            }
        }
        else {
            promise = new Promise;
            promise.$owner = this;

            this.getListeners(true).push({
                scope: scope,
                success: success,
                error: error,
                promise: promise
            });
        }

        return promise;
    },

    error: function(scope, error) {
        if (typeof scope == 'function') {
            error = scope;
            scope = null;
        }

        if (typeof error == 'string') {
            error = scope[error];
        }

        return this.doThen(scope, null, error);
    },

    fulfill: function() {
        var results = arguments,
            listeners, listener, scope, success, promise, callbackResults;

        this.lastResults = results;
        this.completed = 1;

        while (listeners = this.getListeners()) {
            delete this.listeners;
            this.isFulfilling = true;

            while (listener = listeners.shift()) {
                success = listener.success;

                if (success) {
                    scope = listener.scope;
                    promise = listener.promise;
                    delete promise.$owner;

                    callbackResults = success.apply(scope, results);

                    if (callbackResults instanceof Ext.Promise) {
                        callbackResults.connect(promise);
                    }
                    else {
                        promise.fulfill(callbackResults);
                    }
                }
            }

            this.isFulfilling = false;
        }

        return this;
    },

    connect: function(promise) {
        var me = this;

        me.then(promise, function(result) {
            this.fulfill(result);
            return result;
        }, 'reject');
    },

    reject: function(reason) {
        var listeners = this.getListeners(),
            listener, error, promise;

        this.lastReason = reason;
        this.completed = -1;

        if (listeners) {
            delete this.listeners;
            while (listener = listeners.shift()) {
                error = listener.error;
                promise = listener.promise;
                delete promise.$owner;

                if (error) {
                    error.call(listener.scope, reason);
                }

                promise.reject(reason);
            }
        }

        return this;
    },

    cancel: function() {
        var listeners = this.getListeners(),
            owner = this.$owner,
            i, ln, listener;

        if (listeners) {
            for (i = 0, ln = listeners.length; i < ln; i++) {
                listener = listeners[i];
                listener.promise.cancel();
            }
            listeners.length = 0;
            delete this.listeners;
        }

        if (owner) {
            delete this.$owner;
            owner.cancel();
        }
    }
});

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

/**
 * This class allows you to use native APIs to take photos using the device camera.
 *
 * When this singleton is instantiated, it will automatically select the correct implementation depending on the
 * current device:
 *
 * - Sencha Packager
 * - PhoneGap
 * - Simulator
 *
 * Both the Sencha Packager and PhoneGap implementations will use the native camera functionality to take or select
 * a photo. The Simulator implementation will simply return fake images.
 *
 * ## Example
 *
 * You can use the {@link Ext.device.Camera#capture} function to take a photo:
 *
 *     Ext.device.Camera.capture({
 *         success: function(image) {
 *             imageView.setSrc(image);
 *         },
 *         quality: 75,
 *         width: 200,
 *         height: 200,
 *         destination: 'data'
 *     });
 *
 * See the documentation for {@link Ext.device.Camera#capture} all available configurations.
 *
 * @mixins Ext.device.camera.Abstract
 *
 * @aside guide native_apis
 */
Ext.define('Ext.space.Camera', {
    singleton: true,

    source: {
        library: 0,
        camera: 1,
        album: 2
    },

    destination: {
        data: 0, // Returns base64-encoded string
        file: 1  // Returns file's URI
    },

    encoding: {
        jpeg: 0,
        jpg: 0,
        png: 1
    },

    /**
     * Allows you to capture a photo.
     *
     * @param {Object} options
     * The options to use when taking a photo.
     *
     * @param {Function} options.success
     * The success callback which is called when the photo has been taken.
     *
     * @param {String} options.success.image
     * The image which was just taken, either a base64 encoded string or a URI depending on which
     * option you chose (destination).
     *
     * @param {Function} options.failure
     * The function which is called when something goes wrong.
     *
     * @param {Object} scope
     * The scope in which to call the `success` and `failure` functions, if specified.
     *
     * @param {Number} options.quality
     * The quality of the image which is returned in the callback. This should be a percentage.
     *
     * @param {String} options.source
     * The source of where the image should be taken. Available options are:
     *
     * - **album** - prompts the user to choose an image from an album
     * - **camera** - prompts the user to take a new photo
     * - **library** - prompts the user to choose an image from the library
     *
     * @param {String} destination
     * The destination of the image which is returned. Available options are:
     *
     * - **data** - returns a base64 encoded string
     * - **file** - returns the file's URI
     *
     * @param {String} encoding
     * The encoding of the returned image. Available options are:
     *
     * - **jpg**
     * - **png**
     *
     * @param {Number} width
     * The width of the image to return
     *
     * @param {Number} height
     * The height of the image to return
     */
    capture: function(options) {
        var sources = this.source,
            destinations = this.destination,
            encodings = this.encoding,
            source = options.source,
            destination = options.destination,
            encoding = options.encoding;

        if (sources.hasOwnProperty(source)) {
            source = sources[source];
        }

        if (destinations.hasOwnProperty(destination)) {
            destination = destinations[destination];
        }

        if (encodings.hasOwnProperty(encoding)) {
            encoding = encodings[encoding];
        }

        Ext.space.Communicator.send({
            command: 'Camera#capture',
            callbacks: {
                success: options.success,
                failure: options.failure
            },
            scope: options.scope,
            quality: options.quality,
            width: options.width,
            height: options.height,
            source: source,
            destination: destination,
            encoding: encoding
        });
    }
});

/**
 * This class is used to check if the current device is currently online or not. It has three different implementations:
 *
 * - Sencha Packager
 * - PhoneGap
 * - Simulator
 *
 * Both the Sencha Packager and PhoneGap implementations will use the native functionality to determine if the current
 * device is online. The Simulator version will simply use `navigator.onLine`.
 *
 * When this singleton ({@link Ext.device.Connection}) is instantiated, it will automatically decide which version to
 * use based on the current platform.
 *
 * ## Examples
 *
 * Determining if the current device is online:
 *
 *     alert(Ext.device.Connection.isOnline());
 *
 * Checking the type of connection the device has:
 *
 *     alert('Your connection type is: ' + Ext.device.Connection.getType());
 *
 * The available connection types are:
 *
 * - {@link Ext.device.Connection#UNKNOWN UNKNOWN} - Unknown connection
 * - {@link Ext.device.Connection#ETHERNET ETHERNET} - Ethernet connection
 * - {@link Ext.device.Connection#WIFI WIFI} - WiFi connection
 * - {@link Ext.device.Connection#CELL_2G CELL_2G} - Cell 2G connection
 * - {@link Ext.device.Connection#CELL_3G CELL_3G} - Cell 3G connection
 * - {@link Ext.device.Connection#CELL_4G CELL_4G} - Cell 4G connection
 * - {@link Ext.device.Connection#NONE NONE} - No network connection
 *
 * @mixins Ext.device.connection.Abstract
 *
 * @aside guide native_apis
 */
Ext.define('Ext.space.Connection', {
    extend: Ext.space.Observable,

    singleton: true,

    /**
     * @property {String} UNKNOWN
     * Text label for a connection type.
     */
    UNKNOWN: 'Unknown connection',

    /**
     * @property {String} ETHERNET
     * Text label for a connection type.
     */
    ETHERNET: 'Ethernet connection',

    /**
     * @property {String} WIFI
     * Text label for a connection type.
     */
    WIFI: 'WiFi connection',

    /**
     * @property {String} CELL_2G
     * Text label for a connection type.
     */
    CELL_2G: 'Cell 2G connection',

    /**
     * @property {String} CELL_3G
     * Text label for a connection type.
     */
    CELL_3G: 'Cell 3G connection',

    /**
     * @property {String} CELL_4G
     * Text label for a connection type.
     */
    CELL_4G: 'Cell 4G connection',

    /**
     * @property {String} NONE
     * Text label for a connection type.
     */
    NONE: 'No network connection',

    startWatching: function() {
        Ext.space.Communicator.send({
            command: 'Connection#watch',
            callbacks: {
                callback: this.doConnectionChange
            },
            scope: this
        });
    },

    onConnectionChange: function() {
        this.addListener.apply(this, arguments);
    },

    doConnectionChange: function(e) {
        this.invokeListeners(!!e.online, this[e.type]);
    }
});

/**
 * Provides a cross device way to show notifications. There are three different implementations:
 *
 * - Sencha Packager
 * - PhoneGap
 * - Simulator
 *
 * When this singleton is instantiated, it will automatically use the correct implementation depending on the current device.
 *
 * Both the Sencha Packager and PhoneGap versions will use the native implementations to display the notification. The
 * Simulator implementation will use {@link Ext.MessageBox} for {@link #show} and a simply animation when you call {@link #vibrate}.
 *
 * ## Examples
 *
 * To show a simple notification:
 *
 *     Ext.device.Notification.show({
 *         title: 'Verification',
 *         message: 'Is your email address: test@sencha.com',
 *         buttons: Ext.MessageBox.OKCANCEL,
 *         callback: function(button) {
 *             if (button === "ok") {
 *                 console.log('Verified');
 *             } else {
 *                 console.log('Nope');
 *             }
 *         }
 *     });
 *
 * To make the device vibrate:
 *
 *     Ext.device.Notification.vibrate();
 *
 * @mixins Ext.device.notification.Abstract
 *
 * @aside guide native_apis
 */
Ext.define('Ext.space.Notification', {
    singleton: true,

    show: function(config) {
        Ext.space.Communicator.send({
            command: 'Notification#show',
            callbacks: {
                callback: config.callback
            },
            scope  : config.scope,
            title  : config.title,
            message: config.message,
            buttons: config.buttons.join(',') //@todo fix this
        });
    },

    vibrate: function() {
        Ext.space.Communicator.send({
            command: 'Notification#vibrate'
        });
    }
});


/**
 * This class provides you with a cross platform way of listening to when the the orientation changes on the
 * device your application is running on.
 *
 * The {@link Ext.device.Orientation#orientationchange orientationchange} event gets passes the `alpha`, `beta` and
 * `gamma` values.
 *
 * You can find more information about these values and how to use them on the [W3C device orientation specification](http://dev.w3.org/geo/api/spec-source-orientation.html#deviceorientation).
 *
 * ## Example
 *
 * To listen to the device orientation, you can do the following:
 *
*     Ext.device.Orientation.on({
*         scope: this,
*         orientationchange: function(e) {
*             console.log('Alpha: ', e.alpha);
*             console.log('Beta: ', e.beta);
*             console.log('Gamma: ', e.gamma);
*         }
*     });
 *
 * @mixins Ext.device.orientation.Abstract
 *
 * @aside guide native_apis
 */
Ext.define('Ext.space.Orientation', {
    extend: 'Ext.space.Observable',

    singleton: true,

    /**
     * From the native shell, the callback needs to be invoked infinitely using a timer, ideally 50 times per second.
     * The callback expects one event object argument, the format of which should looks like this:
     *
     *     {
     *          alpha: 0,
     *          beta: 0,
     *          gamma: 0
     *     }
     *
     * Refer to [Safari DeviceOrientationEvent Class Reference][1] for more details.
     *
     * [1]: http://developer.apple.com/library/safari/#documentation/SafariDOMAdditions/Reference/DeviceOrientationEventClassRef/DeviceOrientationEvent/DeviceOrientationEvent.html
     */
    startWatching: function() {
        Ext.space.Communicator.send({
            command: 'Orientation#watch',
            callbacks: {
                callback: this.doDeviceOrientation
            },
            scope: this
        });
    },

    onDeviceOrientation: function() {
        this.addListener.apply(this, arguments);
    },

    doDeviceOrientation: function(e) {
        this.invokeListeners(e);
    }
});

Ext.define('Ext.space.invoke.Connection', {
    constructor: function(receiverId) {
        this.receiverId = receiverId;
    },

    send: function(message, foreground) {
        return Ext.space.Invoke.send(this.receiverId, message, foreground);
    },

    receive: function(message) {}
});

/**
 * Example for a sender:
 *
 *      Ext.require('Ext.space.Invoke', function(Invoke) {
 *          var broadcast = Invoke.broadcast('time'),
 *              connectToFirstReceiver = broadcast.then(function(receivers){
 *                  return Invoke.connect(receivers[0].id);
 *              }),
 *              send = connectToFirstReceiver.then(function(connection) {
 *                  // 'true' as second argument to bring the receiver app to the foreground
 *                  // otherwise, it will simply run in the background
 *                  return connection.send('You are summoned!', true);
 *              });
 *
 *          send.then(function(reply){
 *              console.log(reply);
 *          });
 *      });
 *
 * Example for a receiver:
 *
 *      Ext.require('Ext.space.Invoke', function(Invoke) {
 *          Invoke.onConnect(function(appId) {
 *              console.log('Got connection from ' + appId);
 *
 *              // Accept all
 *              return true;
 *          });
 *
 *          Invoke.onMessage(function(appId, message) {
 *              console.log('Got message from ' + appId + ' ' + message);
 *
 *              return 'Yeah I got it';
 *          });
 *      });
 *
 * For aync message handling:
 *
 *       Invoke.onMessage(function(appId, message) {
 *          var promise = new Ext.Promise();
 *
 *          console.log('Got message from ' + appId + ' ' + message);
 *
 *          // Do whatever needed asynchronously before return the result (fulfilling the promise)
 *          setTimeout(function(){
 *             promise.fulfill('Yeah I got it');
 *          }, 3000);
 *
 *          return promise;
 *      });
 */
Ext.define('Ext.space.Invoke', {
    singleton: true,

    messageId: 0,

    constructor: function() {
        var me = this,
            i, ln, message;

        this.pendingReceivePromises = {};
        this.connections = {};
        this.connectQueue = [];
        this.messageQueue = [];

        var pendingMessages = window.__invokeMessages,
            appId = window.__invokeAppId;

        delete window.__invokeAppId;
        this.appId = appId;

        if (!appId) {
            throw new Error("window.__invokeAppId is not set properly");
        }

        if (pendingMessages && pendingMessages.length > 0) {
            for (i = 0, ln = pendingMessages.length; i < ln; i++) {
                message = pendingMessages[i];
                this.onReceived(JSON.parse(atob(message)));
            }
        }

        delete window.__invokeMessages;

        window.__pushInvokeMessage = function(message) {
            // Release the execution flow for that the native process can continue right away
            // This enable any debugger statement during this flow to work probably
            setTimeout(function() {
                me.onReceived(JSON.parse(atob(message)));
            }, 1);
        }
    },

    /**
     * Create a connection to another application with the given id
     * @param {String} receiverId The id of the application to connect to. Get this id from #broadcast
     * @returns {Ext.Promise}
     */
    connect: function(receiverId) {
        var connections = this.connections,
            connection = connections[receiverId];

        if (connection) {
            return Ext.Promise.from(connection);
        }
        else {
            return this.send(receiverId, '__CONNECT__').then(function() {
                connections[receiverId] = connection = new Ext.space.invoke.Connection(receiverId);
                return connection;
            });
        }
    },

    /**
     * Send a message
     * @param {String} receiverId The id of the application to connect to. Get this id from #broadcast
     * @param {*} message The message to send, can be an object, as long as it is JSON-able.
     * @param {Boolean} [foreground] Whether or not to bring the receiver app to the foreground
     * @returns {Ext.Promise}
     */
    send: function(receiverId, message, foreground) {
        var messageId = this.messageId++,
            receivePromise = new Ext.Promise(),
            sendPromise = this.doSend(receiverId, messageId, message, foreground),
            pendingReceivePromises = this.pendingReceivePromises;

        pendingReceivePromises[messageId] = receivePromise;

        sendPromise.error(function(reason) {
            delete pendingReceivePromises[messageId];
            receivePromise.reject(reason);
        });

        return receivePromise;
    },

    /**
     * Assign the callback to handle new connection. The boolean returned value dertermine whether or not to accept
     * the connection
     * @param {Function} callback
     */
    onConnect: function(callback) {
        var queue = this.connectQueue.slice(0),
            i, ln, args;

        this.connectQueue.length = 0;

        if (callback) {
            this.connectCallback = callback;

            for (i = 0, ln = queue.length; i < ln; i++) {
                args = queue[i];
                this.onReceived.apply(this, args);
            }
        }
    },

    /**
     * Assign the callback to handling incoming messages. The returned value will be passed back to the sender.
     * If the operation needs to be async, simply return an instance of Ext.Promise
     * @param callback
     */
    onMessage: function(callback) {
        var queue = this.messageQueue.slice(0),
            i, ln, args;

        this.messageQueue.length = 0;

        if (callback) {
            this.messageCallback = callback;

            for (i = 0, ln = queue.length; i < ln; i++) {
                args = queue[i];
                this.onReceived.apply(this, args);
            }
        }
    },

    /**
     * @private
     */
    onAppConnect: function() {
        return this.connectCallback.apply(this, arguments);
    },

    /**
     * @private
     */
    onAppMessage: function(appId, message) {
        var connection = this.connections[appId],
            response;

        if (connection) {
            response = connection.receive(message);
        }

        if (typeof response == 'undefined') {
            response = this.messageCallback.apply(this, arguments);
        }

        return response;
    },

    /**
     * @private
     */
    onReceived: function(data) {
        console.warn('ON RECEIVED ', JSON.stringify(data, null, 2));
        var appId = data.appId,
            message = data.message,
            messageId = data.id,
            foreground = data.foreground,
            pendingReceivePromises = this.pendingReceivePromises,
            pendingPromise = pendingReceivePromises[messageId],
            connectCallback = this.connectCallback,
            messageCallback = this.messageCallback,
            response;

        delete pendingReceivePromises[messageId];

        // A response
        if (pendingPromise) {
            if (message.error) {
                pendingPromise.reject(message.error);
            }
            else {
                pendingPromise.fulfill(message.success);
            }
        }
        // A request
        else {
            try {
                if (message === '__CONNECT__') {
                    if (!connectCallback) {
                        this.connectQueue.push(arguments);
                        return;
                    }
                    else {
                        response = this.onAppConnect(appId);
                    }
                }
                else {
                    if (!messageCallback) {
                        this.messageQueue.push(arguments);
                        return;
                    }
                    else {
                        response = this.onAppMessage(appId, message);
                    }
                }

                if (response instanceof Ext.Promise) {
                    response.then(this, function(result) {
                        this.doSend(appId, messageId, {
                            success: result
                        }, foreground);
                    }, function(reason) {
                        this.doSend(appId, messageId, {
                            error: reason
                        }, foreground);
                    });
                }
                else {
                    this.doSend(appId, messageId, {
                        success: response
                    }, foreground);
                }
            }
            catch (e) {
                this.doSend(appId, messageId, {
                    error: e
                }, foreground);
                throw e;
            }
        }

    },

    /**
     * Broadcast a message (intent) to look for receivers who can respond to it
     * @param message
     * @returns {Ext.Promise} A promise which provides an array of objects upon fulfilled. Each object contains information about
     * a receiver, with 'id', 'name' and 'icon' keys.
     */
    broadcast: function(message) {
        var promise = new Ext.Promise;

        Ext.space.Communicator.send({
            command: 'Invoke#connect',
            callbacks: {
                success: function(result) {
                    if (!result || result.length === 0) {
                        promise.reject({
                            code: 1,
                            message: "There are no receivers for this connection"
                        });
                        return;
                    }

                    promise.fulfill(result);
                },
                failure: function(reason) {
                    promise.reject(reason);
                }
            },
            message: message
        });

        return promise;
    },

    /**
     * @private
     * @param receiverId
     * @param messageId
     * @param message
     * @param foreground
     * @returns {Ext.Promise}
     */
    doSend: function(receiverId, messageId, message, foreground) {
        var promise = new Ext.Promise;

        var success = function(result) {
            console.warn("RESULT", receiverId, messageId, result)
            promise.fulfill(result);
        };

        success.args = arguments;

        console.warn("GOING OUT", receiverId, messageId, message);

        Ext.space.Communicator.send({
            command: 'Invoke#send',
            callbacks: {
                success: success,
                failure: function(reason) {
                    promise.reject(reason);
                }
            },
            receiverId: receiverId,
            foreground: foreground,
            message: btoa(JSON.stringify({
                id: messageId,
                appId: this.appId,
                message: message,
                foreground: foreground
            }))
        });

        return promise;
    }
});

/**
 * The SQLResultSetRowList class which is used to represent rows returned by SQL statements.
 */
Ext.define('Ext.space.sqlite.SQLResultSetRowList', {
    names: null,
    rows: null,

    constructor: function(data) {
        this.names = data.names;
        this.rows = data.rows;
    },

    /**
     * Returns the number of rows returned by the SQL statement.
     *
     * @return {Number}
     * The number of rows.
     */
    getLength: function() {
        return this.rows.length;
    },

    /**
     * Returns a row at specified index returned by the SQL statement.
     * If there is no such row, returns null.
     *
     * @param {Number} index
     * The index of a row. This is required.
     *
     * @return {Object}
     * The row.
     */
    item: function(index) {
        if (index < this.getLength()) {
            var item = {};
            var row = this.rows[index];
            this.names.forEach(function(name, index) {
                item[name] = row[index];
            });

            return item;
        }

        return null;
    }
});

/**
 * The SQLResultSet class which is used to represent SQL statements results.
 */
Ext.define('Ext.space.sqlite.SQLResultSet', {
    insertId: 0,
    rowsAffected: 0,
    rows: null,

    constructor: function(data) {
        this.insertId = data.insertId;
        this.rowsAffected = data.rowsAffected;
        this.rows = new Ext.space.sqlite.SQLResultSetRowList(data);
    },

    /**
     * Returns the row ID of the last row that the SQL statement inserted into the database, if the statement inserted any rows.
     * If the statement did not insert a row, throws an exception.
     *
     * @return {Number}
     * The inserted row ID.
     */
    getInsertId: function() {
        if (this.insertId != 0) {
            return this.insertId;
        } else {
            throw new Error('Ext.space.sqlite.SQLResultSet#getInsertId: An SQLTransaction did not insert a row.');
            return null;
        }
    },

    /**
     * Returns the number of rows that were changed by the SQL statement.
     * If the statement did not change any rows, returns zero.
     *
     * @return {Number}
     * The number of rows affected.
     */
    getRowsAffected: function() {
        return this.rowsAffected;
    },

    /**
     * Returns a {@link Ext.space.sqlite.SQLResultSetRowList} instance representing rows returned by the SQL statement.
     *
     * @return {Ext.space.sqlite.SQLResultSetRowList}
     * The rows.
     */
    getRows: function() {
        return this.rows;
    }
});

/**
 * The SQLTransaction class which is used to execute SQL statements.
 */
Ext.define('Ext.space.sqlite.SQLTransaction', {
    id: 0,
    active: false,
    statements: null,

    constructor: function(id) {
        this.id = id;
        this.statements = [];
    },

    /**
     * Executes an SQL statement.
     *
     * @param {Object} config
     * The object which contains the following config options:
     *
     * @param {String} config.sqlStatement
     * The SQL statement to execute. This is required.
     *
     * @param {Array} config.arguments
     * The arguments array to bind each '?' placeholder in the SQL statement. This is optional.
     *
     * @param {Function} config.callback
     * The callback to be called when the SQL statement succeeded. This is optional.
     *
     * @param {Ext.space.sqlite.SQLTransaction} config.callback.transaction
     * The transaction of the SQL statement.
     *
     * @param {Ext.space.sqlite.SQLTransaction} config.callback.resultSet
     * The result of the SQL statement.
     *
     * @param {Function} config.failure
     * The callback to be called when an error occurred. This is optional.
     * If the callback returns false, next SQL statement will be executed.
     *
     * @param {Ext.space.sqlite.SQLTransaction} config.failure.transaction
     * The transaction of the SQL statement.
     *
     * @param {Object} config.failure.error
     * The occurred error.
     *
     * @param {Object} config.scope
     * The scope object
     */
    executeSql: function(config) {
        if (!this.active) {
            throw new Error('Ext.space.sqlite.SQLTransaction#executeSql: An attempt was made to use a SQLTransaction that is no longer usable.');
            return null;
        }

        if (config.sqlStatement == null) {
            throw new Error('Ext.space.sqlite.SQLTransaction#executeSql: You must specify a `sqlStatement` for the transaction.');
            return null;
        }

        this.statements.push({
            sqlStatement: config.sqlStatement,
            arguments: config.arguments,
            callback: config.callback,
            failure: config.failure,
            scope: config.scope
        });
    }
});

/**
 * The Database class which is used to perform transactions.
 */
Ext.define('Ext.space.sqlite.Database', {
    id: 0,
    version: null,

    constructor: function(id, version) {
        this.id = id;
        this.version = version;
    },

    /**
     * Returns the current version of the database.
     *
     * @return {String}
     * The current database version.
     */
    getVersion: function() {
        return Ext.space.Communicator.send({
            command: 'Sqlite#getVersion',
            sync: true,
            databaseId: this.id
        });
    },

    /**
     * Performs a {@link Ext.space.sqlite.SQLTransaction} instance with a read/write mode.
     *
     * @param {Object} config
     * The object which contains the following config options:
     *
     * @param {Function} config.callback
     * The callback to be called when the transaction has been created. This is required.
     *
     * @param {Ext.space.sqlite.SQLTransaction} config.callback.transaction
     * The created transaction.
     *
     * @param {Function} config.success
     * The callback to be called when the transaction has been successfully commited. This is optional.
     *
     * @param {Function} config.failure
     * The callback to be called when an error occurred and the transaction has been rolled back. This is optional.
     *
     * @param {Object} config.failure.error
     * The occurred error.
     *
     * @param {Object} config.scope
     * The scope object
     */
    transaction: function(config) {
        if (!config.callback) {
            throw new Error('Ext.space.sqlite.Database#transaction: You must specify a `callback` callback.');
            return null;
        }

        var me = this;
        Ext.space.Communicator.send({
            command: 'Sqlite#createTransaction',
            databaseId: this.id,
            readOnly: config.readOnly,
            callbacks: {
                success: function(id) {
                    var exception = null;
                    var error = null;
                    var transaction = new Ext.space.sqlite.SQLTransaction(id);

                    error = Ext.space.Communicator.send({
                        command: 'Sqlite#beginTransaction',
                        sync: true,
                        transactionId: transaction.id
                    });

                    if (!error && config.preflight) {
                        error = config.preflight.call(config.scope || this);
                    }

                    if (!error) {
                        try {
                            transaction.active = true;
                            config.callback.call(config.scope || this, transaction); // may throw exception
                        } catch (e) {
                            exception = e;
                        } finally {
                            transaction.active = false;
                        }
                    }

                    var statements = transaction.statements;

                    while (!(exception || error) && statements.length > 0) {
                        var statement = statements.shift();
                        var result = Ext.space.Communicator.send({
                            command: 'Sqlite#executeStatement',
                            sync: true,
                            transactionId: transaction.id,
                            databaseId: me.id,
                            version: me.version,
                            sqlStatement: statement.sqlStatement,
                            arguments: JSON.stringify(statement.arguments)
                        });

                        if (result) {
                            if (result.error) {
                                error = result.error;
                            } else if (statement.callback) {
                                var resultSet = new Ext.space.sqlite.SQLResultSet(result);

                                try {
                                    transaction.active = true;
                                    statement.callback.call(statement.scope || this, transaction, resultSet); // may throw exception
                                } catch (e) {
                                    exception = e;
                                } finally {
                                    transaction.active = false;
                                }
                            }
                        }

                        if (error && statement.failure) {
                            try {
                                transaction.active = true;
                                if (!statement.failure.call(statement.scope || this, transaction, error)) { // may throw exception
                                    error = null;
                                }
                            } catch (e) {
                                exception = e;
                            } finally {
                                transaction.active = false;
                            }
                        }
                    }

                    if (!(exception || error)) {
                        error = Ext.space.Communicator.send({
                            command: 'Sqlite#commitTransaction',
                            sync: true,
                            transactionId: transaction.id
                        });

                        if (!error) {
                            if (config.postflight) {
                                config.postflight.call(config.scope || this);
                            }

                            if (config.success) {
                                config.success.call(config.scope || this);
                            }
                        }
                    }

                    if (exception || error) {
                        statements.splice(0, statements.length);

                        Ext.space.Communicator.send({
                            command: 'Sqlite#rollbackTransaction',
                            sync: true,
                            transactionId: transaction.id
                        });

                        if (exception) {
                            throw exception;
                        } else if (config.failure) {
                            config.failure.call(config.scope || this, error);
                        }
                    }
                },
                failure: function(error) {
                    if (config.failure) {
                        config.failure.call(config.scope || this, error);
                    }
                }
            },
            scope: config.scope || this
        });
    },

    /**
     * Works same as {@link Ext.space.sqlite.Database#transaction}, but performs a {@link Ext.space.sqlite.SQLTransaction} instance with a read-only mode.
     */
    readTransaction: function(config) {
        this.transaction(Ext.apply(config, {
            readOnly: true
        }));
    },

    /**
     * Verifies and changes the version of the database at the same time as doing a schema update with a {@link Ext.space.sqlite.SQLTransaction} instance.
     *
     * @param {Object} config
     * The object which contains the following config options:
     *
     * @param {String} config.oldVersion
     * The current version of the database. This is required.
     *
     * @param {String} config.newVersion
     * The new version of the database. This is required.
     *
     * @param {Function} config.callback
     * The callback to be called when the transaction has been created. This is optional.
     *
     * @param {Ext.space.sqlite.SQLTransaction} config.callback.transaction
     * The created transaction.
     *
     * @param {Function} config.success
     * The callback to be called when the transaction has been successfully commited. This is optional.
     *
     * @param {Function} config.failure
     * The callback to be called when an error occurred and the transaction has been rolled back. This is optional.
     *
     * @param {Object} config.failure.error
     * The occurred error.
     *
     * @param {Object} config.scope
     * The scope object
     */
    changeVersion: function(config) {
        if (config.oldVersion == null) {
            throw new Error('Ext.space.Sqlite#changeVersion: You must specify an `oldVersion` of the database.');
            return null;
        }

        if (config.newVersion == null) {
            throw new Error('Ext.space.Sqlite#changeVersion: You must specify a `newVersion` of the database.');
            return null;
        }

        this.transaction(Ext.apply(config, {
            preflight: function() {
                return config.oldVersion == this.getVersion() ? null : 'Unable to change version: version mismatch';
            },
            postflight: function() {
                var result = Ext.space.Communicator.send({
                    command: 'Sqlite#setVersion',
                    sync: true,
                    databaseId: this.id,
                    version: config.newVersion
                });

                if (result) {
                    this.version = config.newVersion;
                }
            }
        }));
    }
});

Ext.define('Ext.space.Sqlite', {
    singleton: true,

    /**
     * Returns a {@link Ext.space.sqlite.Database} instance. If the database with specified name does not exist, it will be created.
     * If the creationCallback is provided, the database is created with the empty string as its version regardless of the specified version.
     *
     * @param {Object} config
     * The object which contains the following config options:
     *
     * @param {String} config.name
     * The name of the database to open. This is required.
     *
     * @param {String} config.version
     * The version of the database to open. This is required.
     *
     * @param {String} config.displayName
     * The display name of the database to open. This is required.
     *
     * @param {Number} config.estimatedSize
     * The estimated size of the database to open. This is required.
     *
     * @param {Function} config.creationCallback
     * The callback to be called when the database has been created. This is optional.
     *
     * @param {Ext.space.sqlite.Database} config.creationCallback.database
     * The created database with the empty string as its version regardless of the specified version.
     *
     * @param {Object} config.scope
     * The scope object. This is optional.
     *
     * @return {Ext.space.sqlite.Database}
     * The opened database, null if an error occured.
     */
    openDatabase: function(config) {
        if (config.name == null) {
            throw new Error('Ext.space.Sqlite#openDatabase: You must specify a `name` of the database.');
            return null;
        }

        if (config.version == null) {
            throw new Error('Ext.space.Sqlite#openDatabase: You must specify a `version` of the database.');
            return null;
        }

        if (config.displayName == null) {
            throw new Error('Ext.space.Sqlite#openDatabase: You must specify a `displayName` of the database.');
            return null;
        }

        if (config.estimatedSize == null) {
            throw new Error('Ext.space.Sqlite#openDatabase: You must specify a `estimatedSize` of the database.');
            return null;
        }

        var database = null;

        var result = Ext.space.Communicator.send({
            command: 'Sqlite#openDatabase',
            sync: true,
            name: config.name,
            version: config.version,
            displayName: config.displayName,
            estimatedSize: config.estimatedSize,
            callbacks: {
                // `creationCallback != null` is checked for internal logic in native plugin code
                creationCallback: !config.creationCallback ? null : function() {
                    config.creationCallback.call(config.scope || this, database);
                }
            },
            scope: config.scope || this
        });

        if (result) {
            if (result.error) {
                throw new Error(result.error);
                return null;
            }

            database = new Ext.space.sqlite.Database(result.id, result.version);

            return database;
        }

        return null;
    }
});
