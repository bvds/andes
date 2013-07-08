/**
 * @private
 *
 * This object handles communication between the WebView and Sencha's native shell.
 * Currently it has two primary responsibilities:
 *
 * 1. Maintaining unique string ids for callback functions, together with their scope objects
 * 2. Serializing given object data into HTTP GET request parameters
 *
 * As an example, to capture a photo from the device's camera, we use `Ext.space.Camera.capture()` like:
 *
 *     Ext.space.Camera.capture(
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
 * Internally, `Ext.space.Communicator.send()` will then be invoked with the following argument:
 *
 *     Ext.space.Communicator.send({
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
 * respectively) and maintained by `Ext.space.Communicator`.
 *
 * Whenever the requested operation finishes, `Ext.space.Communicator.invoke()` simply needs
 * to be executed from the native shell with the corresponding ids given before. For example:
 *
 *     Ext.space.Communicator.invoke('3', ['DATA_URI_OF_THE_CAPTURED_IMAGE_HERE']);
 *
 * will invoke the original `onSuccess` callback under the given scope. (`callbackScope`), with
 * the first argument of 'DATA_URI_OF_THE_CAPTURED_IMAGE_HERE'
 *
 * Note that `Ext.space.Communicator` maintains the uniqueness of each function callback and
 * its scope object. If subsequent calls to `Ext.space.Communicator.send()` have the same
 * callback references, the same old ids will simply be reused, which guarantee the best possible
 * performance for a large amount of repetitive calls.
 */
Ext.define('Ext.space.Communicator', {
    singleton: true,

    SERVER_URL: 'http://127.0.0.1:30015/', // Change this to the correct server URL

    callbackDataMap: {},

    callbackIdMap: {},

    idSeed: 0,

    globalScopeId: '0',

    constructor: function() {
        this.sendQueue = [];
    },

    init: function(info) {
        var queue = this.sendQueue,
            appId = info.appId,
            device = info.device,
            session = info.session,
            messages = info.messages;

        if (DEBUG && !appId) {
            throw new Error("[Communicator#init] Missing appId");
        }

        if (DEBUG && !device) {
            throw new Error("[Communicator#init] Missing device info");
        }

        if (DEBUG && !session) {
            throw new Error("[Communicator#init] Missing session info");
        }

        if (DEBUG && !messages || !Array.isArray(messages)) {
            throw new Error("[Communicator#init] Missing messages");
        }

        this.device = device;
        this.session = session;
        this.appId = appId;

        Ext.setSpaceReady();

        Ext.space.Invoke.invoke(messages);

        if (queue.length > 0) {
            queue.forEach(function(args) {
                this.send(args);
            }, this);

            queue.length = 0;
        }

        this.watchTitle();
    },

    watchTitle: function() {
        if (Ext.spaceIsAndroid) {
            var currentTitle = document.title;

            document.__defineSetter__('title', function(title) {
                document.__title = title;
                me.send({
                    command: 'TitleWatcher#update',
                    title: title
                });
                return title;
            });

            document.__defineGetter__('title', function() {
                return document.__title;
            });

            document.title = currentTitle;
        }
        else {
            var me = this,
                target = document.querySelector('head > title'),
                observer = new window.WebKitMutationObserver(function(mutations) {
                    mutations.forEach(function(mutation) {
                        me.send({
                            command: 'TitleWatcher#update',
                            title: mutation.target.textContent
                        });
                    });
                });

            target && observer.observe(target, { subtree: true, characterData: true, childList: true });
        }
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
        }
        else if (scope.isIdentifiable) {
            scopeId = scope.getId();
        }
        else {
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
        if (!Ext.isSpaceReady) {
            if (synchronous) {
                throw new Error('Making synchronous request while Space isn\'t yet ready. ' +
                    'Please wrap the statement inside Ext.onSpaceReady(callback)');
            }

            this.sendQueue.push(args);
            return;
        }

        var callbacks, scope, name, callback;

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

        return this.doSend(args, synchronous);
    },

    doSend: function() {
        if (Ext.spaceIsBlackBerry) {
            return function(args, synchronous) {
                var data = {
                    args: args,
                    appId: this.appId,
                    sync: synchronous
                };

                navigator.cascades.postMessage(window.btoa(JSON.stringify(data)));

                var xhr = new XMLHttpRequest();
                xhr.open('GET', 'getdata', false);
                xhr.send(null);

                var result = xhr.responseText;

                if (result) {
                    return JSON.parse(result);
                }
            }
        }
        else if (Ext.spaceIsAndroid) {
            return function(args, synchronous) {
                var data = {
                    args: args,
                    appId: this.appId,
                    sync: synchronous
                };

                var result = window.Sencha.action(JSON.stringify(data));

                if (result) {
                    return JSON.parse(result);
                }
            }
        }
        else {
            return function(args, synchronous) {
                var response, data, xhr;

                xhr = new XMLHttpRequest();
                xhr.open('POST', this.SERVER_URL + '?_dc=' + new Date().getTime(), !synchronous);
                xhr.setRequestHeader('Content-Type', 'text/plain');

                if (!this.appId) {
                    throw new Error("Missing appId at this point");
                }

                data = {
                    args: args,
                    appId: this.appId,
                    sync: synchronous
                };

                data = JSON.stringify(data);

                DEBUG && console.log("[OUT]", data);

                if (!synchronous) {
                    xhr.onreadystatechange = function() {
                        if (xhr.readyState == 4) {
                            var status = xhr.status;

                            if (status !== 200) {
                                throw new Error("Failed communicating to native bridge, got status code: " + status + ". " +
                                    "XHR Data: " + data);
                            }
                        }
                    }
                }

                xhr.send(data);

                if (synchronous) {
                    response = xhr.responseText;

                    try {
                        response = JSON.parse(response);
                    }
                    catch (e) {
                    }

                    return response;
                }
            }

        }
    }(),

    notifyReady: function() {
        var Communicator = Ext.space.Communicator,
            communicatorInitId = Communicator.getCallbackId(Communicator.init, Communicator);

        if (Ext.spaceIsIos) {
            window.location = 'sencha://ready.local/' + communicatorInitId;
        }
        else {
            this.doSend({
                command: "ViewStateManager#setOnReady",
                callback: communicatorInitId
            });
        }
    }
});
