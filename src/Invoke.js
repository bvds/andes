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
