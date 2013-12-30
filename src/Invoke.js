/**
    @aside guide invoke
    The Invoke API allows Applications running inside a Sencha Space client to communicate.
    Applications can securely exchange data with each other.  

    When one application requests data from another, that application loads, and the user 
    is shown the called app. Once the user is done interacting with the called app,
    the called app returns data back to the calling application, and Sencha Space
    returns the user to the original application. 

    The two primary functions for Invoke are Ext.space.Invoke.get and Ext.space.Invoke.onMessage

    For additional information on how to use  please see our [Invoke Guide](#!/guide/invoke) and [example applications](#!/guide/examples)

 */
Ext.define('Ext.space.Invoke', {
    singleton: true,

    messageId: 0,


    /*
    * @private
    */
    constructor: function() {
        this.pendingReceivePromises = {};
        this.connections = {};
        this.connectQueue = [];
        this.messageQueue = [];
    },


    /*
    * @private
    */
    invoke: function(messages) {
        var me = this;

        if (!Array.isArray(messages)) {
            throw new Error('[Invoke#invoke] Invalid messages, must be an array');
        }

        // Unblock native thread
        setTimeout(function() {
            messages.forEach(function(message) {
                me.onReceived(message);
            })
        }, 1);
    },

    /**
     * Get a connection to another application.

        Ext.space.Invoke.get('photos').then(send, failure);

        var failure = function(error) {
            console.('Received error:', error);
        }

        var send = function(connection) {
            connection.send(data, background).then(
                success,
                failure
            );
        };

     * @param {String} receiverId The ID of the application to connect to. Get this ID from #broadcast
     * @returns {Ext.Promise}
     */
    get: function(broadcastMessage) {
        var connections = this.connections,
            connection = connections[broadcastMessage];

        if (connection) {
            return Ext.Promise.from(connection);
        }
        else {
            return this.broadcast(broadcastMessage).then(this, function(receiverIds) {
                connections[broadcastMessage] = connection = new Ext.space.invoke.Connection(receiverIds[0].id);
                return connection;
            });
        }
    },

    /**
     * Send a message
     * @private
     * @param {String} receiverId The ID of the application to connect to. Get this ID from #broadcast
     * @param {*} message The message to send, can be an object, as long as it is JSON-able.
     * @param {Boolean} [foreground] Whether or not to bring the receiver app to the foreground.
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
     * @private
     * Assign the callback to handle a new connection.  
     * The Boolean returned value determines whether or not 
     * to accept the connection.
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
     *   onMessage registers a function to be called each time another application 
     *   invokes this application. 
     *
     *   For example for the photos application to respond to a request to get photos:
     *
     *       Invoke.onMessage(function(appId, message) {
     *          var promise = new Ext.Promise();
     *
     *          console.log('Got message from ' + appId + ' ' + message);
     *
     *          // Do whatever is needed asynchronously before returning the result 
     *          //  (fulfilling the promise)
     *          setTimeout(function(){
     *             promise.fulfill('Yeah I got it');
     *          }, 3000);
     *
     *          return promise;
     *      });
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
     * @private
     * Broadcast a message (intent) to look for receivers who can respond to the message.
     * @param message
     * @returns {Ext.Promise} A promise that provides an array of objects to fulfill. 
     * Each object contains information about a receiver, with 'id', 'name', and 'icon' keys.
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
        var promise = new Ext.Promise,
            appId = Ext.space.Communicator.appId;

        var success = function(result) {
            promise.fulfill(result);
        };

        success.args = arguments;

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
            message: {
                id: messageId,
                appId: appId,
                message: message,
                foreground: foreground
            }
        });

        return promise;
    }
});
