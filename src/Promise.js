/**
*  Ext.Promise an Asynchronous API based on the Promises A+ spec http://promisesaplus.com
*
*
*   Promises are used extensively by Space's APIs. Most of Space's APIs return promises. 
*   In the case of Ext.space.Invoke, the called application needs to create, return, and resolve 
*   promises.
*   
*   To understand how promises work, here is a simple promise-based version of setTimeout:
*
*       function wait(time) {
*
*           var promise = new Ext.Promise();
*
*           setTimeout(function(){
*                promise.fulfill({resolved: new Date().getTime()});
*           }, time);
*
*           return promise;
*
*       }
*
*   First create the promise, then return the promise so that the caller can react to the result of the promise.
*   
*   The promise can later be resolved when the data is available:
*   
*       promise.fulfill(response);
*
*   If an error occurs, call reject on the promise:
*   
*       promise.reject(errorMessage);
*
*
*   Now your code can call wait instead of setTimeout:
*
*       wait(1000).then(success, failure);
*
*       var success = function(result) {
*           // Do something with the result
*           console.log("resolved at" + result.resolved);
*       };
*
*       var failure = function(error) {
*           console.error('Something went wrong', error);
*       }
* 
*/
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

    then: function(success, error) {
        var Promise = Ext.Promise,
            completed = this.completed,
            promise, result;

        if (completed === -1) {
            if (error) {
                error(this.lastReason);
            }
            return this;
        }

        if (completed === 1 && !this.isFulfilling) {
            if (!success) {
                return this;
            }

            result = success.apply(null, this.lastResults);

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
                success: success,
                error: error,
                promise: promise
            });
        }

        return promise;
    },

    error: function(error) {
        return this.then(null, error);
    },

    fulfill: function() {
        var results = arguments,
            listeners, listener, success, promise, callbackResults;

        this.lastResults = results;
        this.completed = 1;

        while (listeners = this.getListeners()) {
            delete this.listeners;
            this.isFulfilling = true;

            while (listener = listeners.shift()) {
                success = listener.success;

                if (success) {
                    promise = listener.promise;
                    delete promise.$owner;

                    callbackResults = success.apply(null, results);

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

        me.then(function(result) {
            this.fulfill(result);
            return result;
        }.bind(promise), promise.reject.bind(promise));
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
                    error(reason);
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
