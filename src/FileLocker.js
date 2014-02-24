/**
 * Promise-based API for downloading files via URL.
 *
 * To download a file:
 *
 *      // list some details when the download completes
 *      function onComplete(download) {
 *          console.log("File finished: " + download.url);
 *          console.log("Size: " + download.totalBytes);
 *          console.log("Saved to: " + download.fileName);
 *      }
 *
 *      Ext.space.FileLocker.download({ url: "http://www.sencha.com/" }).then(onComplete);
 *
 * The `download` objects involved here are instances of Ext.space.filelocker.Download.
 *
 * To get a list of all downloads currently in progress, plus up to the ten most
 * recently completed downloads:
 *
 *      Ext.space.FileLocker.getDownloads().then(function(downloads) {
 *          downloads.forEach(function(download) {
 *              console.log(download.fileName);
 *          });
 *      });
 *
 * If you have a download object and want to fetch the latest information about it,
 * you can get the progress of a single download at a time:
 *
 *      download.getProgress().then(function(updatedDownload) {
 *          console.log(updatedDownload.bytesDownloaded + " bytes downloaded");
 *      });
 *
 * To cancel a download in progress:
 *
 *      download.cancel().then(function() {
 *          console.log("Canceled!");
 *      });
 *
 * @aside guide file_locker
 *
 */
(function(){

// utility function for returning a particular field from the item parameter, or if
// item is a string, the item itself; if neither is valid, return undefined
function extract(item, field) {
    if (item[field]) {
        return item[field];
    } else if (typeof item == "string") {
        return item;
    }
    // intentionally don't return anything
}


// utility function for creating a promise and wiring up callbacks if they were
// provided in the args object (i.e., "callback style invocation").
function promisize(args, options) {
    var promise = new Ext.Promise(),
        supportProgress = !!(options && options.supportProgress);

    if (args && (args.onComplete || args.onError || (supportProgress && args.onProgress))) {
        promise.then(
            typeof args.onComplete == "function" ? args.onComplete : undefined,
            typeof args.onError == "function" ? args.onError : undefined,
            // TODO: create a standard progress handler that invokes both
            //       options.onProgress and args.onProgress (if it exists), and use that
            (supportProgress && typeof args.onProgress == "function") ? args.onProgress : undefined
        );
    }

    return promise;
}


// utility function to filter out the weird empty items that sometimes come back from
// the native side, that look like they're still in progress, but they're invalid;
function downloadIsReal(item) {
    return item.isComplete || (!!item.totalBytes && !!item.fileName);
}


// TODO: Support progress events.
//
// For that, we need our Promise implementation to support them as well, and we'll
// want to store a cache of the promises we create. Then when we get data from
// watchDownloads(), we can send the update in through the cached promises.
//
// If app code wants to listen for updates, that will happen by attaching a progress
// handler to the promise returned by download(), so that means we'll need to
// automatically create a standard progress handler on every download and fire
// progress events on every download we create, whether the app code actually asked
// for them or not (because apps can always add such handlers later, and the File
// Locker won't have any knowledge of that).
//
Ext.define("Ext.space.FileLocker", {
    singleton: true,

    /**
     * @private
     * Cache of the download information returned by the native bridge
     */
    downloads: null,

    /**
     * @private
     * Cache of the downloads' Promise objects
     */
    promises: null,

    /**
     * @private
     * Whether or not the file locker has registered callbacks with the native bridge Locker#watchDownloads
     */
    watching: false,

    /**
     * @private
     */
    constructor: function() {
        this.downloads = {};
        this.promises = {};
    },

    /**
     * Download a file.
     *
     * Normal usage is to pass in only the URL, and attach callbacks to the returned
     * Promise via .then(...). If you pass callbacks directly, in the args parameter,
     * they are installed as the respective Promise handlers before returning.
     *
     * @param {string|object} args URL to download, or property bag with .url,
     *                             .onComplete, .onError, .onProgress
     * @return {Ext.Promise} Promise which will receive an Ext.space.filelocker.Download object
     */
    download: function(args) {
        var url, promise, locker = this;

        if (args) {
            // TODO: wire up the standard progress handler (see the big TODO above)
            promise = promisize(args, {supportProgress: true});
            url = extract(args, "url");

            if (url) {
                Ext.space.Communicator.send({
                    command: "Locker#download",
                    callbacks: {
                        onStart: function(id) {
                            if (id) {
                                // cache a reference to the Download object, so we can
                                // continue to update it over time
                                locker.downloads[id] = new Ext.space.filelocker.Download();
                                locker.promises[id] = promise;
                            }

                            locker.watchDownloads();
                        },
                        // no onSuccess callback because we'll let watchDownloads do
                        // the necessary notification
                        // onSuccess: function(id) {},
                        onError: function(error) {
                            promise.reject(error);
                        }
                    }
                });

            }
        }

        if (!args || !url) {
            promise.reject("Missing URL");
        }

        return promise;
    },

    /**
     * Retrieve the current status of all active downloads, plus up to 10 of the most
     * recently completed downloads.
     *
     * @param {object} args (optional) Object with .onComplete and/or .onError
     *                      callback(s) to run when the download finishes
     * @return {Ext.Promise} Promise which will receive an array of
     *                       Ext.space.filelocker.Download objects
     */
    getDownloads: function(args) {
        var promise = promisize(args, {supportProgress: false});

        var locker = this;

        function makeDownload(item) {
            var id = item.downloadId;
            if (locker.downloads[id]) {
                return locker.downloads[id]._updateWith(item);
            } else {
                locker.downloads[id] = new Ext.space.filelocker.Download(item);
                return locker.downloads[id];
            }
        }

        Ext.space.Communicator.send({
            command: "Locker#getDownloads",
            callbacks: {
                onSuccess: function(responses) {
                    if (Object.prototype.toString.call(responses) === "[object Array]") {
                        // resolve with an array of Download objects
                        promise.fulfill(responses.filter(downloadIsReal).map(makeDownload));
                        locker.watchDownloads();

                    } else {
                        // what happened?
                        promise.reject("Malformed (non-Array) response from the native bridge");
                    }
                },
                onError: function(error) {
                    promise.reject(error);
                }
            }
        });

        return promise;
    },

    /**
     * Check a download's progress (normally done via download.getProgress()).
     *
     * @private
     * @param {string|object} args Download ID of the download to check, or an object
     *                             containing a .downloadId property containing such.
     * @return {Ext.Promise} Promise which will receive an up-to-date copy of the
     *                       Ext.space.filelocker.Download
     */
    getProgress: function(args) {
        var id, promise, match, locker = this;

        if (args) {
            promise = promisize(args, {supportProgress: false});
            id = typeof args == "number" ? args : extract(args, "downloadId");

            if (id && locker.downloads[id]) {
                if (locker.downloads[id].isComplete) {
                    // if it's cached and complete, return it
                    promise.fulfill(locker.downloads[id]);

                } else {
                    // if it's cached and incomplete, get it from getDownloads
                    this.getDownloads().then(function(downloads) {
                        downloads.some(function(download) {
                            if (download.downloadId === id) {
                                match = download;
                                return true;
                            }
                        });

                        if (match) {
                            promise.fulfill(match);
                        } else {
                            promise.reject("Download " + id + " not found");
                        }

                    }, function(error) {
                        promise.reject(error);
                    });
                }
            }


        }

        if (!args || !id) {
            if (!promise) {
                promise = new Ext.Promise();
            }
            promise.reject("Missing download ID");
        } else if (!locker.downloads[id]) {
            promise.reject("Download " + id + " not found");
        }

        return promise;
    },

    /**
     * Cancel a download (normally done via download.cancel()).
     *
     * @private
     * @param {string|object} args Download ID of the download to check, or an object
     *                             containing a .downloadId property containing such.
     * @return {Ext.Promise} Promise which will resolve when the download is canceled. If
     *                       the download is already done or canceled, it will reject.
     */
    cancel: function(args) {
        var id, promise = new Ext.Promise(), locker = this;

        if (args) {
            promise = promisize(args, {supportProgress: false});
            id = extract(args, "downloadId");

            if (id) {
                Ext.space.Communicator.send({
                    command: "Locker#cancel",
                    downloadId: id,
                    callbacks: {
                        onSuccess: function() {
                            promise.fulfill(true);
                        },
                        onError: function(error) {
                            promise.reject(error);
                        }
                    }
                });
            }
        }

        if (!args || !id) {
            promise.reject("Missing download ID");
        }

        return promise;
    },

    /**
     * Watch for updates coming in from the native bridge, to keep the internal
     * cache up to date
     *
     * @private
     */
    watchDownloads: function() {
        var locker = this,
            cache = this.downloads,
            promises = this.promises,
            activeCount = 0;

        function processItem(item) {
            var id = item.downloadId,
                alreadyComplete = !(id in cache) || cache[id].isComplete,
                justCompleted = !alreadyComplete && item.isComplete;

            // count the downloads still in progress to we know when to unwatch
            // (we check totalBytes and fileName because if an invalid bridge call
            // makes it through, the native side will return an object with zeroes
            // across the board, no filename, and isComplete == false)
            if (!item.isComplete && downloadIsReal(item)) {
                activeCount++;
            }

            // create or update the cached download object
            if (cache[id]) {
                cache[id]._updateWith(item);
            } else {
                cache[id] = new Ext.space.filelocker.Download(item);
            }

            // resolve the original promise with the final data
            if (justCompleted && (id in promises)) {
                promises[id].fulfill(cache[id]);
            }
        }

        if (!locker.watching) {
            locker.watching = true;
            Ext.space.Communicator.send({
                command: "Locker#watchDownloads",
                callbacks: {
                    onSuccess: function(responses) {
                        activeCount = 0;
                        if (Object.prototype.toString.call(responses) === "[object Array]") {
                            responses.forEach(processItem);
                            if (!activeCount) {
                                locker.unwatchDownloads();
                            }
                        }
                    },
                    onError: function(error) {
                        locker.unwatchDownloads();
                    }
                }
            });
        }
    },

    /**
     * Discontinue watching for download updates from the native bridge
     *
     * @private
     */
    unwatchDownloads: function() {
        if (this.watching) {
            Ext.space.Communicator.send({
                command: "Locker#unwatchDownloads"
            });
            this.watching = false;
        }
    }
});

}());
