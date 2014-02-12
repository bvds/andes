/**
 * @private
 * The Download class which is used to represent a downloading file. It's basically
 * a property bag with two methods: getProgress() and cancel().
 */
Ext.define("Ext.space.filelocker.Download", {
    /**
     * Internal identifier for this download
     * @type {string}
     */
    downloadId: null,

    /**
     * Source URL
     * @type {string}
     */
    url: null,

    /**
     * MIME type
     * @type {string}
     */
    mimeType: null,

    /**
     * Progress so far
     * @type {number}
     */
    bytesDownloaded: 0,

    /**
     * Final size
     * @type {number}
     */
    totalBytes: 0,

    /**
     * Download status
     * @type {boolean}
     */
    isComplete: false,

    /**
     * When the download initiated
     * @type {Date}
     */
    
    dateStarted: null,

    /**
     * Destination file path/name
     * @type {string}
     */
    fileName: null,

    /**
     * @private
     */
    constructor: function(args) {
        this._updateWith(args);
    },

    /**
     * Check this download's progress.
     *
     * @return {Ext.Promise} Promise which will fulfill when progress is fetched and
     *                       updated into this object (and which is resolved with this
     *                       download as a parameter too).
     */
    getProgress: function() {
        return Ext.space.FileLocker.getProgress(this);
    },

    /**
     * Cancel this download.
     *
     * @return {Ext.Promise} Promise which will fulfills when the download is
     *                       successfully canceled. If the download is already done,
     *                       the promise will reject.
     */
    cancel: function() {
        return Ext.space.FileLocker.cancel(this);
    },

    /**
     * Bulk update this download with the data provided.
     *
     * @private
     * @param {object} source Object with data to overwrite onto this Download
     */
    _updateWith: function(source) {
        if (source) {
            if (source.downloadId) { this.downloadId = source.downloadId; }
            if (source.url) { this.url = source.url; }
            if (source.mimeType) { this.mimeType = source.mimeType; }
            if (source.bytesDownloaded) { this.bytesDownloaded = source.bytesDownloaded; }
            if (source.totalBytes) { this.totalBytes = source.totalBytes; }
            if (source.isComplete) { this.isComplete = true; }
            if (source.dateStarted) { this.dateStarted = source.dateStarted; }
            if (source.fileName) { this.fileName = source.fileName; }
        }
        return this;
    }
});
