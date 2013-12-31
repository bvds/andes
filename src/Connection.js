/**
  @aside guide sensor_apis
 * This class is used to check if the current device is currently online or not.
 *
 * ## Examples
 *

    Ext.space.Connection.getStatus().then(function(status){
        log("is online" + status.online + " " + status.type)
    });
    
 *
 * The available connection types are:
 *
 * - {@link Ext.space.Connection#UNKNOWN UNKNOWN} - Unknown connection
 * - {@link Ext.space.Connection#ETHERNET ETHERNET} - Ethernet connection
 * - {@link Ext.space.Connection#WIFI WIFI} - WiFi connection
 * - {@link Ext.space.Connection#CELL_2G CELL_2G} - Cell 2G connection
 * - {@link Ext.space.Connection#CELL_3G CELL_3G} - Cell 3G connection
 * - {@link Ext.space.Connection#CELL_4G CELL_4G} - Cell 4G connection
 * - {@link Ext.space.Connection#NONE NONE} - No network connection
 *
 * @mixins Ext.space.connection.Abstract
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
    WIFI: 'WIFI',

    /**
     * @property {String} CELL_2G
     * Text label for a connection type.
     */
    CELL_2G: 'Cell 2G',

    /**
     * @property {String} CELL_3G
     * Text label for a connection type.
     */
    CELL_3G: 'Cell 3G',

    /**
     * @property {String} CELL_4G
     * Text label for a connection type.
     */
    CELL_4G: 'Cell 4G',

    /**
     * @property {String} NONE
     * Text label for a connection type.
     */
    NONE: 'No network',

    startWatching: function() {
        Ext.space.Communicator.send({
            command: 'Connection#watch',
            callbacks: {
                callback: this.doConnectionChange
            },
            scope: this
        });
    },

    getStatus: function() {
        var result = new Ext.Promise();
        var self = this;
        Ext.space.Communicator.send({
            command: 'Connection#getStatus',
            callbacks: {
                callback: function(status){
                    result.fulfill(self._convertStatus(status));
                }
            }
        });
        return result;
    },

    /**
    *@private
    * converts the raw status object from the bridge into a usable version
    */
    _convertStatus: function(status){
        var isOnline = status.online == "1";
        var type = status.type;
        var typeString = this[status.type]
        return {online: isOnline, type: type, typeString: typeString };
    },

    doConnectionChange: function(e) {
        this.invokeListeners(this._convertStatus(e));
    }
});
