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
