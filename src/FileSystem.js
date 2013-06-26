/**
 * Provides an API to navigate file system hierarchies.
 *
 * @mixins Ext.space.filesystem.Sencha
 *
 * @aside guide native_apis
 */
Ext.define('Ext.space.FileSystem', {
    singleton: true,

    requires: [
        'Ext.space.Communicator',
        'Ext.space.filesystem.Cordova',
        'Ext.space.filesystem.Chrome',
        'Ext.space.filesystem.Simulator',
        'Ext.space.filesystem.Sencha'
    ],

    constructor: function() {
        var browserEnv = Ext.browser.is;
        if (browserEnv.WebView) {
            if (browserEnv.Cordova) {
                return Ext.create('Ext.space.filesystem.Cordova');
            } else {
                return Ext.create('Ext.space.filesystem.Sencha');
            }
        } else if (browserEnv.Chrome) {
            return Ext.create('Ext.space.filesystem.Chrome');
        }

        return Ext.create('Ext.space.filesystem.Simulator');
    }
});
