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
 *     Ext.space.Notification.show({
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
 *     Ext.space.Notification.vibrate();
 *
 * @mixins Ext.space.notification.Abstract
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

