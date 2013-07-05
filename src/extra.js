(function() {
    window.__evaluate = function(base64Encoded) {
        var script = atob(base64Encoded);

        DEBUG && console.log('[EVALUATE] ', script);

        setTimeout(function() {
            try {
                eval(script);
            }
            catch (e) {
                if (e.constructor !== Error) {
                    DEBUG && console.error("[EVALUATE][ERROR] Failed evaluating script. Error: ", e.toString(), ". Script: ", script);
                }
                else {
                    throw e;
                }
            }
        }, 1);

        return 'ok';
    };

    if (Ext.isSpace) {
        function notifyNative() {
            Ext.space.Communicator.notifyReady();
        }

        if ('onReady' in Ext) {
            Ext.onReady(notifyNative);
        }
        else {
            if (document.readyState.match(/interactive|complete|loaded/) !== null) {
                notifyNative();
            }
            else {
                window.addEventListener('DOMContentLoaded', notifyNative, false);
            }
        }
    }
})();
