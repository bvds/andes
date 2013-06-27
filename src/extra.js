(function() {
    window.__evaluate = function(base64Encoded) {
        var script = atob(base64Encoded);

        console.log('[EVALUATE] ', script);

        setTimeout(function() {
            try {
                eval(script);
            }
            catch (e) {
                console.error("[EVALUATE][ERROR] Failed evaluating script. Error: ", e.toString(), ". Script: ", script);
            }
        }, 1);

        return 'ok';
    };

    if (Ext.isSpace) {
        document.addEventListener("DOMContentLoaded", function() {
            var Communicator = Ext.space.Communicator,
                communicatorInitId;

            if (DEBUG) {
                $expect('Communicator#init to be called from native', 1000, Communicator, 'init');
            }

            communicatorInitId = Communicator.getCallbackId(Communicator.init, Communicator);

            // Notify native bridge
            window.location = 'sencha://ready.local/' + communicatorInitId;
        });
    }
})();
