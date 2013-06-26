(function() {
    var Communicator = Ext.space.Communicator,
        communicatorInitId = Communicator.getCallbackId(Communicator.init, Communicator);

    window.__evaluate = function(base64Encoded) {
        var script = atob(base64Encoded);

        console.log('[EVALUATE] ', script);

        setTimeout(function() {
            eval(script);
        }, 1)
    };

    DEBUG && $expect('Communicator#init to be called from native', 1000, Communicator, 'init');

    // Notify native bridge
    window.location = 'sencha://ready.local/' + communicatorInitId;

})();
