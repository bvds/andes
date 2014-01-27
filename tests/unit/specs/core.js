(function() {

describe("Core", function() {

    // TODO (rgg) This all runs after onSpaceReady because the test harness currently
    //            kicks off Mocha in an onSpaceReady callback. Perhaps someday we can
    //            rework this to be better able to test the core initialization code.

    it("Ext.isSpace should be true", function(done) {
        expect(Ext.isSpace).to.exist;
        expect(Ext.isSpace).to.be.true;
        done();
    });

    it("Ext.isSpaceReady should be true", function(done) {
        expect(Ext.isSpaceReady).to.exist;
        expect(Ext.isSpaceReady).to.be.true;
        done();
    });

    it("Ext.onSpaceReady should return a promise", function(done) {
        expect(Ext.onSpaceReady()).to.be.an.instanceof(Ext.Promise);
        done();
    });

    it("Ext.onSpaceReady should be able to take a callback", function(done) {
        Ext.onSpaceReady(function() {
            done();
        });
    });

    it("Ext.onSpaceReady should be able to take a scope", function(done) {
        var scope = {};

        Ext.onSpaceReady(function() {
            expect(this).to.equal(scope);
            done();
        }, scope);
    });

    it("Ext.onSpaceReady's returned promise should accept callbacks", function(done) {
        Ext.onSpaceReady().then(function() {
            done();
        });
    });

    it("Ext.onSpaceReady should call mixed callbacks and promises in sequence", function(done) {
        var message;

        Ext.onSpaceReady(function() {
            message = "callback";

        }).then(function() {
            expect(message).to.equal("callback");
            done();
        });
    });
});

})();
