(function() {

describe("Camera", function() {
    describe("Ext.space.Camera", function() {
        it("Ext.space.Camera should exist", function(done) {
                expect(Ext.space.Camera).to.exist;
                done();
        });

        it("Ext.space.Camera.capture should exist", function(done) {
            expect(Ext.space.Camera.capture).to.exist;
            done();
        });
    });
});

})();
