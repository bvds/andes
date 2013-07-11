(function() {

describe("Communicator", function() {
    describe("Ext.space.Communicator", function() {
        it("Ext.space.Communicator should exist", function(done) {
            expect(Ext.space.Communicator).to.exist;
            done();
        });

        it("Ext.space.Communicator.send should exist", function(done) {
            expect(Ext.space.Communicator.send).to.exist;
            done();
        });
    });
});

})();
