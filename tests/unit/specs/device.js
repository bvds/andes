(function() {

describe("Device", function() {
    describe("Ext.space.Device", function() {
        it("Ext.space.Device should exist", function(done) {
            expect(Ext.space.Device).to.exist;
            done();
        });

        it("Ext.space.Device.openURL should exist", function(done) {
            expect(Ext.space.Device.openURL).to.exist;

            done();
        });

        it("should be able to get the device name", function(done) {
            var name = Ext.space.Device.name;
            expect(name).to.exist;

            done();
        });

        it("should be able to get the device uuid", function(done) {
            var uuid = Ext.space.Device.uuid;
            expect(uuid).to.exist;

            done();
        });

        it("should be able to get the device platform", function(done) {
            var platform = Ext.space.Device.platform;
            expect(platform).to.exist;

            done();
        });
    });
});

})();
