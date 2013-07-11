(function() {

describe("Notification", function() {
    describe("Ext.space.Notification", function() {
        it("Ext.space.Notification should exist", function(done) {
                expect(Ext.space.Notification).to.exist;
                done();
        });

        it("should be able to show a notification", function(done) {
            expect(Ext.space.Notification.show).to.exist;
            done();
        });

        it("should be able to vibrate", function(done) {
            expect(Ext.space.Notification.vibrate).to.exist;
            done();
        });
    });
});

})();
