(function() {

describe("Connection", function() {
    describe("Ext.space.Connection", function() {
        it("Ext.space.Connection should exist", function(done) {
            expect(Ext.space.Connection).to.exist;
            done();
        });

        it("should be able to determine whether device is online", function(done) {
            Ext.space.Connection.getStatus({callback: function(e){
                  expect(e.online).to.exist;
                  expect(e.type).to.exist;
                  done();
            }});
          
           
        });
    });
});

})();
