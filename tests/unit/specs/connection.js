(function() {

describe("Connection", function() {
    describe("Ext.space.Connection", function() {
        it("Ext.space.Connection should exist", function(done) {
            expect(Ext.space.Connection).to.exist;
            done();
        });


        it("covert status should handle online (as string)", function(done) {
            var result = Ext.space.Connection._convertStatus({online: "1", type:"WIFI"});
            expect(result).to.exist;
            expect(result.online).to.exist;
            expect(result.online).to.equal(true);

            expect(result.type).to.exist;
            expect(result.type).to.equal("WIFI");

            expect(result.typeString).to.exist;
            expect(result.typeString).to.equal(Ext.space.Connection.WIFI);

            done();
           
        });

         it("covert status should handle online (as number)", function(done) {
            var result = Ext.space.Connection._convertStatus({online: 1, type:"WIFI"});
            expect(result).to.exist;
            expect(result.online).to.exist;


            console.log("result.online", result.online);

            expect(result.online).to.equal(true);

            expect(result.type).to.exist;
            expect(result.type).to.equal("WIFI");

            expect(result.typeString).to.exist;
            expect(result.typeString).to.equal(Ext.space.Connection.WIFI);

            done();
           
        });

        it("covert status should handle offline", function(done) {
            var result = Ext.space.Connection._convertStatus({online: "0", type:"WIFI"});
            expect(result).to.exist;
            expect(result.online).to.exist;
            expect(result.online).to.equal(false);

            expect(result.type).to.exist;
            expect(result.type).to.equal("WIFI");

            expect(result.typeString).to.exist;
            expect(result.typeString).to.equal(Ext.space.Connection.WIFI);

            done();
           
        });

        it("covert status should handle unknown", function(done) {
            var result = Ext.space.Connection._convertStatus({online: undefined, type:"WIFI"});
            expect(result).to.exist;
            expect(result.online).to.exist;
            expect(result.online).to.equal(false);

            expect(result.type).to.exist;
            expect(result.type).to.equal("WIFI");

            expect(result.typeString).to.exist;
            expect(result.typeString).to.equal(Ext.space.Connection.WIFI);

            done();
           
        });

        it("covert status should handle online CELL", function(done) {
            var result = Ext.space.Connection._convertStatus({online: "1", type:"CELL_3G"});
            expect(result).to.exist;
            expect(result.online).to.exist;
            expect(result.online).to.equal(true);


            expect(result.type).to.exist;
            expect(result.type).to.equal("CELL_3G");

            expect(result.typeString).to.exist;
            expect(result.typeString).to.equal(Ext.space.Connection.CELL_3G);

            done();
           
        });


        it("covert status should handle offline CELL", function(done) {
            var result = Ext.space.Connection._convertStatus({online: "0", type:"CELL_3G"});
            expect(result).to.exist;
            expect(result.online).to.exist;
            expect(result.online).to.equal(false);

            expect(result.type).to.exist;
            expect(result.type).to.equal("CELL_3G");

            expect(result.typeString).to.exist;
            expect(result.typeString).to.equal(Ext.space.Connection.CELL_3G);

            done();
           
        });

        it("should be able to determine whether device is online", function(done) {
            var result = Ext.space.Connection.getStatus();

            expect(result).to.exist;
            expect(result.then).to.exist;
            

            result.then(function(status){

                  expect(status.online).to.exist;
                  expect(status.online).to.equal(true);
                  expect(status.type).to.exist;
                  done();
            });
          
           
        });
    });
});

})();
