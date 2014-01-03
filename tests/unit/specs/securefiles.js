(function() {


var testCollection;

describe("File Storage", function() {
    describe("Ext.space.SecureFiles", function() {

    	it("Ext.space.SecureFiles should exist", function(done) {
            expect(Ext.space.SecureFiles).to.exist;
            expect(Ext.space.files.Collection).to.exist;

            done();
        });



        it("Should be able to create collections", function(done) {
            testCollection = Ext.space.SecureFiles.get('myCollection');

	        expect(testCollection).to.exist;

            done();

        });

        it("Should store a string", function(done) {
            testCollection.set("first.txt", "Hello this is a test").then(function(){
                done();
            }, function(err){
                console.log("Could not write file", err);
                throw new Error(err);
            });
        });


        it("Should get a string", function(done) {
            testCollection.get("first.txt").then(function(str){
                expect(str).to.equal("Hello this is a test");
                done();
            }, function(err){
                console.log("Could not read file", err);
                throw new Error(err);
            });
        });

         it("Should not get a missing file", function(done) {
            testCollection.get("doesnotexist.txt").then(function(str){
                throw new Error("Got file contents for a file that does not exist.");
            }, function(err){
                done();
            });
        });

    });
});

})();
