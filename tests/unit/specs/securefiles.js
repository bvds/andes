(function() {


var testCollection;

var erCb = function(message) {
    return function(err){
        console.error(message, err);
        throw new Error(err);
    }
}

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
            },erCb("Could not write file"));
        });


        it("Should get a string", function(done) {
            testCollection.get("first.txt").then(function(str){
                expect(str).to.equal("Hello this is a test");
                done();
            },erCb("Could not read file"));
        });

         it("Should not get a missing file", function(done) {
            testCollection.get("doesnotexist.txt").then(function(str){
                throw new Error("Got file contents for a file that does not exist.");
            }, function(err){
                done();
            });
        });

        it("has() should return false for non-existent file", function(done) {
            testCollection.has("doesnotexist.txt").then(function(result){
               expect(result).to.be.false;
               done();
            });
        });

        it("has() should return true for file that exists", function(done) {
            testCollection.has("first.txt").then(function(result){
               expect(result).to.be.true;
               done();
            });
        });

        it("Remove should remove the file", function(done) {
            testCollection.delete("first.txt").then(function(){
                testCollection.has("first.txt").then(function(result){
                   expect(result).to.be.false;
                   done();
                });
            }, erCb("Could not delete the file"));
        });

        it("count should be zero", function(done) {
            testCollection.count().then(function(count){ 
                expect(count).to.exist;
                expect(count).to.equal(0);
                done();
             });
        });

        it("keys should be empty", function(done) {
            testCollection.keys().then(function(keys){ 
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(0);
                done();
             });
        });

        it("foreach should be empty", function(done) {
            testCollection.forEach(function(key){
                console.log("callback!", key);
                throw new Error("Collection should be empty, callback should not have been called.");
            }).then(function(keys){
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(0);
                done();
            });
            
        });

        it("should be able to create multiple files", function(done) {
            testCollection.set("second.txt", "a").then(function(){
               testCollection.set("third.txt", "b").then(function(){
                    testCollection.set("fourth.txt", "c").then(function(){
                       done();
                    });
                });
            });
        });

        it("count should be 3", function(done) {
            testCollection.count().then(function(count){ 
                expect(count).to.exist;
                expect(count).to.equal(3);
                done();
             });
        });

        it("keys should return 3 files", function(done) {
            testCollection.keys().then(function(keys){ 
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(3);
                done();
             });
        });

        it("foreach should callback for each file", function(done) {
            var callbackCount = 0;
            testCollection.forEach(function(key){
                expect(key.getName).to.exsit;
                expect(key.getName()).to.exsit;
                expect(key.getContent).to.exsit;
                callbackCount++;
            }).then(function(keys){
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(3);
                done();
            });
            
        });

        it("should be able to delete all the files in a collection", function(done) {
            testCollection.clear().then(function(){
                done();
            });
            
        });

        it("after clear, count should be zero", function(done) {
            testCollection.count().then(function(count){ 
                expect(count).to.exist;
                expect(count).to.equal(0);
                done();
             });
        });

        it("after clear, keys should be empty", function(done) {
            testCollection.keys().then(function(keys){ 
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(0);
                done();
             });
        });

        it("after clear, foreach should be empty", function(done) {
            testCollection.forEach(function(key){
                console.log("callback!", key);
                throw new Error("Collection should be empty, callback should not have been called.");
            }).then(function(keys){
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(0);
                done();
            });
            
        });

        it("after clear, should store a string", function(done) {
            testCollection.set("first.txt", "Hello this is a test").then(function(){
                done();
            },erCb("Could not write file"));
        });

        it("after clear, Should get a string", function(done) {
            testCollection.get("first.txt").then(function(str){
                expect(str).to.equal("Hello this is a test");
                done();
            },erCb("Could not read file"));
        });


    });
});

})();
