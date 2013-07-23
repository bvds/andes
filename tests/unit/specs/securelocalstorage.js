(function() {

var testDatabase = null;

var testCollection;

describe("Secure Local Storage", function() {
    describe("Ext.space.SecureLocalStorage", function() {

    	it("Ext.space.SecureLocalStorage should exist", function(done) {
            expect(Ext.space.SecureLocalStorage).to.exist;
            expect(Ext.space.localstorage.Collection).to.exist;

            done();
        });



        it("Should be able to create collections", function(done) {
            testCollection = Ext.space.SecureLocalStorage.get('myCollection');

	        expect(testCollection).to.exist;

            testCollection.clear().then(function(){
                done();                
            }) 

        });

        it("Should set a key and value", function(done) {
           
            testCollection.set('myKey',{foo: "bar"}).then(function(){
	            done();
	        });

        })

        it("Should get a value for key", function(done) {
           
            testCollection.get('myKey').then(function(obj){
	            expect(obj).to.exist;
	            expect(obj.foo).to.exist;
	            expect(obj.foo).to.equal("bar");
	            done();
	        });

        })

        it("Should be able to check the presence of a key", function(done) {
           
            testCollection.has('myKey').then(function(hasKey){
	            expect(hasKey).to.exist;
	            expect(hasKey).to.equal(true);
	            done();
	        });

        })

        it("has(...) should return false when key not in collection", function(done) {
           
            testCollection.has('nokey').then(function(hasKey){
	            expect(hasKey).to.exist;
	            expect(hasKey).to.equal(false);
	            done();
	        });

        });

        it("Should be able to count the items in the collection", function(done) {
           
            testCollection.set('secondKey',{foo: "bar"}).then(function(){
                testCollection.count().then(function(count){
                    expect(count).to.equal(2);
                    done();
                });
            });

        })


        it("Should be able to get the keys of the collection", function(done) {
       
            testCollection.keys().then(function(keys){
                expect(keys.length).to.equal(2);
                keys.sort();
                expect(keys).to.eql(["myKey", "secondKey"]);
                done();
            });

        })


        it("Should be able get all the items in the collection", function(done) {
           var count = 0;
           var toSee = 2;

           var itr = function(key, value){ 
                count++; 
                if(key == "myKey" || key == "secondKey") { 
                    toSee--; 
                }
            };

            testCollection.forEach(itr).then(function(all){
                expect(count).to.equal(2);
                expect(toSee).to.equal(0);
                done();
            });

        });


        it("Should be able to have string values", function(done) {

            var check = function(){
                testCollection.get('primKey').then(function(val){
                    expect(val).to.equal("Hello");
                     done();    
                });
            } 

           testCollection.set('primKey', "Hello").then(check);

        })

        it("Should be able to have number values", function(done) {
            var check = function(){
                testCollection.get('primKey').then(function(val){
                    expect(val).to.equal(33333);
                    done();    
                });
            } 
           testCollection.set('primKey', 33333).then(check);
        });


        it("Should be able to have array values", function(done) {
            var check = function(){
                testCollection.get('arrayTest').then(function(val){
                    expect(val).to.eql(['a','b',{'c': 'd'}]);
                    done();    
                });
            } 
           testCollection.set('arrayTest', ['a','b',{'c': 'd'}]).then(check);
        });



        it("Should be able to delete an item", function(done) {
            testCollection.delete('myKey').then(function(deleted){
                expect(deleted).to.equal.true;
                testCollection.get('myKey').then(function(object){
                    expect(object).to.equal.undefined;
                    testCollection.has('myKey').then(function(hasKey){
                        expect(hasKey).to.be.false;
                        done();   
                    });
                });
            });
        })

        it("collections should be isolated", function(done) {
        
	       	//Set some data, create a different collection
	       	// check to make sure the data isn't there.

	        testCollection.set('baz',{foo: "blort", num:7}).then(function(){

	            var collection2 = Ext.space.SecureLocalStorage.get('secondCollection');
	            expect(collection2).to.exist;

	            collection2.has('baz').then(function(hasKey){
	            	expect(hasKey).to.be.false;
	                done();

	            });

	        });

        });



        it("Should be able to clear a collection", function(done) {
            testCollection.clear().then(function(){
                testCollection.count().then(function(count){
                    expect(count).to.equal(0);
                    done();
                });              
            });
        });






    });
});

})();
