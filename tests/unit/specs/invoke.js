(function() {

describe("Invoke", function() {
    describe("Ext.space.Invoke", function() {
        it("Ext.space.Invoke should exist", function(done) {
            expect(Ext.space.Invoke).to.exist;
            done();
        });


         it("Should be able to invoke another app in forground", function(done) {

              var success = function(message) {
                  console.log("message", arguments);
                  expect(message).to.exist;
                  expect(message).to.eql({result: "success", data :{str:"b", num: 7, arry: ['a','b',1,{a:'b'}]}});
                  done();
              };

              var failure = function(error) {
                  throw error;
                  done();

                  //throw error;
                  //log('Received error: ' + JSON.stringify(error, null, 2));
              }

              var send = function(connection) {
                  connection.send({}, true).then(
                      success,
                      failure
                  );
              };
            

             Ext.space.Invoke.get('apiTests').then(send, failure);
            
        });


        it("Should be able to invoke another app in background", function(done) {

              var success = function(message) {
                  console.log("message", arguments);
                  expect(message).to.exist;
                  expect(message).to.eql({result: "success", data :{str:"b", num: 7, arry: ['a','b',1,{a:'b'}]}});
                  done();
              };

              var failure = function(error) {
                  throw error;
                  done();

                  //throw error;
                  //log('Received error: ' + JSON.stringify(error, null, 2));
              }

              var send = function(connection) {
                  connection.send({}, false).then(
                      success,
                      failure
                  );
              };
            

             Ext.space.Invoke.get('apiTests').then(send, failure);
            
        });
    });
});

})();
