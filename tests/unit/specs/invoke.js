(function() {

describe("Invoke", function() {
    describe("Ext.space.invoke.Proxy", function() {
        it("Ext.space.invoke.Proxy should exist", function(done) {
            expect(Ext.space.invoke.Proxy).to.exist;
            done();
        });

        it("Proxy methods should be functions that return promises", function(done) {

            /*var Test = {

                first: function(){


                },

                second: function(){


                },

                third: function(){


                }
            }*/

            /**
            * Mock connection object.
            */
            var connection = {send: function(message, foreground){
              console.log("send", message, foreground);
              expect(message).to.exist;
              expect(message["$obj"]).to.exist;
              expect(message["$obj"]).to.eql('Test');
              return new Ext.Promise()

            }};

            var proxy = new Ext.space.invoke.Proxy(connection,'Test',['first', 'second', 'third']);

            expect(proxy.first).to.exist;
            expect(proxy.first()).to.be.an.instanceof(Ext.Promise);
            
            expect(proxy.second).to.exist;
            expect(proxy.second()).to.be.an.instanceof(Ext.Promise);
            
            expect(proxy.third).to.exist;
            expect(proxy.third()).to.be.an.instanceof(Ext.Promise);
            
            done();
        });
        
        it("Should be able to get a proxy from an invoked app", function(done) {
            Ext.space.Invoke.get('apiTests').then(function(app){
              app.get('Test').then(function(test){
                test.first({a:'b'}, false).then(function(){
                  done();
                });
              });
            });
            
        });

         


    });

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
