Ext.define('Ext.space.SecureLocalStorage', {
    singleton: true,


    /*
    * @private
    */
    constructor: function() {
        this.loaded = new Ext.Promise();

    },


    /**
    * Get the 
    */
    get: function(name){

        this.load();

        var collection = new Ext.space.localstorage.Collection(name, this.loaded);

        return collection;

    },


    /**
    * @private
    */
    load: function() {
        var loaded = this.loaded,
        me = this;



        var name = this.name;

        if(this.db){
           return;
        }


        Ext.onSpaceReady(function(){
            
            me.db = Ext.space.Sqlite.openDatabase({
                    name: 'sencha_secure_local_store',
                    version: '1',
                    displayName: 'Secure Local Storage',
                    estimatedSize: 5 * 1024 * 1024
            });

            
            me.db.transaction({
                callback: function(transaction) {
                       
                        transaction.executeSql({
                            sqlStatement: "CREATE TABLE IF NOT EXISTS item (collection TEXT, name TEXT, value TEXT, PRIMARY KEY (collection, name))",
                            arguments: [],
                            callback: function(tx, results) {
                                loaded.fulfill(me.db);
                            },
                            failure: function(tx, err) {
                                loaded.reject(err);
                            }
                        });


                        transaction.executeSql({
                            sqlStatement: "CREATE INDEX IF NOT EXISTS name_idx on item (name)",
                            arguments: [],
                            callback: function(tx, results) {
                                console.log("index on name column created");
                            },
                            failure: function(tx, err) {
                                 console.error("index on name column could not be created", err);
                            }
                        });

                        transaction.executeSql({
                            sqlStatement: "CREATE INDEX IF NOT EXISTS collection_idx on item (collection)",
                            arguments: [],
                            callback: function(tx, results) {
                                console.log("index on collection column created");
                            },
                            failure: function(tx, err) {
                                 console.error("index on collection column could not be created", err);
                            }
                        });

                        console.log("create table transaction end");
                },
                failure: function(err) {
                    console.log("Could not create Table", arguments);
                    loaded.reject(err);
                }
            });


        })

        return loaded;
    },


});