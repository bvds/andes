
/**

Secure Local Storage is a key value store modeled around html5 localstoage. 

The key differences from localstrorage are:

 - Uses an Asynchronous api based on Ext.Promise
 - Each application can more than one named collection of keys or easier data storage
 - All data is encrypted before being persisted to disk
 - The storage limits for SecureLocalStorage are much higher than the 2-3mb allocated for localstorage.


        var secrets = Ext.space.SecureLocalStore.get('secrets');

        secrets.set('myKey',object).then(function(){
            //do something when done.
        });

        secrets.get('myKey').then(function(object){
            var a = object.field;
        });

        secrets.delete().then(function(isDeleted){
            // done.
        });

        secrets.has(key).then(function(hasKey){
            
        });

        secrets.forEach(function(key, value){}).then(function(){
            // done.
        });

        secrets.count().then(function(numberOfItems){
            
        });

        secrets.clear().then(function(){
            // done.
        });
    
*/
Ext.define('Ext.space.SecureLocalStorage', {
    singleton: true,


    /*
    * @private
    */
    constructor: function() {
        this.loaded = new Ext.Promise();

    },


    /**
    * Get a collection of name. Collections are automatically created if they do not exist.
    *
    * @param {String} collectionName The name of the collection to get.
    * @return {Ext.space.localstorage.Collection} the secure collection.
    *
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


        Ext.onSpaceReady().then(function(){
            
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


        });

        return loaded;
    },


});
