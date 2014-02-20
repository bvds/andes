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

        secrets.remove().then(function(isDeleted){
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
    get: function(name) {
        this.load();
        return new Ext.space.localstorage.Collection(name, this.loaded);
    },

    /**
    * @private
    */
    load: function() {
        var me = this,
            loaded = me.loaded;

        if (me.db) {
           return;
        }

        Ext.onSpaceReady().then(function() {
            return Ext.space.Sqlite.openDatabase({
                name: 'sencha_secure_local_store',
                version: '1',
                displayName: 'Secure Local Storage',
                estimatedSize: 5 * 1024 * 1024
            }).then(function(db) {
                me.db = db;

                return db.transaction().then(function(transaction) {
                    transaction.executeSql("CREATE TABLE IF NOT EXISTS item (collection TEXT, name TEXT, value TEXT, PRIMARY KEY (collection, name))");
                    transaction.executeSql("CREATE INDEX IF NOT EXISTS name_idx on item (name)");
                    transaction.executeSql("CREATE INDEX IF NOT EXISTS collection_idx on item (collection)");

                    return transaction.run().then(function() {
                        loaded.fulfill(me.db);
                    });
                });
            });
        }).error(function(e) {
            loaded.reject(e);
        });

        return loaded;
    }
});
