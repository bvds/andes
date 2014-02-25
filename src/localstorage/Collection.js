/**
*  A Key/Value store where the data is persisted to an encrypted store inside of Sencha Space instead of plain text.
  This class should not be created directly but instead should be obtained via Ext.space.SecureLocalStorage
*
*/
Ext.define('Ext.space.localstorage.Collection', {
    /*
    * @private
    */
    constructor: function(name, loaded) {
        //add code to normalize name to sql table name limits
        this.name = name;
        this.loaded = loaded;

    },

    /**
    * @private
    */
    query: function(query, params) {
        var rs = new Ext.Promise();

        this.loaded.then(function(db) {
            return db.transaction().then(function(transaction) {
                transaction.executeSql(query, params).then(function(results) {
                    rs.fulfill(results);
                });

                return transaction.run();
            });
        }).error(function(e) {
            rs.reject(e);
        });

        return rs;
    },



    /**
    * Get the value for a key

        var secrets = Ext.space.SecureLocalStore.get('secrets');

        secrets.get('myKey').then(function(object){
            var a = object.field;
        });

    * @param {String} key  The key to get a value for.
    * @return {Ext.Promise} the promise that will resolve when the value is fetched.
    *
    */
    get: function(key){
       var result = new Ext.Promise();
        this.query("select value from item where collection = ? and name = ?", [this.name, key]).then(function(rs){
            console.log("value ", rs.rows.rows.length);
            if(rs.rows.rows.length > 0){
                result.fulfill(JSON.parse(rs.rows.rows[0][0]));
            } else {
                result.fulfill(undefined);
            }
        });
        return result;
    },



    /**
    * Get the value for a key

        var secrets = Ext.space.SecureLocalStore.get('secrets');

        secrets.set('myKey',object).then(function(){
            //do something when done.
        });



    * @param {String} key  The key to store the value at.
    * @param {Object} value The JSON object to store.
    * @return {Ext.Promise} the promise that will resolve when the value is stored.
    *
    */
    set: function(key, value){
        var result = new Ext.Promise();

        this.query("INSERT OR REPLACE into item values(?,?,?)", [this.name, key, JSON.stringify(value)]).then(function(rs){
                if(rs.value){
                    result.fulfill(rs.value);
                } else {
                    result.fulfill(undefined);
                }
        });
        return result;
    },

    /**
    * Checks to see if key is present in collection without fetching and de-serializing the value.

        var secrets = Ext.space.SecureLocalStore.get('secrets');

        secrets.has('myKey').then(function(hasKey){

        });

    * @param {String} key  The key to get a value for.
    * @return {Ext.Promise} the promise that will resolve when the value is checked.
    *
    */
    has: function(key){

        var result = new Ext.Promise();

        this.query("select count(*) from item where collection = ? and name = ?", [this.name, key]).then(function(rs){
            console.log("value ",rs.rows.rows.length );
            if(rs.rows.rows[0][0] > 0){
                result.fulfill(true);
            } else {
                result.fulfill(false);
            }
        });

        return result;

    },

    /**
    * Deletes the key if present in collection.

        var secrets = Ext.space.SecureLocalStore.get('secrets');

        secrets.remove('myKey').then(function(done){

        });

    * @param {String} key The key to delete
    * @return {Ext.Promise} the promise that will resolve when the value is checked.
    *
    */
    remove: function(key){
        var result = new Ext.Promise();
        this.query("delete from item where collection = ? and name = ?", [this.name, key]).then(function(rs){
            console.log("value ", rs.rowsAffected);
            if(rs.rowsAffected > 0){
                result.fulfill(true);
            } else {
                result.fulfill(false);
            }
        });
        return result;
    },

    /**
     * Alias for .remove()
     *
     * @private
     * @return {Ext.Promise} the promise that will resolve when the value is checked
     */
    delete: function(key) {
        return this.remove.apply(this, arguments);
    },


    /**
    * Gets an array of all the keys in a collection

        var secrets = Ext.space.SecureLocalStore.get('secrets');

        secrets.keys().then(function(keys){
           console.log(keys.length);
        });

    * @return {Ext.Promise} the promise that will resolve when all of the keys have been collected.
    *
    */
    keys: function() {
        var result = new Ext.Promise();
        this.query("select name from item where collection = ?", [this.name]).then(function(rs){
            var results = [];
            for(var i =0, l = rs.rows.rows.length; i < l; i++ ){
                results.push(rs.rows.rows[i][0]);
            }
            result.fulfill(results);
        });
        return result;
    },

    /**
    * Iterates over all the items in a collection

        var secrets = Ext.space.SecureLocalStore.get('secrets');

         secrets.forEach(function(key, value){}).then(function(){
            // done.
        });


    * @param {function}  callback this function will be called once for each item in the collection.
    * @return {Ext.Promise} the promise that will resolve when all of the itmes have been iterated.
    *
    */
    forEach: function(callback) {
        var result = new Ext.Promise();
        this.query("select name, value from item where collection = ?", [this.name]).then(function(rs){
            for(var i =0, l = rs.rows.rows.length; i < l; i++ ){
                callback(rs.rows.rows[i][0], JSON.parse(rs.rows.rows[i][1]));
            }
            result.fulfill();
        });
        return result;
    },

    /**
    * Returns a count of the total number of items in the collection

        var secrets = Ext.space.SecureLocalStore.get('secrets');

         secrets.count().then(function(count){
            // done.
        });

    * @return {Ext.Promise} the promise that will resolve with a the number of items in the collection.
    *
    */
    count: function() {
        var result = new Ext.Promise();
        this.query("select count(*) from item where collection = ?", [this.name]).then(function(rs){
            console.log("value ", rs.rows.rows[0][0]);
            result.fulfill(parseInt(rs.rows.rows[0][0]));
        });
        return result;
    },

    /**
    * Deletes all of the items in a collection.

        var secrets = Ext.space.SecureLocalStore.get('secrets');

         secrets.clear().then(function(){
            // done.
        });

    * @return {Ext.Promise} the promise that will resolve with a the number of items in the collection.
    *
    */
    clear: function(){
        return this.query("DELETE FROM item where collection = ?", [this.name]);
    }
});
