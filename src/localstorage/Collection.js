Ext.define('Ext.space.localstorage.Collection', {
    /*
    * @private
    */
    constructor: function(name) {
        //add code to normalize name to sql table name limits
        this.name = name;
        this.cache = {};

        this.loaded = this.load();
    },

    /**
    *@private
    */
    load: function() {
        var loaded = new Ext.Promise(),
        me = this;

        var name = this.name;
        console.log("name?", name, this.name, this);
        Ext.onSpaceReady(function(){
            console.log("Collection.load onspaceready");

            me.db = Ext.space.Sqlite.openDatabase({
                    name: 'sencha_secure_local_store',
                    version: '1',
                    displayName: 'Secure Local Storage',
                    estimatedSize: 5 * 1024 * 1024
            });

            
            me.db.transaction({
                callback: function(transaction) {
                        console.log("create table transaction begin");

                        transaction.executeSql({
                            sqlStatement: "CREATE TABLE IF NOT EXISTS "+ name +" (name TEXT UNIQUE, value TEXT)",
                            arguments: [],
                            callback: function(tx, results) {
                                loaded.fulfill(me.db);
                            },
                            failure: function(tx, err) {
                                console.log("error callback", arguments);
                                loaded.reject(err);
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

    query: function(query, params) {
        var rs = new Ext.Promise(),
        me = this;


        me.loaded.then(function(db){
            console.log("I have a db!!", db, me.db);
            me.db.transaction({
                callback: function(transaction) {
                        console.log("create table transaction begin");

                        transaction.executeSql({
                            sqlStatement: query,
                            arguments: params,
                            callback: function(tx, results) {
                                console.log("query complete ", tx, results);

                                rs.fulfill(results);

                            },
                            failure: function(tx, err) {
                                console.log("error callback", arguments);
                                loaded.reject(err);
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
        return rs;

    },



    /**
    * Get the value for a key
    */
    get: function(key){
       var result = new Ext.Promise(),
       me = this;

        if(this.cache[key]){
            result.fulfill(this.cache[key]);
        } else {
           this.query("select value from "+this.name+" where name = ?", [key]).then(function(rs){
                console.log("value ", rs.rows.rows.length);
                if(rs.rows.rows.length > 0){
                    result.fulfill(JSON.parse(rs.rows.rows[0][0]));
                } else {
                    result.fulfill(undefined);
                }
           });
        }
        return result;

    },



    /**
    * Get the 
    */
    set: function(key, value){
        var result = new Ext.Promise(),
        me = this;

        this.query("INSERT OR REPLACE into "+this.name+" values(?,?)", [key, JSON.stringify(value)]).then(function(rs){
                if(rs.value){
                    result.fulfill(rs.value);
                } else {
                    result.fulfill(undefined);
                }
        });
        return result;
    },

    /**
    * Get the 
    */
    has: function(key){

        var result = new Ext.Promise(),
        me = this;

        this.query("select count(*) from "+this.name+" where name = ?", [key]).then(function(rs){
            console.log("value ",rs.rows.rows.length );
            if(rs.rows.rows.length > 0){
                result.fulfill(true);
            } else {
                result.fulfill(false);
            }
        });

        return result;

    },

    /**
    * Get the 
    */
    delete: function(key){
        var result = new Ext.Promise();
        this.query("delete from "+this.name+" where name = ?", [key]).then(function(rs){
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
    *
    */
    keys: function() {
        var result = new Ext.Promise();
        this.query("select name from "+this.name, [key]).then(function(rs){
            var results = [];
            for(var i =0, l = rs.rows.rows.length; i < l; i++ ){
                results.push(rs.rows.rows[i][0]);
            }
            result.fulfill(results);
        });
        return result;
    },

    /**
    *
    */
    forEach: function(callback) {
        var result = new Ext.Promise();
        this.query("select name, value from "+this.name, []).then(function(rs){
            for(var i =0, l = rs.rows.rows.length; i < l; i++ ){
                callback(rs.rows.rows[i][0], JSON.parse(rs.rows.rows[i][1]));
            }
            result.fulfill();
        });
        return result;
    },

        /**
    *
    */
    count: function() {
        var result = new Ext.Promise();
        this.query("select count(*) from "+this.name, []).then(function(rs){
            console.log("value ", rs.rows.rows[0][0]);
            result.fulfill(rs.rows.rows[0][0]);
        });
        return result;
    },



    clear: function(){
        return this.query("DELETE FROM TABLE "+ this.name, []);
    },

    destroy: function() {
        return this.query("DROP TABLE "+ this.name, []);
    }

});