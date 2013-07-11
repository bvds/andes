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
    
        this.loaded.then(function(db){
            console.log("I have a db!!", db);
            db.transaction({
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
                                console.log("Could not execute query", query, params, arguments);
                                rs.reject(err);
                            }
                        });

                        console.log("create table transaction end");
                },
                failure: function(err) {
                    console.log("Could not start Transaction", arguments);
                    rs.reject(err);
                }
            });

        });
        return rs;

    },



    /**
    * Get the value for a key
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
    * Get the 
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
    * Get the 
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
    * Get the 
    */
    delete: function(key){
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
    *
    */
    count: function() {
        var result = new Ext.Promise();
        this.query("select count(*) from item where collection = ?", [this.name]).then(function(rs){
            console.log("value ", rs.rows.rows[0][0]);
            result.fulfill(rs.rows.rows[0][0]);
        });
        return result;
    },



    clear: function(){
        return this.query("DELETE FROM item where collection = ?", [this.name]);
    }

});