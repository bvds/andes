/**
*  A Key/Value store where the data is persisted to an encrypted store inside of Sencha Space instead of plain text.
  This class should not be created directly but instead should be obtained via Ext.space.SecureLocalStorage
*
*/
Ext.define('Ext.space.files.Collection', {
    /*
    * @private
    */
    constructor: function(name, loaded) {
        //add code to normalize name to sql table name limits
        this.name = name;
        this.loaded = loaded;
        this.createDir();
    },

     /**
    * @private
    */
    createDir: function() {
        var result = new Ext.Promise();
        var me = this;

        this.baseDir = result;

        this.loaded.then(function(fileSystem){
            fileSystem.getRoot().getDirectory({
                path: me.name,
                options: {
                    create: true
                },
                success: function(dir) {
                    console.log("my Dir", dir);
                    result.fulfill(dir);
                },
                failure: function(err) {
                    console.log("errr", err);
                    result.reject(err);
                }
            });
        });
        
        return result;
    },


    /**
    * Get the file contents for a key

        var files = Ext.space.SecureFile.get('secrets');

        files.get('myKey').then(function(contents){
            //do something with the content of the file.
        });

    * @param {String} key  The key to get a value for. 
    * @return {Ext.Promise} the promise that will resolve when the value is fetched.
    *
    */
    get: function(key){
        var me = this;

        var result = this._getFile(key, false).then(function(file) {
            console.log("my file", file);
            return me._getFileContent(file);
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
        var me = this;

        //Chain the promise returned by getFile to the promise returned by write file.
        var result = this._getFile(key, true).then(function(file) {
            console.log("my file", file);
            return me._writeFile(file, value);
        });

        return result;
    },

   /**
    * @private
    * get file descriptor.
    */
    _getFile: function(key, create){
        var result = new Ext.Promise();
        var me = this;
        this.baseDir.then(function(dir){
            dir.getFile({
                path: key,
                options: {
                    create: create
                },
                success: function(file) {
                    console.log("my file", file);
                    result.fulfill(file);
                },
                failure: function(err) {
                    console.log("errr", err);
                    result.reject(err);
                }
            });
        });       
        return result;
    },

    /**
    * @private
    * write file contents.
    */
    _writeFile: function(file, content){
        var result = new Ext.Promise();
        file.write({
                data: content,
                success: function() {
                    result.fulfill();
                },
                failure: function(err) {
                    result.reject(err);
                }
        });
        return result;
    },

    /**
    * @private
    * get file contents.
    */
    _getFileContent: function(file){
        var result = new Ext.Promise();
        file.read({
            success: function(content) {
                result.fulfill(content);
            },
            failure: function(err) {
                result.reject(err);
            }
        });
        return result;
    },


    /**
    * @private
    * list all of the files in a directory 
    */
    _listFiles: function(){
        var result = new Ext.Promise();
        var me = this;
        this.baseDir.then(function(dir){
            dir.readEntries({
                success: function(files) {
                    console.log("collection files", files);
                    result.fulfill(files);
                },
                failure: function(err) {
                    console.log("errr", err);
                    result.reject(err);
                }
            })
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
        this._getFile(key, false).then(function(file) {
            result.fulfill(true);
        }, function(err) {
            result.fulfill(false);
        });
        return result;
    },

    /**
    * Deletes the key if present in collection.

        var secrets = Ext.space.SecureLocalStore.get('secrets');

        secrets.delete('myKey').then(function(done){
           
        });

    * @param {String} key The key to delete
    * @return {Ext.Promise} the promise that will resolve when the value is checked.
    *
    */
    delete: function(key){
        var result = new Ext.Promise();
        

        this._getFile(key, false).then(function(file) {
            if(!file){
                result.reject("could not remove file, invalid file object returned.");
            }
            file.remove({
                success: function() {
                    result.fulfill(true);
                },
                failure: function(err) {
                    result.reject(err);
                }
            });
        }, function(err) {
            result.fulfill(true);
        });

        return result;
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
        var result = this._listFiles().then(function(files) {
            var fileNames = [];
            for(var i =0, l = files.length; i < l; i++){
                fileNames.push(files[i].getName());
            }
            return fileNames;
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
        var result = this._listFiles().then(function(files) {
            if(callback){
               for(var i =0, l = files.length; i < l; i++){
                    callback(files[i].getName());
                }  
            }
            return files;
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
        var result = this._listFiles().then(function(files) {
            if(!files) {
                return 0;
            }
            return files.length;
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
        var result = new Ext.Promise();
        var me = this;
        this.baseDir.then(function(dir){
            dir.removeRecursively({
                success: function(files) {
                    me.createDir().then(function(){
                        result.fulfill(true);
                    },function(err){
                        console.log("errr", err);
                        result.reject(err);
                    });
                },
                failure: function(err) {
                    console.log("errr", err);
                    result.reject(err);
                }
            })
        });     
        return result;
    }

});