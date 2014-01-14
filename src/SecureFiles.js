/*
 * Key/Value store for files. Files stored using this API are encrypted automatically 
 * using Sencha Space's security infrastructure. 
 *
 *        var files = Ext.space.SecureFile.get('secrets');
 *
 *        files.get('myKey').then(function(contents){
 *            //do something with the content of the file.
 *        });
 *
 * files is an instance of Ext.space.files.Collection. See Ext.space.files.Collection for 
 * a complete list of file operations.
 * 
 * @aside guide secure_file_api
 *  
 */
Ext.define('Ext.space.SecureFiles', {
    singleton: true,


    /*
    * @private
    */
    constructor: function() {
        this.loaded = new Ext.Promise();

    },


    /**
    * Get a collection by name. Collections are automatically created if they do not exist.
    *
    * @param {String} collectionName The name of the collection to get.
    * @return {Ext.space.localstorage.Collection} the secure collection.
    *
    */
    get: function(name){

        this.load();

        var collection = new Ext.space.files.Collection(name, this.loaded);

        return collection;

    },

    /**
    * @private
    */
    load: function() {
        var loaded = this.loaded,
        me = this;

        if(me.filesystem) {
            return;
        }

        Ext.onSpaceReady(function(){
            
            Ext.space.FileSystem.requestFileSystem({
                success: function(filesystem) {
                    console.log("Got filesystem", arguments);
                    me.filesystem = filesystem;
                    loaded.fulfill(me.filesystem);
                },
                failure: function(err) {
                    console.log("could not fetch file system", arguments);
                    loaded.reject(err);
                }
            });
        })

        return loaded;
    },

});
