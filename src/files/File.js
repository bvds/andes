

Ext.define('Ext.space.files.File', {
    /*
    * @private
    */
    constructor: function(name, collection) {
       this.name = name;
       this.collection = collection;
    },

    /**
    * Returns the name/key of the file.
    */
    getName: function() {
    	return this.name;
    },

    /**
    * Fetch the contents of the file.

    	file.get().then(function(contents){
    		//do something with the contents.
    	});

    * @return {Ext.Promise} the promise that will resolve when the contents are fetched.
    *
    *@alias Ext.space.Collection.get
    */
    getContents: function() {
    	return this.collection.get(this.name);
    }

});