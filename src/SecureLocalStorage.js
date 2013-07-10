Ext.define('Ext.space.SecureLocalStorage', {
    singleton: true,


    /*
    * @private
    */
    constructor: function() {

    },


    /**
    * Get the 
    */
    get: function(name){

        var collection = new Ext.space.localstorage.Collection(name);

        return collection;

    }

});