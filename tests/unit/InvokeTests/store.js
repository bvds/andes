var store = Ext.space.Storage.get('myCollection');

collection.set('myKey',object).then(function(){
//Called when data is saved.
});


collection.get('myKey').then(function(object){

});



collection.has('myKey').then(function(hasObject){

});


collection.delete('myKey').then(function(object){

});


collection.forEach(function(object){
	//called with each object

}).then(function(){
	//all done
});

collection.keys().then(function(array){
	// array of all items.
});

collection.all().then(function(array){
	// array of all items.
});


collection.clear().then(function(object){

});