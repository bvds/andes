dojo.provide("andes.principles");
dojo.require("dijit.Tree");
dojo.require("dojo.data.ItemFileReadStore");
dojo.require("dojo.data.ItemFileReadStore");

andes.principles={
  extp: null,
  externP: function (){
    if(!this.extp || this.extp.closed){
      this.extp=window.open("/review/principles-tree.html",
      "Principles",
      "directories=0,menubar=0,status=0,toolbar=0,location=0,scrollbars=1,resizable=1"
		    );
      // If this fails, open a Modal dialog.
      if(!this.extp) {
	console.log("window.open failed");
	dijit.byId("allPrinciples").show();
      }
    }
    this.extp && this.extp.focus();
  }
}  

// This should be loaded after everything else, in the background
dojo.addOnLoad(function() {
  var principlesStore = new dojo.data.ItemFileReadStore({
    url: "/review/principles.json",
  });

  var majorPrinciplesModel = new dijit.tree.ForestStoreModel({
    store: principlesStore,
    query: {"complexity": "major"},
            rootLabel: "Major Principles",
            childrenAttrs: ["items"]
        });

        var allPrinciplesModel = new dijit.tree.ForestStoreModel({
            store: principlesStore,
            rootLabel: "All Principles",
            childrenAttrs: ["items"]
        });

        new dijit.Tree({
			 model: majorPrinciplesModel,
			 showRoot: false
        },"majorModalTree");

new dijit.Tree({
		 model: allPrinciplesModel,
		 showRoot: false
        },"allModalTree");
});
