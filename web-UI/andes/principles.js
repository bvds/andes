dojo.provide("andes.principles");
dojo.require("dijit.Tree");
dojo.require("dojo.data.ItemFileReadStore");
dojo.require("dojo.data.ItemFileReadStore");

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
