dojo.provide("andes.principles");
dojo.require("dijit.Tree");
dojo.require("dojo.data.ItemFileReadStore");

// Modify tree so that HTML is rendered in node labels
// See Bug #1603
dijit._TreeNode._meta.hidden.attributeMap.label.type="innerHTML";

// The principles and other review pages can be opened either 
// via the menus or via the help system (links in the Tutor pane).
andes.principles={
	
	// Either constants or Equations.
	reviewp: [],
	review: function(file,title,section,dimensionString){
		if(!this.reviewp[title] || this.reviewp[title].closed){
			var dims = dimensionString?dimensionString+",scrollbars=no":"width=350,height=450,scrollbars=yes";
			this.reviewp[title]=window.open("../review/"+file,
							title,
							dims+",directories=no,menubar=no,toolbar=no,location=no,status=no"
						       );
			if(this.reviewp[title]){
				// Does not work for IE.
				dojo.connect(this.reviewp[title], "onblur", andes.drawing.onWindowBlur);
				dojo.connect(this.reviewp[title], "onfocus", andes.drawing.onWindowFocus);
			}
		}
		if(this.reviewp[title]){
			this.reviewp[title].focus();
			if(section){
				var obj = this.reviewp[title].document.getElementById(section);
				obj.scrollIntoView();
			}
		}
	},
	
	extp: null,
	externP: function (){
		if(!this.extp || this.extp.closed){
			// See https://developer.mozilla.org/en/DOM/window.open
			this.extp=window.open("../review/principles-tree.html","Principles",
					      // Default starting size in Firefox is too big, need to set explicitly.
					      // Need scrollbars=1 in Firefox for reopen with long (opened) tree.
					      // status=0 ignored by Firefox.
					      "width=350,height=450,scrollbars=yes,directories=no,menubar=no,toolbar=no,location=no,status=no"
					     );
			if(this.extp){
				// Does not work for IE.
				dojo.connect(this.extp, "onblur", andes.drawing.onWindowBlur);
				dojo.connect(this.extp, "onfocus", andes.drawing.onWindowFocus);
			}else{
				// If window creation fails, open a Modal dialog.
				// Delete any text leftover from old hints.
   				dojo.byId("allModalTreeText").innerHTML = "";
				dijit.byId("allPrinciples").show();
			}
		}
		if(this.extp){
			this.extp.focus();
		}
	}
}

// This should be loaded after everything else, in the background
dojo.addOnLoad(function() {
	
	var principlesStore = new dojo.data.ItemFileReadStore({
		url: "../review/principles.json"
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
		showRoot: false,
		onClick: function(item,node) {
			var psm=principlesStore.getValue(item,"psm");
			// if student clicks on a group, there is no psm.
			if(psm){
				andes.help.echo(principlesStore.getValue(item,"label"));
				andes.help.principles(psm);
				dijit.byId("majorPrinciples").hide();
			}
		}
	},"majorModalTree");
	
	new dijit.Tree({
		model: allPrinciplesModel,
		showRoot: false,
		onClick: function(item,node) {
			var psm=principlesStore.getValue(item,"psm");
			// if student clicks on a group, there is no psm.
			if(psm){
				andes.help.echo(principlesStore.getValue(item,"label"));
				andes.help.principles(psm);
				// This is a bit ugly, but close both possible windows:
				dijit.byId("allPrinciples").hide();
			}
		}
	},"allModalTree");
	
});
