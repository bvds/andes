dojo.provide("andes.principles");
dojo.require("dijit.Tree");
dojo.require("dojo.data.ItemFileReadStore");

// Modify tree so that HTML is rendered in node labels
// See Bug #1603
dijit._TreeNode._meta.hidden.attributeMap.label.type="innerHTML";

// The principles and other review pages can be opened either 
// via the menus or via the help system (links in the Tutor pane).
andes.principles={
	reviewp: [],	
	review: function(file,title,section,dimensionString){
		if(!this.reviewp[file] || this.reviewp[file].closed){
			// console.log('New window "'+file+'"');
			var dims = dimensionString?dimensionString+",scrollbars=no":"width=350,height=450,scrollbars=yes";
			this.reviewp[file]=window.open("../review/"+file,
						       title,
						       dims+",directories=no,menubar=no,toolbar=no,location=no,status=no"
						      );
			if(this.reviewp[file]){
				if(section){
					// In principle, this could fail if window is already loaded
					// by the time the code gets to here.
					this.reviewp[file].onload = function (){
						var obj=this.document.getElementById(section); 
						obj.scrollIntoView();
					}
				}
				// Does not work for IE.
				dojo.connect(this.reviewp[file], "onblur", andes.drawing.onWindowBlur);
				dojo.connect(this.reviewp[file], "onfocus", andes.drawing.onWindowFocus);
			}else if(title=="Principles"){
				// If principles window creation has failed, open a Modal dialog.
				// Delete any text leftover from old hints.
				dojo.byId("allModalTreeText").innerHTML = "";
				dijit.byId("allPrinciples").show();
			}
		}else{
			// Window already open
			this.reviewp[file].focus();
			if(section){
				var obj = this.reviewp[file].document.getElementById(section);
				obj.scrollIntoView();
			}
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
