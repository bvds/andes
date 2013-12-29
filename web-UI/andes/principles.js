define([
    "andes/principles",
    "dojo/on",
    "dojo/ready",
    "andes/drawing",
	"dijit/Tree",
	"dojo/data/ItemFileReadStore"
],function(andes,on,ready,drawing){

// See review/principles-tree.html
  ready(function(){  // wait until dom is loaded
	  console.info("andes/principles.js:  wire up principles tree");
  if(dijit._TreeNode._meta.hidden.attributeMap){   // old way
    // See Bug #1949
    dijit._TreeNode._meta.hidden.attributeMap.label.type="innerHTML";
    console.warn("old way for html render, version ",dojo.version.toString());
  }else if(dijit._TreeNode._meta.hidden._setLabelAttr){
        dijit._TreeNode._meta.hidden._setLabelAttr.type="innerHTML";
  }else{
          console.error("Can't render HTML in tree. Version ",dojo.version.toString());
  }
});


// The principles and other review pages can be opened either 
// via the menus or via the help system (links in the Tutor pane).
andes.principles={
	reviewp: [],	
	review: function(file,title,section,dimensionString){
		if(!this.reviewp[file] || this.reviewp[file].closed){
			// console.log('New window "'+file+'"');
			var dims = dimensionString?dimensionString+",scrollbars=no":"width=350,height=450,scrollbars=yes";
			// Internet Explorer gives error if window.open arguments contain spaces.
			// see http://developer.mozilla.org/En/DOM/Window.open
			if(title.match(' ')){
				console.error("window.open title with space:  ",title);
				title=title.replace(/ /g,'_'); // Logging name is modified.
			}
			this.reviewp[file]=window.open("../review/"+file,
						title,
						dims+",directories=no,menubar=no,toolbar=no,location=no,status=no"
			);
			if(this.reviewp[file]){
				if(dojo.isIE){
					// console.log("==========================>>>We've got IE here");
					var body, win = this.reviewp[file];
					function childLoaded(){
						body = win.document.getElementsByTagName("body");
						if(body[0]==null){
							// Not loaded yet, try again
							setTimeout(childLoaded, 20);
						}else{
							var n = win.document.createElement("script");
							n.src = "../web-UI/andes/recordIE.js";
							body[0].appendChild(n);
							// console.log("Completed operation inserting: ",n);
						}
					}
					childLoaded();
				} else {
					if(section){
						// In principle, this could fail if window is already loaded
						// by the time the code gets to here.
						this.reviewp[file].onload = function(){
							var obj=this.document.getElementById(section); 
							obj.scrollIntoView();
						};
					}
					on(this.reviewp[file], "onblur", drawing.onWindowBlur);
					on(this.reviewp[file], "onfocus", drawing.onWindowFocus);
				}
					
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
};

// This should be loaded after everything else, in the background
ready(function() {
	console.info("andes/principles.js: finish wiring principles tree.");
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
});
