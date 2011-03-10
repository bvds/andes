dojo.provide("andes.principles");
dojo.require("dijit.Tree");
dojo.require("dojo.data.ItemFileReadStore");

// Modify tree so that HTML is rendered in node labels. Bug #1603
// see http://bugs.dojotoolkit.org/ticket/12278
// Should remove switch when release uses new method.
// Also in review/principles-tree.html
if(dijit._TreeNode._meta.hidden.attributeMap){	// Old way
	dijit._TreeNode._meta.hidden.attributeMap.label.type="innerHTML";
}else if(dijit._TreeNode._meta.hidden._setLabelAttr){
	dijit._TreeNode._meta.hidden._setLabelAttr.type="innerHTML";
}else{
	console.error("Can't render HTML in tree.");
}

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
					var win = this.reviewp[file];
					// On IE 8, using this pointer to get properties gives "permission denied" errors.
					// var doc = this.reviewp[file].document;
					
					// On IE 8, child window event objects may not exist when window.open is returned.
					// window.document exists, but window.document.body may not.  
					
					function waitUntilLoaded(){
						if(!win || win.closed){
							// Should send message to server?
							console.log("Window closed.");
						}else if(win.document.readyState=='complete'){
							console.log("waiting got it  ....");
							pollChild();  // poll to test for child window focus.
							
							// Since window is already open, we can just scroll.
							if(section){
								var obj = win.document.getElementById(section);
								obj.scrollIntoView();
							}
						}else{
							console.log("waiting  ....",win.document.readyState);
							window.setTimeout(waitUntilLoaded,100);
						}
					}					

					// In IE 8, can't get events to reliably track window closing or minimization.
					// Instead, we poll child window.
					function pollChild(){
						if(!win || win.closed){ 
							// In IE 8, window object does not persist after closing.
							console.log("Closed window for ",title);
							andes.api.recordAction({type:"window", name: title, value: "blur"});
						}else{
							var f=win.document.hasFocus();
							if(!f && win._lastFocus){
								// Supply window as context so name is available.
								andes.drawing.onWindowBlur.call(win);
							}else if(f && !win._lastFocus){
								// Supply window as context so name is available.
								andes.drawing.onWindowFocus.call(win);
							}
							win._lastFocus=f;
							window.setTimeout(pollChild,100);
						}							
					}
					
					waitUntilLoaded();
					
				} else {
					if(section){
						// In principle, this could fail if window is already loaded
						// by the time the code gets to here.
						this.reviewp[file].onload = function (){
							var obj=this.document.getElementById(section); 
							obj.scrollIntoView();
						}
					}
					dojo.connect(this.reviewp[file], "onblur", andes.drawing.onWindowBlur);
					dojo.connect(this.reviewp[file], "onfocus", andes.drawing.onWindowFocus);
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
