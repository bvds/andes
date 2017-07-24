/* global define */
define([
    "dojo/_base/declare",
    "dojo/data/ItemFileReadStore",
    "dojo/dom",
    "dojo/on",
    "dojo/ready",
    "dojo/sniff",
    "dijit/registry",
    "dijit/Tree",
    "dijit/tree/ForestStoreModel",
    "dojo/domReady!"
],function(declare, ItemFileReadStore, dom, on, ready, has, registry, Tree, ForestStoreModel){

    // The principles and other review pages can be opened either 
    // via the menus or via the help system (links in the Tutor pane).
    window.andes.principles={
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
                    if(has("ie")){
                        // console.log("==========================>>>We've got IE here");
                        var body, win = this.reviewp[file];
                        var childLoaded = function (){
                            body = window.document.getElementsByTagName("body");
                            if(body[0]==null){
                                // Not loaded yet, try again
                                window.setTimeout(childLoaded, 20);
                            }else{
                                var n = win.document.createElement("script");
                                n.src = "../web-UI/andes/recordIE.js";
                                body[0].appendChild(n);
                                // console.log("Completed operation inserting: ",n);
                            }
                        };
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
                        on(this.reviewp[file], "onblur", window.andes.drawing.onWindowBlur);
                        on(this.reviewp[file], "onfocus", window.andes.drawing.onWindowFocus);
                    }

                }else if(title=="Principles"){
                    // If principles window creation has failed, open a Modal dialog.
                    // Delete any text leftover from old hints.
                    dom.byId("allModalTreeText").innerHTML = "";
                    registry.byId("allPrinciples").show();
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
        var principlesStore = new ItemFileReadStore({
            url: "../review/principles.json"
        });

        var majorPrinciplesModel = new ForestStoreModel({
            store: principlesStore,
            labelType: "html",
            query: {"complexity": "major"},
            rootLabel: "Major Principles",
            childrenAttrs: ["items"]
        });

        var allPrinciplesModel = new ForestStoreModel({
            store: principlesStore,
            labelType: "html",
            rootLabel: "All Principles",
            childrenAttrs: ["items"]
        });

        new Tree({
            model: majorPrinciplesModel,
            showRoot: false,
            onClick: function(item, node) {
                var psm=principlesStore.getValue(item,"psm");
                // if student clicks on a group, there is no psm.
                if(psm){
                    window.andes.help.echo(principlesStore.getValue(item,"label"));
                    window.andes.help.principles(psm);
                    registry.byId("majorPrinciples").hide();
                }
            }
        }, "majorModalTree");

        new Tree({
            model: allPrinciplesModel,
            showRoot: false,
            onClick: function(item,node) {
                var psm=principlesStore.getValue(item,"psm");
                // if student clicks on a group, there is no psm.
                if(psm){
                    window.andes.help.echo(principlesStore.getValue(item,"label"));
                    window.andes.help.principles(psm);
                    // This is a bit ugly, but close both possible windows:
                    registry.byId("allPrinciples").hide();
                }
            }
        },"allModalTree");

    });
});
