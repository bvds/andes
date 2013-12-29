define([
    "andes/startup",
    "andes/drawing"
],function(andes,drawing){

    var self = this;
    // From pre-AMD version.  Not sure why this form was used?
    // var andes = window.opener.andes;
    this.onfocus = function(){
        //console.log("Window focus, title: ",self.name);
        drawing.onWindowFocus.call(self);
    };
    
    this.document.onfocusout = function(){
        //console.log("Window blur, title: ",self.name);
        if (this._activeElement != document.activeElement){
            this._activeElement = document.activeElement;
        }else{
            drawing.onWindowBlur.call(self);
        }    
    };
    
    this.onunload = function(){
        //console.log("Window unload, title: ",self.name);
        drawing.onWindowBlur.call(self);
        //andes.api.recordAction({type:"window", name: "IntroVideo", value: "blur"});
    };
});
