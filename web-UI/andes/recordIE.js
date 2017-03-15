(function(){
    var self = this;
    var andes = window.opener.andes;
    this.onfocus = function(){
        //console.log("Window focus, title: ",self.name);
        window.andes.drawing.onWindowFocus.call(self);
    };
    
    this.document.onfocusout = function(){
        //console.log("Window blur, title: ",self.name);
        if (this._activeElement != document.activeElement){
            this._activeElement = document.activeElement;
        }else{
            window.andes.drawing.onWindowBlur.call(self);
        }    
    };
    
    this.onunload = function(){
        //console.log("Window unload, title: ",self.name);
        window.andes.drawing.onWindowBlur.call(self);
        //window.andes.api.recordAction({type:"window", name: "IntroVideo", value: "blur"});
    };
})();
