dojo.provide("andes.timer");

dojo.declare("andes.timer", null, {
    // summary:
    //      This is created from the api when startTime is
    //      initialized.
    node: null,
    display:false,
    ready:false,
    constructor: function(st){
        this.startTime = st;
        dojo.addOnLoad(this, function(){
            this.node = dojo.byId("timer");
            this.ready = true;
            if(this.display) this.displayTimer(true);
        });
    },
    
    displayTimer: function(/*boolean*/t){
        this.display = t;
        if(!this.ready) return;
        
        var disp;
        if(t){
            disp = "block";
            this.interval = window.setInterval(dojo.hitch(this, function(){ this.updateTime(); }), 500);
        }else{
            disp = "none";
            if(this.interval) window.clearInterval(this.interval);
        }
        dojo.style(this.node, {"display": disp });
    },
    
    updateTime: function(){
        var elapsed = (new Date()).getTime()-this.startTime;
        if(this.node){
            this.node.innerHTML = Math.floor(elapsed/1000);
        }
    }
});
