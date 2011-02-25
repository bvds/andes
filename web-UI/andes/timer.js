dojo.provide("andes.timer");

dojo.declare("andes.timer", null, {
    // summary:
    //      This is created from the api when startTime is
    //      initialized.
    node: null,
    display:false,
    constructor: function(st){
        this.startTime = st;
        dojo.addOnLoad(this, function(){
            this.node = dojo.byId("timer");
        });
    },
    
    displayTimer: function(){
        this.display = true;
        this.interval = setInterval(dojo.hitch(this, function(){ this.updateTime(); }), 500);
        dojo.style(this.node,{"display":"block"});
    },
    
    hideTimer: function(){
        this.display = false;
        dojo.style(this.node,{"display":"none"});
        clearInterval(this.interval);
    },
    
    updateTime: function(){
        var elapsed = (new Date()).getTime()-this.startTime;
        if(this.node){
            this.node.innerHTML = Math.floor(elapsed/1000);
        }
    }
});
