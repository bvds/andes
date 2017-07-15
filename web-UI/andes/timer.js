/* global define */
define([
    "dojo/dom",
    "dojo/dom-style",
    "dojo/_base/declare",
    "dojo/_base/lang",
    "dojo/ready"
], function(dom, domStyle, declare, lang, ready){

    return declare(null,{
        // summary:
        //      This is created from the api when startTime is
        //      initialized.
        node: null,
        display:false,
        ready:false,
        constructor: function(st){
            this.startTime = st;
            ready(this, function(){
                console.info("andes/timer.js: connect timer");
                this.node = dom.byId("timer");
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
                this.interval = window.setInterval(lang.hitch(this, function(){ this.updateTime(); }), 500);
            }else{
                disp = "none";
                if(this.interval) window.clearInterval(this.interval);
            }
            domStyle.set(this.node, "display", disp);
        },

        updateTime: function(){
            var elapsed = (new Date()).getTime()-this.startTime;
            if(this.node){
                this.node.innerHTML = Math.floor(elapsed/1000);
            }
        }
    });
});
