dojo.provide("andes.widget.UpgradeBar");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dojo.fx");
dojo.require("dojo.cookie");

dojo.declare("andes.widget.UpgradeBar", [dijit._Widget, dijit._Templated], {

	startExpanded: false,

	templateString: "<div class=\"andesUpgradeBar\"><div class=\"andesUpgradeBarMessage\" dojoAttachPoint=\"messageNode\">message</div><div class=\"andesUpgradeBarControls\" dojoAttachPoint=\"controlsNode\"><button type=\"button\" class=\"andesUpgradeBarReminderButton\" dojoAttachPoint=\"dontRemindButtonNode\" dojoAttachEvent=\"onclick:_onDontRemindClick\">Don't Remind Me Again</button><div class=\"andesUpgradeBarCloseIcon\" dojoAttachPoint=\"closeIconNode\" dojoAttachEvent=\"onclick:_onCloseClick\">X</div></div></div>",

	postCreate: function(){
		this.inherited(arguments);

		dojo.mixin(this.attributeMap, {
			message:{ node:"messageNode", type:"innerHTML" }
		});

		this._bodyMarginTop = dojo.style(dojo.body(), "marginTop");
		this._size = dojo.contentBox(this.domNode).h;
		this._showing = this.startExpanded;
		if(!this._showing){
			dojo.style(this.domNode, "display", "none");
		}
	},

	ask: function(msg){
		if(msg){
			this.attr("message", msg);
		}
		if(!dojo.cookie("disableUpgradeReminders")){
			this.show();
		}
	},

	show: function(){
		dojo.style(this.domNode, { display:"block", height:0, opacity:0 });
		if(!this._showAnim){
			this._showAnim = dojo.fx.combine([
				dojo.animateProperty({ node:dojo.body(), duration:500, properties:{ marginTop:this._bodyMarginTop+this._size } }),
				dojo.animateProperty({ node:this.domNode, duration:500, properties:{ height:this._size, opacity:1 } })
			]);
		}
		this._showAnim.play();
	},

	hide: function(){
		if(!this._hideAnim){
			this._hideAnim = dojo.fx.combine([
				dojo.animateProperty({ node:dojo.body(), duration:500, properties:{ marginTop:this._bodyMarginTop } }),
				dojo.animateProperty({ node:this.domNode, duration:500, properties:{ height:0, opacity:0 } })
			]);
			dojo.connect(this._hideAnim, "onEnd", this, function(){
				dojo.style(this.domNode, "display", "none");
			});
		}
		this._hideAnim.play();
	},

	_onDontRemindClick: function(){
		dojo.cookie("disableUpgradeReminders", true, { expires:3650 });
		this.hide();
	},

	_onCloseClick: function(){
		this.hide();
	}

});
