dojo.provide("andes.widget.ExpandoPane");

dojo.require("dojox.layout.ExpandoPane");

dojo.declare("andes.widget.ExpandoPane", dojox.layout.ExpandoPane, {
	// summary:
	//	Allows the expandable Help Pane.
	// 	extends dojox.layout.ExpandoPane
	
	// minimum size the pane can be set
	minSize: 160,
	
	// if true remembers whether pane was open or close
	//	when page loads
	rememberState:true,

	startup: function(){
		this.inherited(arguments);
		
		// create open button that shows when pane is closed
		this.openButtonNode = dojo.create("div", {id:"helpPaneOpenButton", innerHTML:"&nbsp;"});
		
		dojo.style(this.openButtonNode, "display", "none");
		dojo.place(this.openButtonNode, dojo.byId("drawingPane"), "last");
		dojo.connect(this.openButtonNode, "onclick", this, "openHelp");
		
		if(dojo.isIE){
			dojo.addClass(this.iconNode, "IEHelpIconFix");
			dojo.addClass(this.titleNode, "IEHelpTileFix");
			dojo.style(this.titleWrapper, "height", "30px");
		}
		var scoreContainerNode = dojo.create("div", {className:"helpScore", innerHTML:"Score: <span>0</span>%"}, this.titleWrapper, "last");
		this.scoreNode = dojo.query("span", scoreContainerNode)[0];
		
		if(this.rememberState){
			dojo.connect(this, "toggle", this, function(arg){
				this._setCookie();
			});
		}
		
		dojo.addOnLoad(this, "initLayout");
	},
	
	_setCookie: function(){
		var _isOpen = this._showing ? "open" : "closed";
		dojo.cookie("helpPane", _isOpen, { expires: 999 });
	},
	
	initLayout: function(){
		var ck = dojo.cookie("helpPane");
		if(ck && ck == "open"){
			this.toggle();
		}
		
		dojo.connect(dijit.byId("helpInput"), "onKeyUp", this, function(evt){
			if(evt.keyCode==13){
				dijit.byId("helpSubmit").onClick();
			}
		});
		
		dojo.connect(this, "resize", this, function(){
			if(this._contentBox.w){
				dojo.style(dijit.byId("helpInput").domNode, "width", this._contentBox.w - 52 + "px");
			}
		});
	},
	
	_setupAnims: function(){
		this._closedSize = 0;
		this.inherited(arguments);
	},

	_showEnd: function(){
		dojo.style(this.openButtonNode, "display", "none");
		this.inherited(arguments);
	},

	_hideEnd: function(){
		this.inherited(arguments);
		dojo.style(this.openButtonNode, {display:"block", opacity:0});
		dojo.fadeIn({node:this.openButtonNode}).play();
	},

	score: function(value){
		// set score
		if(typeof value != "undefined"){
			this.scoreNode.innerHTML = value;
		}
		return this.scoreNode.innerHTML;
	},
	
	open: function(){
		// explicitly opens pane (if closed)
		if(!this._showing){
			this.toggle();
		}
	},
	
	openHelp: function(){
		// actually this toggles
		dijit.byId("helpInput").attr("value", "");
		if(dijit.byId("helpContentPane").attr("content").length == 0){
			dijit.byId("helpSubmit").onClick();
		}
		this.toggle();
	}
});
