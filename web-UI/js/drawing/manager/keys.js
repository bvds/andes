dojo.provide("drawing.manager.keys");

(function(){
	drawing.manager.keys = {
		arrowIncrement:1,
		arrowShiftIncrement:10,
		
		shift:false,
		ctrl:false,
		alt:false,	// also option key
		cmmd:false, // apple key
		meta:false, // any meta key
		
		onDelete: function(evt){
			// stub
		},
		onEsc: function(evt){
			// stub
		},
		onEnter: function(evt){
			// stub
		},
		onArrow: function(evt){
			// stub
		},
		
		init: function(){
			dojo.mixin(this, dojo.keys);
			dojo.connect(document, "keydown", this, function(evt){
				if(evt.keyCode==16){
					this.shift = true;
				}
				if(evt.keyCode==17){
					this.ctrl = true;
				}
				if(evt.keyCode==18){
					this.alt = true;
				}
				if(evt.keyCode==224){
					this.cmmd = true;
				}
				
				this.meta = this.shift || this.ctrl || this.cmmd || this.alt;
				
				if(evt.keyCode==8 || evt.keyCode==46){
					//this.onDelete(); on down or up?
					dojo.stopEvent(evt);
				}
			});
			dojo.connect(document, "keyup", this, function(evt){
				console.log("KEY UP:", evt)
				if(evt.keyCode==16){
					this.shift = false;
				}
				if(evt.keyCode==17){
					this.ctrl = false;
				}
				if(evt.keyCode==18){
					this.alt = false;
				}
				if(evt.keyCode==224){
					this.cmmd = false;
				}
				
				this.meta = this.shift || this.ctrl || this.cmmd || this.alt;
				
				if(evt.keyCode==13){
					this.onEnter(evt);
					dojo.stopEvent(evt);
				}
				if(evt.keyCode==27){
					this.onEsc(evt);
					dojo.stopEvent(evt);
				}
				if(evt.keyCode==8 || evt.keyCode==46){
					this.onDelete(evt);
					dojo.stopEvent(evt);
				}
			});
			
			dojo.connect(document, "keypress", this, function(evt){
				var inc = this.shift ? this.arrowIncrement*this.arrowShiftIncrement : this.arrowIncrement;
				
				var x =0, y =0;
				if(evt.keyCode==37){ //left
					x = -inc;
				}
				if(evt.keyCode==38){ //up
					y = -inc;
				}
				if(evt.keyCode==39){ //right
					x = inc;
				}
				if(evt.keyCode==40){ //down
					y = inc;
				}
				if(x || y){
					evt.x = x;
					evt.y = y;
					evt.shift = this.shift;
					this.onArrow(evt);
					dojo.stopEvent(evt);
				}
			});
		}
	};
	dojo.addOnLoad(drawing.manager.keys, "init");
})();

/*
 dojo.keys = {
	// summary: definitions for common key values
	BACKSPACE: 8,
	TAB: 9,
	CLEAR: 12,
	ENTER: 13,
	SHIFT: 16,
	CTRL: 17,
	ALT: 18,
	PAUSE: 19,
	CAPS_LOCK: 20,
	ESCAPE: 27,
	SPACE: 32,
	PAGE_UP: 33,
	PAGE_DOWN: 34,
	END: 35,
	HOME: 36,
	LEFT_ARROW: 37,
	UP_ARROW: 38,
	RIGHT_ARROW: 39,
	DOWN_ARROW: 40,
	INSERT: 45,
	DELETE: 46,
	HELP: 47,
	LEFT_WINDOW: 91,
	RIGHT_WINDOW: 92,
	SELECT: 93,
	NUMPAD_0: 96,
	NUMPAD_1: 97,
	NUMPAD_2: 98,
	NUMPAD_3: 99,
	NUMPAD_4: 100,
	NUMPAD_5: 101,
	NUMPAD_6: 102,
	NUMPAD_7: 103,
	NUMPAD_8: 104,
	NUMPAD_9: 105,
	NUMPAD_MULTIPLY: 106,
	NUMPAD_PLUS: 107,
	NUMPAD_ENTER: 108,
	NUMPAD_MINUS: 109,
	NUMPAD_PERIOD: 110,
	NUMPAD_DIVIDE: 111,
	F1: 112,
	F2: 113,
	F3: 114,
	F4: 115,
	F5: 116,
	F6: 117,
	F7: 118,
	F8: 119,
	F9: 120,
	F10: 121,
	F11: 122,
	F12: 123,
	F13: 124,
	F14: 125,
	F15: 126,
	NUM_LOCK: 144,
	SCROLL_LOCK: 145
};
*/