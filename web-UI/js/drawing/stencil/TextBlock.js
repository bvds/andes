dojo.provide("drawing.stencil.TextBlock");

(function(){
	
	var conEdit, wrapNode;
	dojo.addOnLoad(function(){
		// summary:
		//	In order to use VML in IE, it's necessary to remove the
		//	DOCTYPE. But this has the side effect that causes a bug
		//	where contenteditable divs cannot be made dynamically.
		//	The solution is to include one in the main document
		//	that can be added and removed as necessary:
		//	<div id="conEdit" contenteditable="true"></div>
		//
		conEdit = dojo.byId("conEdit");
		if(!conEdit){
			console.error("A contenteditable div is missing from the main document.")
		}
		conEdit.parentNode.removeChild(conEdit);
	});
	
	var showEditNode = function(x, y){
		wrapNode = dojo.doc.createElement("div");
		wrapNode.appendChild(conEdit);
		dojo.style(wrapNode, {
			position:"absolute",
			left:x+10+"px",
			top:y+10+"px"
		});
		document.body.appendChild(wrapNode);
		return conEdit;
	};
	var hideEditNode = function(){
		conEdit.blur();
		conEdit.parentNode.removeChild(conEdit);
		dojo.destroy(wrapNode);
	};
	
	drawing.stencil.TextBlock = drawing.util.oo.declare(
		drawing.stencil.Rect,
		function(options){
			this.points = [];
			this.style.line = this.style.outline;
			this.style.fill = {r:255,g:255,b:255,a:.5};
			this._lineHeight = this.textStyle.size * 1.5//this.textStyle.pad*2
			if(options.data){
				var strAr = this.getLineBreaks(options.data);
				this.render();
				this.renderText(strAr);
			}else{
				this.points = [];
			}
		},
		{
			textStyle:{
				minWidth:150,
				size:24,
				pad:3,
				fontFamily:"serif"
			},
			renderText: function(strAr){
				this.remove(this.textblock);
				var d = this.pointsToData();
				var x = d.x + this.textStyle.pad*2;
				var y = d.y + this._lineHeight - (this.textStyle.size*.3);
				var h = this._lineHeight;
				this.textblock = this.parent.createGroup();
				this._textArray = strAr || this._textArray;
				dojo.forEach(this._textArray, function(ar, i){
					var txt = ar.join(" ");
					var tb = this.textblock.createText({x: x, y: y+(h*i), text: txt, align: "start"})
						.setFont({family: this.textStyle.fontFamily, size: this.textStyle.size+"pt", weight: "normal"})
						.setFill("black");
					
					this.util.attr(tb, "drawingType", "stencil");
					
				}, this);
				this.util.attr(this.textblock, "drawingType", "stencil");
				this.onRender(this);
				dojo.connect(this, "render", this, "renderText");
			},

			getLineBreaks: function(/* node or String */ s){
				console.log("BREAKS:", s, this._lineHeight)
				var el;
				if(!s.nodeType){
					var d = s;
					var p = this.textStyle.pad;
					// offsets are the diffs from the page to the canvas
					var x = 10;
					var y = 10;
					el = showEditNode(x, y);
					
					// FF needs a height or the cursor blinks above
					// the input when there's no text
					dojo.style(el, {
						width:d.width-(p*2)+"px",
						fontSize:this.textStyle.size+"pt",
						fontFamily:this.textStyle.fontFamily
					});
					
					el.innerHTML = d.text;
					
				}else{
					el = s;
				}
				
				dojo.style(el, "height", "auto");
				var txt = el.innerHTML;
				txt = txt.replace(/<br>/g, " ");
				txt = txt.replace(/&nbsp;/g, " ");
				txt = dojo.trim(txt);
				// remove double spaces, since SVG doesn't show them anyway
				txt = txt.replace(/\s{2,}/g, " ");
				el.innerHTML = "X";
				var h = dojo.marginBox(el).h;
				
				el.innerHTML = txt;
				if(dojo.marginBox(el).h == h){
					// no line breaks
					var strAr = [[txt]];
				}else{
					var ar = txt.split(" ");
					var strAr = [[]];
					var line = 0;
					el.innerHTML = "";
					while(ar.length){
						var word = ar.shift();
						el.innerHTML += word+" "; //urk, always an extra space
						if(dojo.marginBox(el).h > h){
							line++;
							strAr[line] = [];
							el.innerHTML = word+" ";
						}
						strAr[line].push(word)
					}
				}
				
				if(!s.nodeType){
					//we need to get our dimensions
					var txt = [];
					dojo.forEach(strAr, function(ar, i){
						txt.push(ar.join(" "));
					});
					el.innerHTML = txt.join("<br/>");
					var dim = dojo.marginBox(el);
					var data = {
						x:d.x,
						y:d.y,
						width:d.width || dim.w,
						height:dim.h
					}
					this.points = this.dataToPoints(data);
					console.log("POINTS:", this.points)
					hideEditNode();
					el = null;
				}
				
				return strAr;
			},
			
			showText: function(obj){
				this.keys.editMode(true);
				var d = this.pointsToData();
				var p = this.textStyle.pad;
				// offsets are the diffs from the page to the canvas
				var x = d.x + this._offsetX - p;
				var y = d.y + this._offsetY - (this.textStyle.size*.3) - p;
				var el = showEditNode(x, y);
				
				// FF needs a height or the cursor blinks above
				// the input when there's no text
				dojo.style(el, {
					width:d.width-(p*2)+"px",
					height:this._lineHeight+"px",
					fontSize:this.textStyle.size+"pt",
					fontFamily:this.textStyle.fontFamily
				});
				
				
				el.innerHTML = "";
				
				var kc1, kc2,kc3,kc4,kc5,kc6;
				
				kc1 = dojo.connect(el, "keyup", this, function(evt){
					if(!this._editheight){
						dojo.style(el, "height", "auto");
					}
			
					var h = dojo.marginBox(el).h;
					if(this._editheight!=h){
						//console.warn("HEIGHT CHANGE", h, "from:", this._editheight)
						this._editheight = h;
						
						var s = this.points[0],
							e = this.points[2],
							data =  {
								x: s.x,
								y: s.y,
								width: d.width,
								height: h
							}
							
						this.points = this.dataToPoints(data);
						this.render();
					}
					if(evt.keyCode==13){ dojo.stopEvent(evt); }
				});
				
				
				var self = this, exec = function(){
					console.time("linebreaks");
					var strAr = self.getLineBreaks(el);
					console.timeEnd("linebreaks");
					console.dir(strAr);
					hideEditNode();
					
					dojo.forEach([kc1,kc2,kc3,kc4,kc5,kc6], dojo.disconnect, dojo);
					el = null;
					self.renderText(strAr);
					
				}
				kc2 = dojo.connect(el, "keydown", this, function(evt){
					//console.log("KEY:", evt.keyCode)
					if(evt.keyCode==13){
						exec();
						dojo.stopEvent(evt);
					}
					
				});
				
				// need to allow clicking within the field
				var _elClicked = false;
				kc3 = dojo.connect(el, "mousedown", this, function(evt){
					_elClicked = true;
				});
				kc4 = dojo.connect(this, "onUp", this, function(evt){
					if(!_elClicked){ exec(); }
					_elClicked = false;
				});
				
				// for dev:
				kc5 = dojo.connect(el, "focus", this, function(evt){ /*console.log("FOCUS");*/ });
				kc6 = dojo.connect(el, "blur", this, function(evt){	/*console.log("BLUR");*/ });
				
				el.focus();
				// once again for Silverlight:
				setTimeout(function(){ el.focus();}, 500);
			},
			
			onDrag: function(obj){
				var s = obj.start, e = obj;
				var	x = s.x < e.x ? s.x : e.x,
					y = s.y,
					w = (s.x < e.x ? e.x-s.x : s.x-e.x) + this.textStyle.pad,
					h = this._lineHeight;
				
				this.points = [
					{x:x, y:y}, 	// TL
					{x:x+w, y:y},	// TR
					{x:x+w, y:y+h},	// BR
					{x:x, y:y+h}	// BL
				];
				this.render();
				
			},
			
			onUp: function(obj){
				if(!this.shape && !obj.withinCanvas){ return; }
				if(Math.abs(obj.x-obj.start.x<this.textStyle.minWidth)){
					if(obj.x<obj.start.x){
						obj.start.x = obj.start.x + this.textStyle.minWidth;
					}else{
						obj.x = obj.start.x + this.textStyle.minWidth;
					}
					obj.y = obj.start.y + this.textStyle.size + this.textStyle.pad*2;
					
					this.onDrag(obj);
				}
				this._offsetX = obj.orgX;
				this._offsetY = obj.orgY;
				
				this.onDown = function(evt){}
				this.onUp = function(evt){}
				this.onDrag = function(evt){}
				
				this.showText(obj);
			}
		}
	);
})();