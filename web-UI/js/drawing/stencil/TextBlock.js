dojo.provide("drawing.stencil.TextBlock");
dojo.require("drawing.stencil.Text");

(function(){
	
	var conEdit, wrapNode;
	dojo.addOnLoad(function(){
		// summary:
		//	In order to use VML in IE, it's necessary to remove the
		//	DOCTYPE. But this has the side effect that causes a bug
		//	where contenteditable divs cannot be made dynamically.
		//	The solution is to include one in the main document
		//	that can be appended and removed as necessary:
		//	<div id="conEdit" contenteditable="true"></div>
		//
		conEdit = dojo.byId("conEdit");
		if(!conEdit){
			console.error("A contenteditable div is missing from the main document.")
		}else{
			conEdit.parentNode.removeChild(conEdit);
		}
	});
	
	drawing.stencil.TextBlock = drawing.util.oo.declare(
		//
		// TODO - handle zoom - and zoom while showing
		// disable?
		//
		drawing.stencil.Text,
		function(options){
			if(options.data){
				this._offX = this.mouse.origin.x;
				this._offY = this.mouse.origin.y;
				var d = options.data;
				var w = Math.max(d.width, this.style.text.minWidth)
				var o = this.measureText(this.cleanText(d.text, false), w);
				this.points = [
					{x:d.x, y:d.y},
					{x:d.x+w, y:d.y},
					{x:d.x+w, y:d.y+o.h},
					{x:d.x, y:d.y+o.h}
				];
				this.render(d.text);
				
			}
		},
		{
			
			showParent: function(obj){
				if(this.parentNode){ return; }
				console.log("SHOW PARENT OBJ:", obj)
				var x = obj.pageX || 10;
				var y = obj.pageY || 10;
				this.parentNode = dojo.doc.createElement("div");
				this.parentNode.id = this.id;
				var d = this.style.text.mode.create;
				this._box = {
					left:x,
					top:y,
					width:obj.width || 1,
					height:obj.height || this._lineHeight,
					border:d.width+"px "+d.style+" "+d.color,
					position:"absolute",
					toPx: function(){
						var o = {};
						for(var nm in this){
							o[nm] = typeof(this[nm])=="number" ? this[nm] + "px" : this[nm];
						}
						return o;
					}
				};
				
				dojo.style(this.parentNode, this._box);
				document.body.appendChild(this.parentNode);
			},
			createTextField: function(txt){
				// style parent
				var d = this.style.text.mode.edit;
				this._box.border = d.width+"px "+d.style+" "+d.color;
				this._box.height = "auto";
				this._box.width = Math.max(this._box.width, this.style.text.minWidth);
				dojo.style(this.parentNode, this._box.toPx());
				// style input
				this.parentNode.appendChild(conEdit);
				dojo.style(conEdit, {
					height: txt ? "auto" : this._lineHeight+"px",
					fontSize:this.style.text.size+"pt",
					fontFamily:this.style.text.fontFamily
				});
				conEdit.innerHTML = txt || "";
				
				return conEdit;
			},
			connectTextField: function(){
				this.keys.editMode(true);
				var kc1, kc2, kc3, self = this, _autoSet = false,
					exec = function(){
						dojo.forEach([kc1,kc2,kc3], dojo.disconnect, dojo);
						self.keys.editMode(true);
						self.execText();
					}
					
				kc1 = dojo.connect(conEdit, "keyup", this, function(evt){
					if(!_autoSet){
						dojo.style(conEdit, "height", "auto"); _autoSet = true;
					}
					if(evt.keyCode==13 || evt.keyCode==27){
						dojo.stopEvent(evt);
						exec();
					}
				});
				kc2 = dojo.connect(conEdit, "keydown", this, function(evt){
					if(evt.keyCode==13 || evt.keyCode==27){ // TODO: make escape an option
						dojo.stopEvent(evt);
					}
				});
				kc3 = dojo.connect(conEdit, "mouseup", this, function(evt){
					dojo.stopEvent(evt);
				});
				this.createAnchors();
				conEdit.focus();
				// once again for Silverlight:
				setTimeout(function(){ conEdit.focus();}, 500);
				
				this.onDown = function(){}
				this.onDrag = function(){}
				this.onUp = function(){
					if(!self._onAnchor){
						this.disconnectMouse();
						exec();
					}
				}
			},
			execText: function(){
				var d = dojo.marginBox(this.parentNode);
				console.warn("TEXT BIX:", d, this.style)
				var w = Math.max(d.w, this.style.text.minWidth)
				
				var txt = this.cleanText(conEdit.innerHTML, true);
				conEdit.innerHTML = "";
				conEdit.blur();
				this.destroyAnchors();
				var o = this.measureText(txt, w);
				
				var x = this._box.left - this._offX;
				var y = this._box.top - this._offY;
				this.points = [
					{x:x, y:y},
					{x:x+w, y:y},
					{x:x+w, y:y+o.h},
					{x:x, y:y+o.h}
				];
		
				this.render(o.text);
			},
			
			edit: function(){ console.log("--onStencilDoubleClick--", this.id,this.parentNode, this.points)
				// NOTE: no mouse obj
				if(this.parentNode || !this.points){ return; }
				var d = this.pointsToData();
				var obj = {
					pageX: d.x + this._offX,
					pageY: d.y + this._offY,
					width:d.width,
					height:d.height
				}
				this.remove(this.shape, this.hit);
				this.showParent(obj);
				this.createTextField(this._text.replace("/n", " "));
				this.connectTextField();
				
			},
			cleanText: function(txt, removeBreaks){
				if(removeBreaks){
					dojo.forEach(['<br>', '<br/>', '<br />', '\\n', '\\r'], function(br){
						txt = txt.replace(new RegExp(br, 'gi'), " ");
					});
				}
				txt = txt.replace(/&nbsp;/g, " ");
				txt = dojo.trim(txt);
				// remove double spaces, since SVG doesn't show them anyway
				txt = txt.replace(/\s{2,}/g, " ");
				return txt;
			},
			
			measureText: function(/* String */ str, /* ? Number */width){
				console.log("BREAKS:", width, "x", this._lineHeight)
				this.showParent({width:width || "auto", height:"auto"});
				this.createTextField(str);
				var txt = "";
				var el = conEdit;
				el.innerHTML = "X";
				var h = dojo.marginBox(el).h;
				
				el.innerHTML = str;
				if(dojo.marginBox(el).h == h){
					// one line
					txt = str;console.log("measureText.singleLine");
				}else if(!width){
					// has line breaks in text
					txt = str; console.log("measureText.textLineBreaks");
				}else{
					// text wraps
					var ar = str.split(" ");console.log("measureText.wraps");
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
					
					dojo.forEach(strAr, function(ar, i){
						strAr[i] = ar.join(" ");
					});	
					txt = strAr.join("\n");
					
					// get the resultant height
					el.innerHTML = txt.replace("\n", "<br/>");
					
				}
				
				var dim = dojo.marginBox(el);
				
				console.log("TEXT BREAKS:::", txt);
				console.log("conEdit", conEdit);
				conEdit.parentNode.removeChild(conEdit);
				dojo.destroy(this.parentNode);
				this.parentNode = null;
				
				return {h:dim.h, w:dim.w, text:txt};
			},
			
			onDrag: function(obj){
				//console.log(" >>>>>> HtmlTextBlock drag");
				if(!this.parentNode){
					this.showParent(obj);
				}
				var s = obj.start, e = obj;
				this._box.left = (s.x < e.x ? s.x : e.x) + obj.orgX;
				this._box.top = s.y + obj.orgY;
				this._box.width = (s.x < e.x ? e.x-s.x : s.x-e.x) + this.style.text.pad;
				dojo.style(this.parentNode, this._box.toPx());
				this._offX = obj.orgX;
				this._offY = obj.orgY;
			},
			
			onUp: function(obj){
				if(!this.shape && !obj.withinCanvas || !this._box){ return; }
				console.log(" >>> >>> HtmlTextBlock up");
				this.createTextField();
				this.connectTextField();
			},
			
			onDown: function(){},
			onMove: function(){},
			
			destroyAnchors: function(){
				for(var n in this._anchors){
					dojo.forEach(this._anchors[n].con, dojo.disconnect, dojo);
					dojo.destroy(this._anchors[n].a);
				}
			},
			
			createAnchors: function(){
				this._anchors = {}, self = this;
				var d = this.style.anchors,
					b = d.width,
					w = d.size-b*2,
					h = d.size-b*2,
					p = (d.size)/2*-1 + "px";
				
				var s = {
					position:"absolute",
					width:w+"px",
					height:h+"px",
					backgroundColor:d.fill,
					border:b+"px " + d.style + " "+d.color
				}
				var ss = [
					{top: p, left:p},
					{top:p, right:p},
					{bottom:p, right:p},
					{bottom:p,left:p}
				];
				for(var i=0;i<4;i++){
					var isLeft = (i==0) || (i==3);
					var id = this.util.uid(isLeft ? "left_anchor" : "right_anchor");
					
					var a = dojo.create("div", {id:id}, this.parentNode);
					dojo.style(a, dojo.mixin(dojo.clone(s), ss[i]));
					
					var md, mm, mu;
					var md = dojo.connect(a, "mousedown", this, function(evt){
						isLeft = evt.target.id.indexOf("left")>-1;
						console.log("DOWN", isLeft);
						self._onAnchor = true;
						var orgX = evt.pageX;
						var orgW = this._box.width;
						dojo.stopEvent(evt);
						
							
						mm = dojo.connect(document, "mousemove", this, function(evt){
							var x = evt.pageX;
							if(isLeft){
								this._box.left = x;
								this._box.width = orgW + orgX - x;
							}else{
								this._box.width = x + orgW - orgX;
							}
							dojo.style(this.parentNode, this._box.toPx());
						});
						
						mu = dojo.connect(document, "mouseup", this, function(evt){
							orgX = this._box.left;
							orgW = this._box.width;
							dojo.disconnect(mm);
							dojo.disconnect(mu);
							self._onAnchor = false;
							conEdit.focus();
							dojo.stopEvent(evt);
						});
					});
					
					this._anchors[id] = {
						a:a,
						cons:[md]
					}
				}
			}
		}
	);
})();