/* global define */
// Pre-AMD version had a function wrapper.
define([
    "dojo/dom",
    "dojo/cookie",
    "dojo/_base/connect",  // This needs to be replaced by dojo/on or dojo/aspect
    "dojo/_base/lang",
    "dijit/registry",
    "dojo/ready",
    "dojox/drawing/util/common",
    "dojox/drawing/manager/_registry",
    "dojox/drawing/tools/custom/Axes",
    "dojox/drawing/tools/custom/Vector",
    "dojox/drawing/tools/TextBlock",
    "andes/Combo", // to make new andes.Combo
    "andes/principles" // for andes.principles.review
    // pre-AMD version had no reqire statements.
],function(dom, cookie, connect, lang, registry, ready, common, managerRegistry){
	
	cookie("mikeDev", null, { expires: -1 });
	
	// the html ID in index for the drawing app
	var drawingId = "drawing";
        var _drawing;
	var _surfaceLoaded = false;
	
	
	var stencils = {
		// used for mapping objects between andes
		// and Drawing
		//
		line: 		"dojox.drawing.stencil.Line",
		rect: 		"dojox.drawing.stencil.Rect",
		ellipse: 	"dojox.drawing.stencil.Ellipse",
		vector: 	"dojox.drawing.tools.custom.Vector",
		axes: 		"dojox.drawing.tools.custom.Axes",
		textBlock:	"dojox.drawing.tools.TextBlock"
	};
	

	var hasStatement = {
		// These objects get statements associated with them
		//
		"dojox.drawing.stencil.Line":true,
		"dojox.drawing.stencil.Rect":true,
		"dojox.drawing.stencil.Ellipse":true,
		"dojox.drawing.tools.custom.Vector":true,
		"dojox.drawing.tools.custom.Axes":true
	};
	
	var hasLabel = {
		// Special case for Axes and its double-label
		//
		"dojox.drawing.tools.custom.Axes":true
	};
	
	
	
	var getStatementPosition = function(box){
		// summary:
		//	Simple method for determining position of
		// 	statements next to objects. If changed, do not
		// 	change 'showEmpty'.
		//
		var gap = 10;
		return {
			deleteEmptyCreate:false,
			deleteEmptyModify:false,
			data:{
				x:box.x2 + gap,
				y:box.y1,
				showEmpty:true
			}
		};
	};

	var items = {};
	var masterMap = {};
	
    ready(function(){
        console.log("andes/tracking.js: connect to \"drawing,\" wire up logging.");
	_drawing = registry.byId(drawingId);
	console.log("got drawing widget:  ",_drawing);
	// This was dojo.connect in pre-AMD version
		var cn = connect.connect(_drawing, "onSurfaceReady", function(){
		        connect.disconnect(cn);
			window.andes.WordTip.add(_drawing);
		    // This was in the pre-AMD version
		    // This seems to lead to a recursion?
		    console.log("tracking.js:  about to call onSurfaceReady from ",window.andes.drawing);
		    window.andes.drawing.onSurfaceReady();
			if(_drawing.stencils){
				console.warn("Label double click connected");
				connect.connect(_drawing.stencils, "onLabelDoubleClick", lang.hitch(window.andes.drawing,"onLabelDoubleClick"),true);
			}
		});
		connect.connect(_drawing, "onRenderStencil", window.andes.drawing, "onRenderStencil");
		
		// Track user's focus on Andes.  So far only whether they are using the window/tab
		// or have left to use another program
		if(dojo.isIE){
			connect.connect(dojo.global, "onfocus", window.andes.drawing, "onWindowFocus");
			// connect.connect(dojo.global, "onfocusin", drawing, "onWindowFocus");
			connect.connect(dojo.doc, "onfocusout", this, function() {
				if (this._activeElement != document.activeElement){
					this._activeElement = document.activeElement;
				}else{
					window.andes.drawing.onWindowBlur();
				}
			});
		}else if(dojo.isSafari){
			connect.connect(window, "onblur", window.andes.drawing, "onWindowBlur");
			connect.connect(window, "onfocus", window.andes.drawing, "onWindowFocus");
		}else{
			connect.connect(dojo.doc, "onblur", window.andes.drawing, "onWindowBlur");
			connect.connect(dojo.doc, "onfocus", window.andes.drawing, "onWindowFocus");
		}
    });

	window.andes.drawing = {
		// summary:
		//	The master object that controls behavior of Drawing items
		//	and handles transfer of data between server and client
		//
		onLabelDoubleClick: function(obj){
			// summary:
			//		Use the map to find the statement when the label
			//		is clicked, and allow editing
			//console.log("-------->Andes Double click connected", masterMap[obj.id].statement, masterMap[obj.id].statement.edit);
			var s = masterMap[obj.id].statement;
			if(s.getText()==""){
				// First populate the textBox
				s.select();
				s.deselect();
			};
			s.editMode = true;
			s.edit();
			
		},
		
		onRenderStencil: function(item){
			// summary:
			//	Called on drag-create. This method should call add()
			//	then save info to the server.
			//
			if(items[item.id]){
				console.warn("BLOCKED on render: ", item.id);
				return;
			}
			if(hasStatement[item.type] || hasLabel[item.type]){
				// vector, rect, ellipse, or axes
				var box = item.getBounds();
				var props = getStatementPosition(box);
				if(hasLabel[item.type]){
					// axes
					// default labels for an axes
				        // This is the switch used in dojox/drawing/tools/custom/Axes.js:
					props.data.text = item.style.zAxisEnabled?
						"x and y and z":"x and y";
				}
				// create statement for vector, rect, ellipse, or axes
				var statement = _drawing.addStencil("textBlock", props);
				if(hasLabel[item.type]){
					// axes
					var s = statement;
					s.customType = "axes";
					item.connect(statement, "onChangeText", this, function(value){
						item.setLabel(value);
						console.log("-------> onChangeText calling setLabel for ", item.id,": ",value);
						this.add(item, true);
						_drawing.removeStencil(s);
					});
					

				}else if(hasStatement[item.type]){
					// vector, rect, ellipse
					var c = new window.andes.Combo({master:item, statement:statement, onCreate: lang.hitch(this, function(){
						this.add(c, true);
					})});
					
				}
			}else{
				// statement or equation
				if(item.isText && window.andes.defaults.text.deleteEmptyCreate && !item.getText()){
					// no text. will be deleted.
					return;
				}
				console.log("ADD EQU OR STT>>>", item.customType);
				this.add(item, true);
			}
		},
		
		addGroup: function(/* group of objects */group){
			// Taken from add(...) below.
			items[group.id] = group;

			dojo.forEach(group.items,function(item){
				connect.connect(item.master,"onClick",this,function(item){

					// Handle button clicks; don't do anything for done button.
					if(item.buttonType == "checkbox"){
						if(item.selected) {
							var pos=dojo.indexOf(item.group.checked,item.value);
							item.group.checked.splice(pos,1);
							item.deselect();
						}else{
							item.group.checked.push(item.value);
							item.select();
						}
					}else if(item.buttonType == "radio"){
						item.group.checked=[item.value];
						var myId=item.id;
						dojo.forEach(item.buttons,function(button){
							if(button.id == myId){
								if(!button.selected){button.select();}
							}else{
								// deselect all other buttons in group
								if(button.selected){button.deselect();}
							}
						});
					}

					// Checkboxes only make local modifications
					if(item.buttonType != "checkbox"){
						// Send result to server
						var data = window.andes.convert.drawingToAndes(group, "modify-object");
						// BvdS:  Why doesn't this.save() work?
					    console.log("save drawing data, this=",this);
						window.andes.drawing.save(data);
					}
				});
			});
		},
		
		add: function(/* Stencil */ item, /*Boolean*/ saveToServer, /*Boolean*/noConnect){
			// summary:
			//	items added here may be from the server OR drag-created.
			//	They should most often be combo items with andes.Combo,
			// 	with the exception of Axes and (standalone) Statements.
			//
			//  Test to see if there's already an object with this id,
			//  if so increment the id and the count in dojox util.
			var i = 0;
			while(items[item.id]){
				common.uid(item.type);
				item.id = item.type + i++;
			}
			items[item.id] = item;
			
			if(item.master){
				masterMap[item.master.id]=item;
			};
			
			if(noConnect){
				return;
			}

			item.connect("onDelete", this, function(item){
				var id = item.id;
				console.log("----------------------------> onDelete", id);
				this.remove(item);
				if(!item.mod){
					this.save({action:"delete-object", id:item.id});
				}
			});
			
			item.connect("onChangeData", this, function(item){
				if (item.mod == true) { return;};
				console.log("----------------> onChangeData andes.drawing", item.id, item.type);
				// Until we know server diagnosis, set to unknown.
				item.mod = true; // disable save to server, else we get a recursive call
				item.attr(window.andes.defaults["unknown"]);
				item.mod = false; // restore save to sever
				var data = window.andes.convert.drawingToAndes(item, "modify-object");
				console.info("Save mod to server", data);
				this.save(data);
			});
			
			if(saveToServer){
				// we need to save it to the server
				var data = window.andes.convert.drawingToAndes(item, "new-object");
				console.info("Save new to server:", data);
				this.save(data);
			}
		},
		
		remove: function(/* Stencil */ item){
			// summary:
			//	Just removes reference. See item.connect.onDelete above
			delete items[item.id];
		},
		
		
		handleServerActions: function(data){
			// summary:
			//	Handle objects returned from server.
			//	Handles all returns, including open-problem
			//	and solution-step.
			//
			//	NOTE: andes/help intercepts calls and handles
			//	any help associated with the data.
			//
			console.log("handleServerActions starting", data.length);
			
			// check for highest numerical ID
			// start from there
			var getNum = function(m){
				if(!m.id){ return 0; }
				var ar = m.id.match(/\d/g);
				if(!ar || !ar.length){ return 0; }
				return parseInt(ar.join(""),10);
			};
			var idNum = 0;
			dojo.forEach(data, function(m){
				idNum = Math.max(getNum(m), idNum);
			});
			++idNum;
			
			common.idSetStart(idNum);
			
			//console.dir(data);
			var mods = [];
			var min = 2, max = 5;
			dojo.forEach(data, function(obj, i){
				// For easier-to-read and target server logs-
				/*
				if(obj.action!="new-object" || obj.action!="modify-object"){
					var description="";
					for(var o in obj){
						if(o==undefined || obj[o]==undefined) continue;
						description += o+": "+obj[o]+" ";
					}
					console.log(description);
				}*/
				
				if(obj.action =="new-object"){
					var o = window.andes.convert.andesToDrawing(obj);
					
					var t = o.stencilType;
					// o.stencilType includes:  text, image, line, rect, ellipse, vector
					//                          textBlock (equation & statement), axes
					if(t=="vector" || t=="line" || t=="ellipse" || t=="rect"){
						
						// prevent adding items via onRenderStencil
						// by adding the ids first:
						var statement = _drawing.addStencil("textBlock", o.statement);
						var master = _drawing.addStencil(o.stencilType, o.master);
						items[statement.id] = statement; //statement;
						items[master.id] = master; //master;
						var combo = new window.andes.Combo({master:master, statement:statement, id:o.id});
						this.add(combo);
						
					}else if(o.type=="button" && o.items){ // button groups don't have stencilType
						var butt = dojo.map(o.items,function(item){
							var statement = _drawing.addStencil("text", item.statement);
							var master = _drawing.addUI(item.stencilType, item);
							master.group=o;
							items[statement.id] = statement; //statement;
							items[master.id] = master; //master;
							return {master: master, statement:statement};
						});
						// Back pointer to other buttons in group, for deselection
						// of radio buttons.
						var buttOnly = dojo.map(butt,function(x){return x.master;});
						dojo.forEach(butt,function(x){x.master.buttons=buttOnly;});
						
						var buttonCombo=new window.andes.buttonCombo(butt,o.id);
						buttonCombo.group=o;
						this.addGroup(buttonCombo);
					}else{
						// including:  textBlock, axes ...
						var item = _drawing.addStencil(o.stencilType, o);
						var ID = item.id;
						ID = ID.indexOf("TextBlock");
						if(item.stencilType=='textBlock' && ID!=-1) item.util.uid(item.type);
						item.customType = obj.type; // to tell between equation and statement
						this.add(item);
					}

					// Add any color
					if(obj.mode){
						items[o.id].attr(window.andes.defaults[obj.mode]);
					}					

				}else if(obj.action=="modify-object"){
					mods.push(obj);
					
				}else if(obj.action=="delete-object"){
				        // need error handling for non-existant objects.
					if(items[obj.id]){
						items[obj.id].mod = true;  // don't echo back to server
						if (items[obj.id].type=="andes.Combo") {
							items[obj.id].master.destroy();
						} else {
							items[obj.id].destroy();
						};
						delete items[obj.id];
					}
					
				}else if(obj.action=="set-score"){
					window.andes.help.score(obj.score);
					
				}else if(obj.action=="new-user-dialog" && obj.text){
					window.andes.error({
						title: "Welcome to Andes!",
						message: obj.text,
						dialogType: window.andes.error.OK,
						noLog: true
					});
					var button=dom.byId("andesButtonPageDefault");
					console.assert(button,"buttonsNode object not found");
					// Add event to Error box default OK button.
					// This opens the general introduction.
					// It should be disconnected when the
					// dialog box is closed!  See bug #1628
					connect.connect(button, 
						     "click", 
						     function(){
							     // add 10 px padding
							     window.andes.principles.review('vec1a-video.html','IntroVideo',null,"width=650,height=395");
						     });					
				}else if(obj.action=="new-user-dialog" && obj.url){
					var x=registry.byId("consentDialog");
					x.set("href",obj.url);
					x.set("title","Consent Form");
					x.show();
				}else if(obj.action=="set-styles"){
					if(obj["tool"] && obj["style"]){
						var disable = obj["style"]=="disabled" ? true : false;
						var tool = managerRegistry.getRegistered("button",obj["tool"]);
					    console.assert(tool,"Unregistered button",obj["tool"]);
						disable ? tool.disable() : tool.enable();
					}

				}else if(obj.action=="set-preference"){
					// Try to set in the preferenceRegistry.  All
					// values that can be saved should be available there
					window.andes.preferenceRegistry.setPref(obj["name"],obj["value"]);
					
				}else if(obj.action=="log"){
					// Log actions are ignored by client.
				}else{
					// Ignore items for the help pane.
					// These are handled by handleHelp().
					console.log("    handleServerActions ignoring \"", obj.action,"\"");
				}
			}, this);

			dojo.forEach(mods, function(obj){
				// convertAndesToDrawing has not been applied to obj
				//
				// handles any object modifications
				//
				// obj.mod=="deleted" should never occur if
				// items[obj.id] exists.
				if(items[obj.id]){
					items[obj.id].mod = true;  // don't echo back to server
				        // style
					items[obj.id].attr(window.andes.defaults[obj.mode]);
					// x, y
					if(obj.x!==undefined){
						items[obj.id].attr({
							x:obj.x,
							y:obj.y
						});
					}
					if(obj["x-statement"]!==undefined){
						items[obj.id].statement.attr({
							x:obj["x-statement"],
							y:obj["y-statement"]
						});
					}

					if(obj.type=='vector' || obj.type=='line'){
						if(obj.radius == 0 && obj.angle == 0) { obj.angle = 1; }
						items[obj.id].master.attr({
							angle:obj.angle,
							radius:obj.radius,
							cosphi:obj.cosphi
						});
					}else if(obj.type=="axes"){
						items[obj.id].attr({
							angle:obj.angle,
							radius:obj.radius,
							cosphi:obj.cosphi
						});
					}else if(obj.type=="ellipse" || obj.type=='rectangle'){
						items[obj.id].master.attr({
							height:obj.height,
							width:obj.width
						});
					}else if(obj.type=="button" && obj.checked){ // checked is optional
						items[obj.id].group.checked=obj.checked;
						dojo.forEach(items[obj.id].items,function(pair){
							if(dojo.indexOf(obj.checked,pair.master.value)!=-1){
								pair.master.select();
							}else{
								pair.master.deselect();
							}
						});
					}

					// text
					if(items[obj.id].isText==true && obj.text) { items[obj.id].attr({text:obj.text});};
					if(obj.text && items[obj.id].type == "andes.Combo"){
						// obj.symbol can never change without obj.text changing.
						items[obj.id].textEdit(obj.text);
					};

					items[obj.id].mod = false;   // restore save to server
					
				};
			},this);

			/*
			for(var itm in items){
				if(itm.shortType=="textBlock") { itm.execText(); console.warn(itm); }
			};
			*/

			data = null;
		},
		
		onSurfaceReady: function(){
			// Drawing is ready.
			_surfaceLoaded = true;
			if(this._initialData){
				this.handleServerActions(this._initialData);
			}
		},
		
		save: function(data){
			// summary:
			//	Save an object to the server.
			
			var dfd = window.andes.api.step(data);
			dfd.addCallback(this, function(data){
				window.setTimeout(dojo.hitch(this, function(){
					this.handleServerActions(data);
				}),0);
			});
			dfd.addErrback(this, "onError");
		},
		
		load: function(){
			// summary:
			//	loads project data
			//
			// called from the very bottom of main.js
			//
		    console.info("andes/drawing.js:  loading drawing ");

			// setting 'this'
			this.loadProject = function(){
				console.info("load server data", window.andes.userId, window.andes.projectId, window.andes.sectionId);
				window.andes.api.open({user:window.andes.userId, problem:window.andes.projectId,section:window.andes.sectionId,extra:window.andes.extra})
					.addCallback(this, function(data){
						window.setTimeout(dojo.hitch(this, function(){
							this.onLoad(data);
						}),0);
					})
					.addErrback(this, "onError");
			};
			if(window.andes.closeFirst){
				// a previous project session is open. close it.
				window.andes.api.close({}).addCallback(this, "loadProject").addErrback(this, "onError");
			}else{
				this.loadProject();
			}
		},
		
		onLoad: function(data){
			// summary:
			//	Project Data Loaded
			this._initialData = data;
			if(_surfaceLoaded){
				this.handleServerActions(this._initialData);
			};
			
		},
		onError: function(err){
			console.error("There was an error in the project data:", err);
			if(!this._initialData){
				// apparently an error on open-problem. Try closing session.
				window.andes.api.close({});
				cookie("andes", null, { expires: -1 });
			}
		},
		
		onWindowBlur: function(){
			// summary:
			//	Event for when the user leaves this window
			//	say to open another tab.
			console.log("Lost window focus for ",this.name || "canvas","; ",this);
			window.andes.api.recordAction({type:"window", name: this.name || "canvas", value: "blur"});
		},
		
		onWindowFocus: function(){
			// summary:
			// 	Event for when this window is focused, such as
			// 	switching back to this tab from another browser tab
			console.log("Gained window focus for ",this.name || "canvas","; ",this);
			window.andes.api.recordAction({type:"window", name: this.name || "canvas", value: "focus"});
		}
    };

});
