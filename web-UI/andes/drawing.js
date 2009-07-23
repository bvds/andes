dojo.provide("andes.drawing");


(function(){
	
	dojo.cookie("mikeDev", null, { expires: -1 });
	
	// the html ID in index for the drawing app
	var drawingId = "drawing";
	var _drawing;
	var _surfaceLoaded = false;
	
	
	var stencils = {
		// used for mapping objects between andes
		// and Drawing
		//
		line: 		"drawing.stencil.Line",
		rect: 		"drawing.stencil.Rect",
		ellipse: 	"drawing.stencil.Ellipse",
		vector: 	"drawing.tools.custom.Vector",
		axes: 		"drawing.tools.custom.Axes",
		textBlock:	"drawing.tools.TextBlock"
	};
	
	
	var hasStatement = {
		// These objects get statements associated with them
		//
		"drawing.stencil.Line":true,
		"drawing.stencil.Rect":true,
		"drawing.stencil.Ellipse":true,
		"drawing.tools.custom.Vector":true,
		"drawing.tools.custom.Axes":true
	};
	
	var hasLabel = {
		// Special case for Axes and its double-label
		//
		"drawing.tools.custom.Axes":true
	};
	
	
	
	var getStatementPosition = function(box){
		// summary:
		//	Simple method for determining position of
		// 	statements next to objects. If changed, do not
		// 	change 'showEmpty'.
		//
		var gap = 10;
		return {data:{
			x:box.x2 + gap,
			y:box.y1,
			showEmpty:true
		}};
	};
	
	var items = {};
	
	dojo.addOnLoad(function(){
		_drawing = dijit.byId(drawingId);
		var cn = dojo.connect(_drawing, "onSurfaceReady", function(){
			dojo.disconnect(cn);
			andes.drawing.onSurfaceReady();
		});
		dojo.connect(_drawing, "onRenderStencil", andes.drawing, "onRenderStencil");
		
	});
	
	
	andes.drawing = {
		// summary:
		//	The master object that controls behavior of Drawing items
		//	and handles transfer of data between server and client
		//
		onRenderStencil: function(item){
			// summary:
			//	Called on drag-create. This method should call add()
			//	then save info to the server.
			//
			if(items[item.id]){ return; }
			
			if(hasStatement[item.type] || hasLabel[item.type]){
				// vector, rect, ellipse, or axes
				var box = item.getBounds();
				var props = getStatementPosition(box);
				if(hasLabel[item.type]){
					// axes
					// default labels for an axes
					props.data.text = "x and y";
				}
				// create statement for vector, rect, ellipse, or axes
				var statement = _drawing.addStencil("textBlock", props);
				
				if(hasLabel[item.type]){
					// axes
					var s = statement;
					item.connect(statement, "onChangeText", this, function(value){
						item.setLabel(value);
						console.log("--------------------------------> onNewItem(Axes)", item.id);
						this.add(item, true);
						_drawing.removeStencil(s);
					});
					
				
				}else if(hasStatement[item.type]){
					// vector, rect, ellipse
					var c = new andes.Combo({master:item, statement:statement, onCreate: dojo.hitch(this, function(){
						this.add(c, true);		
					})});
					
				}
			}else{
				// statement or equation
				if(item.isText && andes.defaults.text.deleteEmptyCreate && !item.getText()){
					// no text. will be deleted.
					return;
				}
					console.log("ADD EQU OR STT>>>", item.customType)
					this.add(item, true);
				}
		},
		
		add: function(/* Stencil */ item, /*Boolean*/ saveToServer, /*Boolean*/noConnect){
			// summary:
			//	items added here may be from the server OR drag-created.
			//	They should most often be combo items with andes.Combo,
			// 	with the exception of Axes and (standalone) Statements.
			//
			if(items[item.id]){
				return;
			}
			
			items[item.id] = item;
			
			if(noConnect){
				return;
			}
			
			item.connect("onDelete", this, function(item){
				var id = item.id;
				console.log("--------------------------------> onDelete", id);
				this.remove(item);
				this.save({action:"delete-object", id:item.id});
			});
			
			item.connect("onChangeData", this, function(item){
				console.log("---------------------------------> onChangeData", item.id, item.type);//dojo.toJson(item.data));
				console.log("items:", items)
				var data = andes.convert.drawingToAndes(item, "modify-object")
				console.info("Save to server", data);
				this.save(data);
			});
			
			if(saveToServer){
				// we need to save it to the server
				var data = andes.convert.drawingToAndes(item, "new-object")
				console.info("Save to server:", data);
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
			//	NOTE: andes.help intercepts calls and handles
			//	any help associated with the data.
			//
			console.log("handleServerActions", data.length);
			//console.dir(data);
			var mods = [];
			var min = 2, max = 5;
			dojo.forEach(data, function(obj, i){
				
				if(obj.action =="new-object"){
					var o = andes.convert.andesToDrawing(obj);
					var t = o.stencilType;
					if(t=="vector" || t=="line" || t=="ellipse" || t=="rect"){
						
						// prevent adding items via onRenderStencil
						// by adding the ids first:
						var statementId = _drawing.util.uid("statement");
						var masterId = _drawing.util.uid(t);
						items[statementId] = true;
						items[masterId] = true;
						var statement = _drawing.addStencil("textBlock", o.statement);
						var master = _drawing.addStencil(o.stencilType, o.master);
						var combo = new andes.Combo({master:master, statement:statement, id:o.id});
						this.add(combo);
					
					}else{ // image, statement, equation
						var item = _drawing.addStencil(o.stencilType, o);
						item.andesType = obj.type; // to tell between equation and statement
						this.add(item);
					}
				
				}else if(obj.action=="modify-object"){
					mods.push(obj);
					
				}else if(obj.action=="set-score"){
					andes.help.score(obj.score);
				
				}else{
					console.warn("UNUSED ANDES OBJECT:", obj)
				}
			}, this);
			
			dojo.forEach(mods, function(obj){
				// handles any object modifications
				if(items[obj.id]){
					// style
					items[obj.id].attr(andes.defaults[obj.mode]);
					// x, y
					if(obj.x!==undefined){
						items[obj.id].attr({
							x:obj.x,
							y:obj.y
						});
					}
					// text
					if(obj.text){
						items[obj.id].attr({text:obj.text});
					}
					
				}
			},this);
			
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
			//
			// DEV:
			var devCookie = dojo.fromJson(dojo.cookie("mikeDev"));
			if(devCookie && devCookie.load==false){
				return;
			}
			
			var dfd = andes.api.step(data);
			dfd.addCallback(this, function(data){
				setTimeout(dojo.hitch(this, function(){
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
			// DEV ===================>
			var devCookie = dojo.fromJson(dojo.cookie("mikeDev"));
			if(devCookie && devCookie.load==false){
		
				this._initialData = [
					{"action":"new-object","id":"a18.5","x":200,"y":380,"type":"equation",
"mode":"unknown","text":"Fwall1=?"},

{"action": "modify-object", "id": "a18.5",
"text": "Fwall1 = 15.24609350323404 N"},
				/*{
					"action": "new-object",
					"id": "a6",  
					"type": "vector",
					"mode": "unknown", 
					"x": 240, "y": 222,
					"angle": 120,
					"radius": 0,
					"symbol": "Fwall1",
					"x-statement": 300,
					"y-statement": 350,
					"text": "Fwall1 is the normal force on the ball due to wall1"
				},
				{
					"action": "new-object",
					"id": "a4",  
					"type": "axes",
					"mode": "unknown", 
					"x": 77, "y": 130,
					"angle": 0,
					"radius": 120, 
					"x-label": "x",
					"y-label": "y"
				},
				{
					"action": "new-object",
					"id": "a3",  
					"type": "ellipse",
					"mode": "unknown", 
					"x": 66, "y": 188,
					"width": 25,  "height": 30,
					"text": "ball"
				},
				{
					"action": "new-object",
					"id": "a5",  
					"type": "statement",
					"mode": "unknown", 
					"x": 177, "y": 188,
					"width": 66, "symbol": "m",
					"text": "m is the mass of the ball"
				},
				{
					"action": "new-object",
					"id": "a5b",  
					"type": "equation",
					"mode": "unknown", 
					"x": 177, "y": 230,
					"width": 66, 
					"text": "m=2 kg"
				}*/
				
				];
				// <================ DEV
				
				if(_surfaceLoaded){
					console.log("------------> load actions DEV")
					this.handleServerActions(this._initialData);
				}
				return;
			}
			
			// setting 'this'
			this.loadProject = function(){
				andes.api.open({user:andes.userId, problem:andes.projectId,section:andes.sectionId}).addCallback(this, "onLoad").addErrback(this, "onError");
			}
			if(andes.closeFirst){
				// a previous project session is open. close it.
				andes.api.close({}).addCallback(this, "loadProject").addErrback(this, "onError");
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
			}
		},
		onError: function(err){
			console.error("There was an error in the project data:", err);
			if(!this._initialData){
				// apparently an error on open-problem. Try closing session.
				andes.api.close({});
				dojo.cookie("andes", null, { expires: -1 });
			}
			// call andes.error?
		}
	};
	
})();
