dojo.provide("andes.drawing");


(function(){
	
	//dojo.cookie("mikeDev", "{load:true}", { expires: 999 });

	var theme = {
		DTheta:1,
		locked:{
			//TODO
		},
		fade:{
			//TODO
		},
		correct:{
			fill:  "#CCFFCC",
			color: "#009900"
		},
		incorrect:{
			fill:  "#FE7070",
			color: "#D20202"
		},
		unknown:{
			fill:  "#CCCCCC",
			color: "#000000"
		},
		text:{
			size:"14px",
			minWidth:100
		}
	};
	
	var dtc = 0;
	var getDevTheme = function(){
		dtc = dtc==2 ? 0 : dtc + 1;
		if(dtc==1){
			return theme.correct;
		}else if(dtc==2){
			return theme.incorrect;
		}
		return theme.unknown;
	};
	
	var drawingId = "drawing";
	var _drawing;
	var _surfaceLoaded = false;
	
	var stencils = {
		line: 		"drawing.stencil.Line",
		rect: 		"drawing.stencil.Rect",
		ellipse: 	"drawing.stencil.Ellipse",
		vector: 	"drawing.tools.custom.Vector",
		axes: 		"drawing.tools.custom.Axes",
		textBlock:	"drawing.tools.TextBlock"
	};
	
	var hasStatement = {
		"drawing.stencil.Line":true,
		"drawing.stencil.Rect":true,
		"drawing.stencil.Ellipse":true,
		"drawing.tools.custom.Vector":true,
		"drawing.tools.custom.Axes":true
	};
	
	var hasLabel = {
		"drawing.tools.custom.Axes":true
	};
	
	var items = {};
	
	var getStatementPosition = function(box){
		var gap = 10;
		return {data:{
			x:box.x2 + gap,
			y:box.y1,
			showEmpty:true
		}};
	};
	
	
	dojo.addOnLoad(function(){
		_drawing = dijit.byId(drawingId);
		var cn = dojo.connect(_drawing, "onSurfaceReady", function(){
			dojo.disconnect(cn);
			// setting styles
			var d = drawing.defaults;
			d.angleSnap = theme.DTheta;
			d.norm.color = theme.unknown.color;
			d.norm.fill = theme.unknown.fill;
			d.text.minWidth = theme.text.minWidth;
			d.text.size = theme.text.size;
			d.textDisabled.size = theme.text.size;
			
			andes.drawing.onSurfaceReady();
		});
		dojo.connect(_drawing, "onRenderStencil", andes.drawing, "onRenderStencil");
		
	});
	andes.drawing = {
		
		onRenderStencil: function(item){
			// summary:
			//	Called on drag-create. This method should call add()
			//	then save info to the server.
			//
			if(items[item.id]){ return; }
			
			console.log("-----------> drawing, new item:", item.id, items);
		
			
			//item.disable();

			if(hasStatement[item.type] || hasLabel[item.type]){
				
				var box = item.getBounds();
				var props = getStatementPosition(box);
				if(hasLabel[item.type]){
					props.data.text = "x and y";
				}
				var statement = _drawing.addStencil("textBlock", props);
				
				if(hasLabel[item.type]){
					var s = statement;
					item.connect(statement, "onChangeText", this, function(value){
						item.setLabel(value);
						console.log("--------------------------------> onNewItem(Axes)", item.id);
						this.add(item, true);
						_drawing.removeStencil(s);
					});
					
				}else if(hasStatement[item.type]){
					var c = new andes.Combo({master:item, statement:statement, onCreate: dojo.hitch(this, function(){
						this.add(c, true);		
					})});
					
				}
			}else{
				// statement or equation
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
			delete items[item.id];
		},
		
		
		handleServerActions: function(data){
			console.log("handleServerActions", data.length);
			//console.dir(data);
			var mods = [];
			var min = 2, max = 5;
			dojo.forEach(data, function(obj, i){
				if(obj.action =="new-object"){
					//if(i<min || i>max) return;
					//console.warn("ANDES OBJECT V"); console.dir(obj);
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
						this.add(item);
					}
				
				}else if(obj.action=="modify-object"){
					mods.push(obj);
				}else if(obj.action=="set-score"){
					andes.help.score(obj.score);
				}
			}, this);
			
			dojo.forEach(mods, function(obj){
				if(items[obj.id]){
					items[obj.id].attr(theme[obj.mode]);
					if(obj.x!==undefined){
						items[obj.id].attr({
							x:obj.x,
							y:obj.y
						});
					}
				}
			},this);
			
			data = null;
		},
		
		onSurfaceReady: function(){
			//_drawing.addStencil("image", {data:{src:"tests/images/BallOnWall.png", x:300, y:200, width:"auto"}});
			//_drawing.addStencil("rect", {data:{x:100, y:500, width:100, height:100}});
			
			_surfaceLoaded = true;
			if(this._initialData){
				console.log("------------> load actions surface loaded")
				this.handleServerActions(this._initialData);
				this._initialData = null;
			}
		},
		
		save: function(data){
			var devCookie = dojo.fromJson(dojo.cookie("mikeDev"));
			if(devCookie && devCookie.load==false){
				return;
			}
			andes.api.step(data).addCallback(this, "handleServerActions").addErrback(this, "onError");	
		},
		
		load: function(){
			// called from the very bottom of main.js
			//
			var devCookie = dojo.fromJson(dojo.cookie("mikeDev"));
			if(devCookie && devCookie.load==false){
		//return;
				this._initialData = [
				{
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
				/*{
					"action": "new-object",
					"id": "a4",  
					"type": "axes",
					"mode": "unknown", 
					"x": 77, "y": 130,
					"angle": 0,
					"radius": 120, 
					"x-label": "x",
					"y-label": "y"
				},*/
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
				}
				
				
				
				];
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
				andes.api.close({}).addCallback(this, "loadProject").addErrback(this, "onError");
			}else{
				this.loadProject();
			}
		},
		
		onLoad: function(data){
			console.info("Project Data Loaded");
			this._initialData = data;
			if(_surfaceLoaded){
				console.log("------------> load actions onLoad")
				this.handleServerActions(this._initialData);
				this._initialData = null;
			}
		},
		onError: function(err){
			console.error("There was an error loading project data:", err);
			andes.api.close({});
			dojo.cookie("andes", null, { expires: -1 });
		}
	};
	
})();
