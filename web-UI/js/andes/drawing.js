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
	
	// better name
	var stencilMods = {
		statement:	"textBlock",
		equation:	"textBlock",
		graphics:	"image",
		vector:		"vector",
		axes:		"axes",
		ellipse:	"ellipse"
	};
	
	//"enum": ["statement", "graphics", "equation", "circle", "ellipse", "rectangle", "axes", "vector", "line"],
	  
	var stencils = {
		line: 		"drawing.stencil.Line",
		rect: 		"drawing.stencil.Rect",
		ellipse: 	"drawing.stencil.Ellipse",
		vector: 	"drawing.tools.custom.Vector",
		axes: 		"drawing.tools.custom.Axes",
		textBlock:	"drawing.tools.TextBlock"
	};
	
	var andesTypes = {
		"drawing.stencil.Line":"line",
		"drawing.stencil.Rect":"rectangle",
		"drawing.stencil.Ellipse":"ellipse", // or circle
		"drawing.tools.custom.Vector":"vector",
		"drawing.tools.custom.Axes":"axes",
		"drawing.tools.custom.Equation":"equation",
		"drawing.stencil.Image":"graphics",
		"drawing.tools.TextBlock":"statement" // or statement.... hmmmm
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
					var c = new andes.Combo(item, statement);
					this.add(c, true);
				}
			}else{
				// statement or equation
				this.add(item, true);
			}
		},
		
		add: function(/* Stencil */ item, /*Boolean*/ saveToServer, /*Boolean*/noConnect){
			// summary:
			//	items added here may be from the server OR drag-created.
			//	They should most often be combo items with _Connection,
			// 	with the exception of Axes and (standalone) Statements.
			//
			//console.log("ADD ITEM", item);
			
			if(items[item.id]){
				//console.log("ITEM EXISTS:", item.id)
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
				var data = this.transform.drawingToAndes(item, "modify-object")
				console.info("Save to server", data);
				this.save(data);
			});
			
			if(saveToServer){
				// we need to save it to the server
				var data = this.transform.drawingToAndes(item, "new-object")
				console.info("Save to server:", data);
				this.save(data);
			}
		},
		
		remove: function(/* Stencil */ item){
			delete items[item.id];
		},
		
		transform:{
			andesToDrawing: function(o){
				//console.warn(" ---------------> andesToDrawing:", o.type)
				if(o.x==undefined || o.y===undefined){
					console.error("Imported Object '" + o.id + "' contains no X or Y coordinates.");
					console.warn("Bad Imported object:", o);
				}
				var obj = {
					id:o.id,
					stencilType:stencilMods[o.type],
					data:{
						x:o.x,
						y:o.y
					},
					enabled:o.mode!="locked"
				};
				
				if(o.type!="vector" && o.type!="line" && o.type!="axes" && o.type!="ellipse"){
					obj.data.width = o.width;
					obj.data.height = o.height;
				
				}else if(o.type=="ellipse"){
					obj.data = {
						cx:o.x + o.width/2,
						cy:o.y + o.height/2,
						rx:o.width/2,
						ry:o.height/2
					}
				}else{
					// vector, line, axes
					obj.data.radius = o.radius || 100;
					obj.data.angle = o.angle;
				}
				if(o.type=="statement" && (o.mode=="locked" || o.mode=="fade")){
					obj.stencilType = "text";
				}
				
				if(o.type=="line" || o.type=="vector" || o.type=="rect" || o.type=="ellipse"){
					// seperate objects
					var lbl = o.symbol;
					var txt = o.text;
					if(!lbl){
						lbl = txt;
						txt = "";
					}
					// master
					obj.master = {
						data:obj.data,
						label:lbl
					};
					var xs = o['x-statement'];
					var ys = o['y-statement'];
					
					if(xs === undefined){
						var pt = getStatementPosition({
							y1:o.y,
							x2:o.x+o.width
						}).data;
						xs = pt.x;
						ys = pt.y;
					}
					obj.statement = {
						data:{
							x:xs,
							y:ys,
							text:txt,
							width:"auto"
						}
					}
				}else if(o.type=="statement" || o.type=="equation"){
					obj.data.text = o.text;
				}else if(o.type=="axes"){
					obj.label = o['x-label']+" and "+o['y-label'];
				}
				
				
				
				if(o.type=="vector"){
					if(obj.master.label=="a"){
						console.warn("VECTOR OBJ: 'a':::", o);	
					}
				}
				
				
				
				if(o.href){
					obj.data.src = o.href;
				}
				return obj;
			},
			drawingToAndes: function(item, action){
				
				var round = function(b){
					for(var nm in b){
						b[nm] = Math.round(b[nm]);
					}
					return b;
				}
				// combo...............
				var combo, statement, sbox, id = item.id;
				if(item.type=="andes.Combo"){
					statement = item.statement;
					item = item.master;
					combo = true;
					sbox = round(item.getBounds());
				}
				var type = andesTypes[item.type];
				if(type=="statement" && item instanceof drawing.tools.custom.Equation){
					type = "equation";
				}
				var box = round(item.getBounds(true));
				var obj = {
					x:box.x,
					y:box.y,
					action:action,
					type:type,
					id:id,
					mode: "unknown"
				}
				
				if(type!="vector" && type!="line" && type!="axes"){
					obj.width = box.w;
					obj.height = box.h;
				}else if(type!="axes"){
					var line = {start:{x:box.x1, y:box.y1}, x:box.x2, y:box.y2};
					obj.radius = Math.round(_drawing.util.length(line));
					obj.angle = item.angle;
				}
				
				if(type == "statement" || type == "equation"){
					obj.text = item._text || "SHOULD NOT BE HERE";
					if(type == "statement"){
						// need to add a potential 'symbol' derived from variablename.js
						var symbol = andes.variablename.parse(obj.text);
						if(symbol){
							obj.symbol = symbol;
						}
					}
				}else if(type != "axes"){
					obj.text = statement._text || "SHOULD NOT BE HERE";
					obj.symbol = item.getLabel() || "";
					obj["x-statement"] = sbox.x;
					obj["y-statement"] = sbox.y;
				
				}else if(type == "axes"){
					var lbl = item.getLabel();
					obj["x-label"] = lbl.x;
					obj["y-label"] = lbl.y;
					
					var line = {start:{x:box.x1, y:box.y1}, x:box.x2, y:box.y2};
					obj.radius = Math.round(_drawing.util.length(line));
					obj.angle = item.angle;
				}
				
				if(combo){
					var txt = statement._text || "";
					var lbl = item.getLabel() || "";
					if(txt && lbl){
						obj.text = txt;
						obj.symbol = lbl;	
					}else{
						obj.text = txt;
						obj.symbol = "";
					}
					
				
				}
				
				return obj;
			}
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
					var o = this.transform.andesToDrawing(obj);
					var t = o.stencilType;
					
					//
					// add ID to items first?
					// make combo item create items within it?
					//
					
					if(t=="vector" || t=="line" || t=="ellipse" || t=="rect"){
						
						// prevent adding items via onRenderStencil
						// by adding the ids first:
						var statementId = _drawing.util.uid("statement");
						var masterId = _drawing.util.uid(t);
						items[statementId] = true;
						items[masterId] = true;
						// statement:
						var statement = _drawing.addStencil("textBlock", o.statement);
						// vector:	
						var master = _drawing.addStencil(o.stencilType, o.master);
						// combo:
						var combo = new andes.Combo(master, statement, o.id);
						this.add(combo);
					
						
					}else{ // image, statement, equation
						//if(o.stencilType=="image") return;
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
					//console.warn("MOD:", obj); //a3 a4
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
			var devCookie = dojo.fromJson(dojo.cookie("mikeDev"));
			if(devCookie && devCookie.load==false){
		//return;
				this._initialData = [
			/*	{
					"action": "new-object",
					"id": "a6",  
					"type": "vector",
					"mode": "unknown", 
					"x": 240, "y": 222,
					"angle": 120,
					"radius": 160,
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

/*
 var cb = function(){ console.warn("IN CALLBACK"); };
var eb = function(){ console.error("IN ERRBACK"); };

andes.api.open({user:"joe", problem:"s2e"}).addCallbacks(cb,eb);

andes.api.step({action:"new-object", id:"a3", type:"circle", mode:"unknown", x:66, y:188, radius:25, text:"ball"});

andes.api.close().addCallbacks(cb,eb);
*/