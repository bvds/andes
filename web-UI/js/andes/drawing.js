dojo.provide("andes.drawing");


(function(){
	var theme = {
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
		statement:"text",
		equation:"text",
		graphics:"image"
	};
	
	
	var stencils = {
		line: "drawing.stencil.Line",
		rect: "drawing.stencil.Rect",
		ellipse: "drawing.stencil.Ellipse",
		vector: "drawing.tools.custom.Vector",
		axes: "drawing.tools.custom.Axes",
		textBlock:"drawing.tools.TextBlock"
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
			var d = drawing.defaults;
			d.norm.color = theme.unknown.color;
			d.norm.fill = theme.unknown.fill;
			d.text.minWidth = theme.text.minWidth;
			d.text.size = theme.text.size;
			
			andes.drawing.onSurfaceReady();
		});
		dojo.connect(_drawing, "onRenderStencil", andes.drawing, "onRenderStencil");
		
	});
	andes.drawing = {
		
		onRenderStencil: function(item){
			if(items[item.id]){ return; }
			
			console.log("drawing, new item:", item, "make label:", item.type);
		
			this.add(item);
			//item.disable();

			if(hasStatement[item.type] || hasLabel[item.type]){
				var box = item.getBounds();
				var props = getStatementPosition(box);
				var statement = _drawing.addStencil("textBlock", props);
				
				if(hasLabel[item.type]){
					var s = statement;
					item.connect(statement, "onChangeText", function(value){
						item.setLabel(value);
						_drawing.removeStencil(s);
					});
					
				}else if(hasStatement[item.type]){
					this.add(statement);
					var c = new drawing.stencil._Connection(item);
					item.connection = c;
					statement.connection = c;
					c.statement = statement;
					c.add(statement, [
						// master connects (none)			  
					], [
						// statement connects
						["onChangeText", function(value){
							item.setLabel(andes.variablename.parse(value));
							c.attr(getDevTheme());
						}]
					]);
				}
			}
		},
		
		add: function(/* Stencil */ item){
			items[item.id] = item;
			item.connect("onDelete", this, function(item){
				console.log("--------------------------------> onDelete", item.id);
				this.remove(item);
			});
			
			item.connect("onModify", this, function(item){
				//console.log("onModify", item.id);
			});
			item.connect("onChangeText", this, function(value){
				console.log("---------------------------------> onChangeText", value);
			});
			item.connect("onChangeData", this, function(item){
				console.log("---------------------------------> onChangeData", item.id, dojo.toJson(item.data));
			});
		},
		
		remove: function(/* Stencil */ item){
			console.log("---------------------------------> onDelete", item.id, "connection:", !!item.connection);
			delete items[item.id];
		},
		
		
		loadProjectData: function(data){
			dojo.forEach(data, function(obj){
				if(obj.action =="new-object"){
					if(obj.href) { obj.src = obj.href; }
					var item = _drawing.addStencil(stencilMods[obj.type], {data:obj});
					items[item.id] = {
						item:item
					}
				};
			});
			data = null;
		},
		
		onSurfaceReady: function(){
			//_drawing.addStencil("image", {data:{src:"tests/images/BallOnWall.png", x:300, y:200, width:"auto"}});
			//_drawing.addStencil("rect", {data:{x:100, y:500, width:100, height:100}});
			
			_surfaceLoaded = true;
			if(this._initialData){
				this.loadProjectData(this._initialData);
				this._initialData = null;
			}
		},
		
		load: function(){
			// called from the very bottom of main.js
			//return;
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
			
			//DEV============================= >
		 /*
			this._initialData = andes.drawing._initialData.reverse()
			
			var y=30, h = 25;
			dojo.forEach(this._initialData, function(obj, i){
				if(obj.action =="new-object"){
					if(i===0){
						y = obj.y
					}else{
						obj.y = y;
						y += obj.height || h;
					}
				        console.log("OBJECT:", obj);
				}
			});
			
			console.dir(data);
			andes.api.close();
		*/	
			// < ============================DEV 

			if(_surfaceLoaded){
				this.loadProjectData(this._initialData);
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