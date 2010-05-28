dojo.provide("andes.convert");

(function(){

	var stencilMods = {
		statement:	"textBlock",
		equation:	"textBlock",
		graphics:	"image",
		vector:		"vector",
		axes:		"axes",
		ellipse:	"ellipse",
		rectangle:	"rect",
		line:		"line",
		done:		"button",
		checkbox:	"button",
		radio:		"button"
	};

	var andesTypes = {
		"dojox.drawing.stencil.Line":"line",
		"dojox.drawing.stencil.Rect":"rectangle",
		"dojox.drawing.stencil.Ellipse":"ellipse", // or circle
		"dojox.drawing.tools.custom.Vector":"vector",
		"dojox.drawing.tools.custom.Axes":"axes",
		"dojox.drawing.tools.custom.Equation":"equation",
		"dojox.drawing.stencil.Image":"graphics",
		"dojox.drawing.tools.TextBlock":"statement", // or statement.... hmmmm
		"andes.buttonCombo":"button"
	};

	// dupe code:
	var getStatementPosition = function(box){
		var gap = 10;
		return {data:{
			x:box.x2 + gap,
			y:box.y1,
			showEmpty:true
		}};
	};
	
	andes.convert = {
		// summary:
		//	The conversion object used to transform objects
		//	from and ande object to a drawing object and
		//	vice versa.
		//
		andesToDrawing: function(o){
			// summary:
			//	Converts from andes to drawing
			
			//console.warn(" ---------------> andesToDrawing:", o.type,o.id)
			// Group of objects:  call conversion recursively
			if(o.items) {
				var obj = {
					id:o.id,
					type:o.type,
					itemType:o.items[0].type,
					items:dojo.map(o.items,andes.convert.andesToDrawing),
					checked: o.checked || [] 
				}
				return obj;
			}
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
			
			var buttonWidth;
			// o.type can be:  statement, graphics, radio, done, checkbox, axes, ellipse, 
			//                 vector, equation, line, rectangle
			if(o.type=="statement" || o.type=="equation" || o.type=="graphics" || o.type=="rectangle"){
				obj.data.width = o.width;
				obj.data.height = o.height;
				
			}else if(o.type=="ellipse"){
				obj.data = {
					cx:o.x + o.width/2,
					cy:o.y + o.height/2,
					rx:o.width/2,
					ry:o.height/2
				}
			}else if(o.type=="radio"){
				buttonWidth=andes.defaults.button.radioButtonRadius;
				obj.buttonType=o.type;
				obj.data={
					cx:o.x + 0.5*buttonWidth,
					cy:o.y + 0.5*buttonWidth,
					rx:0.5*buttonWidth,
					ry:0.5*buttonWidth
					// x:o.x, y:o.y, height:20, width:40
				};
				obj.value = o.value;
			}else if(o.type=="checkbox"){
				buttonWidth=andes.defaults.button.checkboxWidth;
				obj.buttonType=o.type;
				obj.data.width = buttonWidth;
				obj.data.height = buttonWidth;
				obj.value = o.value;
			}else if(o.type=="done"){
				buttonWidth=o.label.length*10;  // should be determined by actual drawn width
				obj.buttonType=o.type;
				obj.data.width = 40;
				obj.data.height = 20;
				obj.icon={
					type: "text",
					text: o.label
				};
			}else if(o.type=="vector"){
				//in case of zero vector
				if(o.radius == 0) {obj.data.radius = 0; obj.data.angle = 1; } else { obj.data.radius = o.radius; obj.data.angle = o.angle; }
				obj.data.cosphi = o.cosphi;

			}else if(o.type=="line" || o.type=="axes"){
				obj.data.radius = o.radius || 0;
				obj.data.angle = o.angle;
				obj.data.cosphi = o.cosphi || 0;
			}else{
				console.warn("Unrecognized type ",o.type);
			}

			if(o.type=="statement" && o.mode=="locked"){
				obj.stencilType = "text";
			}

			if(o.type=="done" || o.type=="checkbox" || o.type=="radio"){
				// In principle, we could omit the statement object if there is no text,
				// but the code gets a lot hairier.
				obj.statement = {
					data:{
						x:o.x+buttonWidth,
						y:o.y,
						text:o.text || ""
					},
					enabled: false  // treat as mode=locked
				}
			}else if(o.type=="line" || o.type=="vector" || o.type=="rectangle" || o.type=="ellipse"){
				// separate objects
			        // match logic in drawingToAndes
				var lbl = o.symbol;
				var txt = o.text || "";
				// if there is no symbol, use text for label
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
						text:txt
						//width:"auto"- for cases where the text is "" width:auto makes init fail
						//showEmpty:true
					},
					deleteEmptyCreate: false,
					deleteEmptyModify: false
				}
			}else if(o.type=="statement" || o.type=="equation"){
				obj.data.text = o.text;
			}else if(o.type=="axes"){
				obj.label = o['x-label']+" and "+o['y-label'];
				if(andes.defaults.zAxisEnabled){
					obj.label += " and "+o['z-label'];
				}
			}
			
			if(o.href){
				obj.data.src = o.href;
			}
			return obj;
		},
		
		drawingToAndes: function(item, action){
			// summary:
			//	Converts from Drawing to andes
			//
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
				sbox = round(statement.getBounds());
			}
			var type = item.andesType || item.customType || andesTypes[item.type];

			if(type=="button"){
				if(!item.group){
					console.warn("drawingToAndes: invalid button object ",item);
				}
				var obj = {
					id:item.group.id,
					type:item.group.type,
					action:action,
					checked:item.group.checked
				}
				return obj;
			};
			
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
				obj.radius = Math.ceil(item.getRadius());
				obj.angle = item.getAngle();
			}
			
			if(type == "statement" || type == "equation"){
				obj.text = item.getText() || "";
				if(type == "statement"){
					// need to add any 'symbol' derived from variablename.js
					obj.symbol = andes.variablename.parse(obj.text);
				}
			}else if(type != "axes"){
				obj.text = statement.getText() || " ";
				obj.symbol = item.getLabel() || null;
				obj["x-statement"] = sbox.x;
				obj["y-statement"] = sbox.y;
				
			}else if(type == "axes"){
				var lbl = item.getLabel();
				obj["x-label"] = lbl.x;
				obj["y-label"] = lbl.y;
				if(lbl.z) {
					obj["z-label"] = lbl.z;
				}
				obj.radius = Math.ceil(item.getRadius());
				obj.angle = item.getAngle();
			}
			
			if(type=="vector" || type=="axes"){
				obj.cosphi = item.cosphi;	
			}

			if(combo){
				// match logic in andesToDrawing
				// Send empty string, rather than null
				// The server treats null as "not modified".
				var txt = statement.getText();
				var lbl = item.getLabel() || "";
				if(txt){
					obj.text = txt;
					obj.symbol = lbl;
				}else{
					obj.text = lbl;
					obj.symbol = "";
				}
			}
			
			return obj;
		}
	}
	
})();