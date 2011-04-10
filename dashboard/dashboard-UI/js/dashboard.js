dojo.require("dojox.charting.Chart");
dojo.require('dojox.charting.widget.Chart');
dojo.require("dojox.charting.themes.BlueDusk");
dojo.require("dojox.charting.action2d.Highlight");
dojo.require("dojox.charting.action2d.Tooltip");

var asst, flag;
document.$_GET = [];

var json = 	{
				assignment_labels:
				[	
					{ value: 1, text: "ASST1" }, { value: 2, text: "ASST2" }, { value: 3, text: "ASST3" }, { value: 4, text: "ASST4" },
					{ value: 5, text: "ASST5" }, { value: 6, text: "ASST6" }, { value: 7, text: "ASST7" }, { value: 8, text: "ASST8" },
				],
				kc_labels:
				[	
					{ value: 1, text: "KC1" }, { value: 2, text: "KC2" }, { value: 3, text: "KC3" }, { value: 4, text: "KC4" },
					{ value: 5, text: "KC5" }, { value: 6, text: "KC6" }, { value: 7, text: "KC7" }, { value: 8, text: "KC8" },
				],
				assignment_data: [85, 75, 80, 65, 70, 55, 40, 25],
				kc_data: [75, 85, 65, 80, 55, 70, 25, 40],
			}

dojo.addOnLoad(
	function() {
		
		
		var urlHalves = String(document.location).split('?');
		
		if(urlHalves[1])
		{
		  var urlVars = urlHalves[1].split('&');
		  for(var i=0; i<=(urlVars.length); i++)
		  {
		     if(urlVars[i])
			 {
		        var urlVarPair = urlVars[i].split('=');
		        document.$_GET[urlVarPair[0]] = urlVarPair[1];
		     }
		  }
		}
	   
		flag = (document.$_GET['user'] == null || document.$_GET['section'] == null);
		asst = (document.$_GET['asst'] != null);
		
		/*var chart = new dojox.charting.Chart2D("chart");
		chart.setTheme(dojox.charting.themes.BlueDusk);
		
		if(asst)
		{
			chart.addAxis("x", {labels: json['kc_labels'], minorTicks: false});
		}
		else
		{
			chart.addAxis("x", {labels: json['assignment_labels'], minorTicks: false});
		}
		
		chart.addAxis("y", {vertical: true, min: 0, max: 100});
		chart.addPlot("default", {type: "Columns", gap: 8 });
		
		if(asst)
		{
			chart.addSeries("Mastery", json['kc_data']);
		}
		else
		{
			chart.addSeries("Mastery", json['assignment_data']);
		}

		new dojox.charting.action2d.Highlight(chart, "default");
		new dojox.charting.action2d.Tooltip(chart, 'default');
		
		chart.connectToPlot("default", function(args)
		{
			switch(args.type) 
			{
				case "onclick":
					var i = 0;
					for (i = 0; i < 8; i++)
					{
						if (args.index == i && !asst) 
						{
							window.location = document.URL + "&asst=ASST" + (i + 1);
						}
					}
					break;
				case "onmouseover":
					break;
				case "onmouseout":
					break;
				default:
					break;
			}
		});
		
		chart.render();*/
});