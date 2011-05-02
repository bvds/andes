dojo.require("dojox.charting.Chart");
dojo.require("dojox.charting.axis2d.Default")
dojo.require("dojox.charting.plot2d.Columns")
dojo.require("dojox.charting.plot2d.ClusteredBars")
dojo.require("dojox.charting.widget.Chart");
dojo.require("dojox.charting.themes.BlueDusk");
dojo.require("dojox.charting.action2d.Highlight");
dojo.require("dojox.charting.action2d.Tooltip");

var response = {
	"API-Version" : "1",
	"Timestamp" : "2010-12-01T11:00",
	"SectionId" : "andestutor.org",
	"StudentList":
	[
		{
			"StudentId" : "bja",
			"AssignmentList":
			[
				{
					"AssignmentId" : "ASST1",
					"KCList":
					[
						{"Name" : "draw-vector-aligned-axes", "MasteryLevel" : .85, "ChancesToUse" : 54},
						{"Name" : "draw-bodys", "MasteryLevel" : .74, "ChancesToUse" : 37},
						{"Name" : "draw-applied-force","MasteryLevel" : .68, "ChancesToUse" : 42}
					]
				},
				{
					"AssignmentId" : "ASST2",
					"KCList":
					[
						{"Name" : "draw-vector-aligned-axes", "MasteryLevel" : .85, "ChancesToUse" : 54},
						{"Name" : "draw-bodys", "MasteryLevel" : .74, "ChancesToUse" : 37},
						{"Name" : "draw-applied-force","MasteryLevel" : .68, "ChancesToUse" : 42}
					]
				}
			]
		}
	]
}

var chart_labels = [], chart_data = [];

function parse()
{
	// get all info
	if(!asst)
	{
		var index = 1;
		for (a in response['StudentList'][0]['AssignmentList'])
		{
			var total = 0, count = 0;
			
			for (kc in response['StudentList'][0]['AssignmentList'][a]['KCList'])
			{
				total += response['StudentList'][0]['AssignmentList'][a]['KCList'][kc]['MasteryLevel'] * 100;
				count++;
			}
			
			chart_data.push(total/count);
			chart_labels.push({ value: index, text: response['StudentList'][0]['AssignmentList'][a]['AssignmentId'] });
			
			index++;
		}
	}
	// get KC info
	else
	{	
		var index = 1;
		var asst_index = parseInt(get['asst']);
		
		for (kc in response['StudentList'][0]['AssignmentList'][asst_index]['KCList'])
		{
			chart_data.push(response['StudentList'][0]['AssignmentList'][asst_index]['KCList'][kc]['MasteryLevel'] * 100);
			chart_labels.push({ value: index, text: response['StudentList'][0]['AssignmentList'][asst_index]['KCList'][kc]['Name'] });
			index++;
		}	
	}
}

dojo.addOnLoad(
	function() {
		// create a new chart with id 'chart' and set the theme to BlueDusk
		var chart = new dojox.charting.Chart("chart");
		chart.setTheme(dojox.charting.themes.BlueDusk);
		
		// parse JSON data for desired information
		parse();
		
		chart.addAxis("x", {labels: chart_labels, minorTicks: false});
		
		chart.addAxis("y", {vertical: true, min: 0, max: 100});
		chart.addPlot("default", {type: "Columns", gap: 8 });
		
		chart.addSeries("Mastery", chart_data);

		new dojox.charting.action2d.Highlight(chart, "default");
		new dojox.charting.action2d.Tooltip(chart, "default");
		
		chart.connectToPlot("default", function(args)
		{
			if(!asst)
				switch(args.type) 
				{
					case "onclick":
						var i = 0;
						for (i = 0; i < 8; i++)
						{
							if (args.index == i) 
							{
								window.location = document.URL + "&asst=" + (i + 1);
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
		
		chart.render();
});

