dojo.require("dojox.charting.Chart");
dojo.require("dojox.charting.axis2d.Default")
dojo.require("dojox.charting.plot2d.Columns")
dojo.require("dojox.charting.plot2d.Bars")
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
						{"Name" : "draw-vector-aligned-axes", "MasteryLevel" : .65, "ChancesToUse" : 54},
						{"Name" : "draw-bodys", "MasteryLevel" : .74, "ChancesToUse" : 37},
						{"Name" : "draw-applied-force","MasteryLevel" : .56, "ChancesToUse" : 42}
					]
				},
				{
					"AssignmentId" : "ASST2",
					"KCList":
					[
						{"Name" : "draw-vector-aligned-axes", "MasteryLevel" : .85, "ChancesToUse" : 54},
						{"Name" : "draw-bodys", "MasteryLevel" : .42, "ChancesToUse" : 37},
						{"Name" : "draw-applied-force","MasteryLevel" : .39, "ChancesToUse" : 42}
					]
				},
				{
					"AssignmentId" : "ASST3",
					"KCList":
					[
						{"Name" : "draw-vector-aligned-axes", "MasteryLevel" : .85, "ChancesToUse" : 54},
						{"Name" : "draw-bodys", "MasteryLevel" : .42, "ChancesToUse" : 37},
						{"Name" : "draw-applied-force","MasteryLevel" : .39, "ChancesToUse" : 42}
					]
				}
			]
		},
		{
			"StudentId" : "kvl",
			"AssignmentList":
			[
				{
					"AssignmentId" : "ASST1",
					"KCList":
					[
						{"Name" : "draw-vector-aligned-axes", "MasteryLevel" : .42, "ChancesToUse" : 54},
						{"Name" : "draw-bodys", "MasteryLevel" : .74, "ChancesToUse" : 37},
						{"Name" : "draw-applied-force","MasteryLevel" : .58, "ChancesToUse" : 42}
					]
				},
				{
					"AssignmentId" : "ASST2",
					"KCList":
					[
						{"Name" : "draw-vector-aligned-axes", "MasteryLevel" : .85, "ChancesToUse" : 54},
						{"Name" : "draw-bodys", "MasteryLevel" : .56, "ChancesToUse" : 37},
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
	// for chart labels
	var index = 1;
	
	// Student
	if(student) {
		// Student Top Level
		if(get['asst'] == null) {
			for (a in response['StudentList'][0]['AssignmentList']) {
				var total = 0, count = 0;
				
				for (kc in response['StudentList'][0]['AssignmentList'][a]['KCList']) {
					total += response['StudentList'][0]['AssignmentList'][a]['KCList'][kc]['MasteryLevel'] * 100;
					count++;
				}
				
				chart_data.push(total/count);
				chart_labels.push({ value: index, text: response['StudentList'][0]['AssignmentList'][a]['AssignmentId'] });
				
				index++;
			}
		} else {
		// Student Assignment Level
			var asst_index = parseInt(get['asst']);
			
			for (kc in response['StudentList'][0]['AssignmentList'][asst_index]['KCList']) {
				chart_data.push(response['StudentList'][0]['AssignmentList'][asst_index]['KCList'][kc]['MasteryLevel'] * 100);
				chart_labels.push({ value: index, text: response['StudentList'][0]['AssignmentList'][asst_index]['KCList'][kc]['Name'] });
				index++;
			}	
		}
	// Teacher
	} else { 
		// Teacher Top Level
		if (get['user'] == null && get['asst'] == null) {
			for (s in response['StudentList']) {
				var total = 0, count = 0;
				
				for (a in response['StudentList'][s]['AssignmentList']) {
					for (kc in response['StudentList'][s]['AssignmentList'][a]['KCList']) {
						total += response['StudentList'][s]['AssignmentList'][a]['KCList'][kc]['MasteryLevel'] * 100;
						count++;
					}
				}
				
				chart_data.push(total/count);
				chart_labels.push({ value: index, text: response['StudentList'][s]['StudentId'] });
				
				index++;
			}
		} else if (get['asst'] == null) {
		// Teacher Student Level
			for(s in response['StudentList']) {
				if(response['StudentList'][s]['StudentId'] == get['user'])
					stud_id = s;
			}
		
			for (a in response['StudentList'][stud_id]['AssignmentList']) {
				var total = 0, count = 0;
				
				for (kc in response['StudentList'][stud_id]['AssignmentList'][a]['KCList']) {
					total += response['StudentList'][stud_id]['AssignmentList'][a]['KCList'][kc]['MasteryLevel'] * 100;
					count++;
				}
				
				chart_data.push(total/count);
				chart_labels.push({ value: index, text: response['StudentList'][stud_id]['AssignmentList'][a]['AssignmentId'] });
				
				index++;
			}
		} else {
			for(s in response['StudentList']) {
				if(response['StudentList'][s]['StudentId'] == get['user'])
					stud_id = s;
			}
		
			var asst_index = parseInt(get['asst']);
			
			for (kc in response['StudentList'][stud_id]['AssignmentList'][asst_index]['KCList']) {
				chart_data.push(response['StudentList'][stud_id]['AssignmentList'][asst_index]['KCList'][kc]['MasteryLevel'] * 100);
				chart_labels.push({ value: index, text: response['StudentList'][0]['AssignmentList'][asst_index]['KCList'][kc]['Name'] });
				index++;
			}			
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
		
		// fill in chart
		if(!student && get['user'] == null && get['asst'] == null) {
			chart.addAxis("x", {min: 0, max: 100});
			chart.addAxis("y", {vertical: true, labels: chart_labels, minorTicks: false});
			chart.addPlot("default", {type: "Bars", gap: 8 });
			chart.addSeries("", chart_data);
		} else {
			chart.addAxis("x", {labels: chart_labels, rotation: 20, minorTicks: false});
			chart.addAxis("y", {vertical: true, min: 0, max: 100});
			chart.addPlot("default", {type: "Columns", gap: 8 });
			chart.addSeries("", chart_data);
		}

		// add mouse over and tooltip actions
		new dojox.charting.action2d.Highlight(chart, "default");
		new dojox.charting.action2d.Tooltip(chart, "default");
		
		// create links to other pages if on top level
		chart.connectToPlot("default", function(args) {
			switch (args.type) {
				case "onclick":
					if(!student && get['user'] == null && get['asst'] == null) {
						for(s in response['StudentList']) {
							if (args.index == s) {
								window.location = document.URL + "&user=" + response['StudentList'][s]['StudentId'];
							}
						}
					} else if (!student && get['asst'] == null ) {
						for(a in response['StudentList'][s]['AssignmentList']) {
							if (args.index == a) {
								window.location = document.URL + "&asst=" + a;
							}
						}
					} else if (student && get['asst'] == null){
						for(a in response['StudentList'][0]['AssignmentList']) {
							if (args.index == a) {
								window.location = document.URL + "&asst=" + a;
							}
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

