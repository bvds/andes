dojo.require("dojox.charting.Chart");
dojo.require("dojox.charting.axis2d.Default")
dojo.require("dojox.charting.plot2d.Columns")
dojo.require("dojox.charting.plot2d.Bars")
dojo.require("dojox.charting.widget.Chart");
dojo.require("dojox.charting.themes.BlueDusk");
dojo.require("dojox.charting.action2d.Highlight");
dojo.require("dojox.charting.action2d.Tooltip");

dojo.require("dojox.rpc.Service"); // from
dojo.require("dojox.rpc.JsonRPC"); // api.js
dojo.require("dojox.json.schema"); //

///////////////////////////////////////////////////
json = {"version": 1, "section": "andestutor.org"};

dashboard_rpc = new dojox.rpc.Service("js/dashboard.smd");

var dfd;
dojo.addOnLoad(
	function() {
		dfd = dashboard_rpc["dashboard"]({"version":"1","section":"andesTutor"});
//		dfd = dashboard_rpc["dashboard-rpc-test"]();
		console.log("sent request")
		dfd.addCallbacks(
			function(candy) {
   				console.log(candy);
				response = candy[0];
				makeChart();
			},
			function(error) {
				console.log(error);
			}
		);
	}
);

//////////////////////////////////////////////////

var chart_labels = [], chart_data = [];

function parse()
{
	// for chart labels
	var index = 1;
	
	// Student
	if(student) {
		// Student Top Level
		if(get['asst'] == null) {
			for (a in response["student-list"][0]["assignment-list"]) {
				var total = 0, count = 0;
				
				for (kc in response["student-list"][0]["assignment-list"][a]["kc-list"]) {
					total += response["student-list"][0]["assignment-list"][a]["kc-list"][kc]["mastery-level"] * 100;
					count++;
				}
				
				chart_data.push(total/count);
				chart_labels.push({ value: index, text: response["student-list"][0]["assignment-list"][a]["assignment-id"] });
				
				index++;
			}
		} else {
		// Student Assignment Level
			var asst_index = parseInt(get['asst']);
			
			for (kc in response["student-list"][0]["assignment-list"][asst_index]["kc-list"]) {
				chart_data.push(response["student-list"][0]["assignment-list"][asst_index]["kc-list"][kc]["mastery-level"] * 100);
				chart_labels.push({ value: index, text: response["student-list"][0]["assignment-list"][asst_index]["kc-list"][kc]["name"] });
				index++;
			}	
		}
	// Teacher
	} else { 
		// Teacher Top Level
		if (get['user'] == null && get['asst'] == null) {
			for (s in response["student-list"]) {
				var total = 0, count = 0;
				
				for (a in response["student-list"][s]["assignment-list"]) {
					for (kc in response["student-list"][s]["assignment-list"][a]["kc-list"]) {
						total += response["student-list"][s]["assignment-list"][a]["kc-list"][kc]["mastery-level"] * 100;
						count++;
					}
				}
				
				chart_data.push(total/count);
				chart_labels.push({ value: index, text: response["student-list"][s]["student-id"] });
				
				index++;
			}
		} else if (get['asst'] == null) {
		// Teacher Student Level
			for(s in response["student-list"]) {
				if(response["student-list"][s]["student-id"] == get['user'])
					stud_id = s;
			}
		
			for (a in response["student-list"][stud_id]["assignment-list"]) {
				var total = 0, count = 0;
				
				for (kc in response["student-list"][stud_id]["assignment-list"][a]["kc-list"]) {
					total += response["student-list"][stud_id]["assignment-list"][a]["kc-list"][kc]["mastery-level"] * 100;
					count++;
				}
				
				chart_data.push(total/count);
				chart_labels.push({ value: index, text: response["student-list"][stud_id]["assignment-list"][a]["assignment-id"] });
				
				index++;
			}
		} else {
			for(s in response["student-list"]) {
				if(response["student-list"][s]["student-id"] == get['user'])
					stud_id = s;
			}
		
			var asst_index = parseInt(get['asst']);
			
			for (kc in response["student-list"][stud_id]["assignment-list"][asst_index]["kc-list"]) {
				chart_data.push(response["student-list"][stud_id]["assignment-list"][asst_index]["kc-list"][kc]["mastery-level"] * 100);
				chart_labels.push({ value: index, text: response["student-list"][0]["assignment-list"][asst_index]["kc-list"][kc]["name"] });
				index++;
			}			
		}
	}
}

function makeChart() 
{
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
	chart.connectToPlot("default", function(args) 
	{
		switch (args.type) 
		{
			case "onclick":
				if(!student && get['user'] == null && get['asst'] == null) {
					for(s in response["student-list"]) {
						if (args.index == s) {
							window.location = document.URL + "&user=" + response["student-list"][s]["student-id"];
						}
					}
				} else if (!student && get['asst'] == null ) {
					for(a in response["student-list"][s]["assignment-list"]) {
						if (args.index == a) {
							window.location = document.URL + "&asst=" + a;
						}
					}
				} else if (student && get['asst'] == null){
					for(a in response["student-list"][0]["assignment-list"]) {
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
};

