/*
   Persist form fields across pages using local storage.
 */

function formFields() {
    var forms = document.getElementsByTagName("form"); 
    // This is a list of form elements to be persisted 
    // across pages.
    var fields = ["passwd", "dbuser", "dbname", "startDate", "endDate", "problem"];
    for(var i=0; i<forms.length; i++) {
	var form = forms[i];
	fields.forEach(function(field) {
		var input = form.querySelector('input[name="'+field+'"]');
		if(localStorage && input) {
		    var submit = form.querySelector('input[type="submit"]');
		    function recordInputs() {
			localStorage.setItem(field, input.value);
			console.log("save "+field+" = ", input.value);
		    }
		    submit.addEventListener('click', recordInputs);
		    var value = localStorage.getItem(field);
		    if(typeof value !== "undefined" && value != null) {
			input.value = value;
			console.log("set "+field+" = " , value);
		    } else {
			console.log("no "+field+" found");
		    }
		}
	    });
    };
}

window.addEventListener('pageshow', formFields);
if (document.readyState != 'loading'){
    formFields();
}
