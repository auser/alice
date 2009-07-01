$(function(){
 
 var aliceUrl = "http://localhost:9999";
 
	// Status
	$.ajax({
	  url: aliceUrl + "/control/status",
	  cache: false,
	  dataType: "json",
	  success: function(data){
	    $("#status").append("<h3>Applications</h3><p>"+data.status[0].applications+"</p>");
	    $("#status").append("<h3>Nodes</h3><p>"+data.status[0].nodes+"</p>");
	    $("#status").append("<h3>Running Nodes</h3><p>"+data.status[0].running_nodes+"</p>");
	  },
		error: function(e, xhr){
			$("#users").append("<b>Error accessing status</b>")
		}
	});
 
	// Users
	$.ajax({
	  url: aliceUrl + "/users",
	  cache: false,
	  dataType: "json",
	  success: function(data){
			$("#users").append("<ul>")
			$(data.users).each(function(i,username){
				$("#users").append("<li>"+username+"</li>");
			});	    
			$("#users").append("</ul>")
	  },
	  error: function(e, xhr){
			$("#users").append("<b>Error accessing users</b>")
		}
	});
	
	// Vhosts
	$.ajax({
	  url: aliceUrl + "/vhosts",
	  cache: false,
	  dataType: "json",
	  success: function(data){
			$("#vhosts").append("<ul>")
			$(data.vhosts).each(function(i,username){
				$("#vhosts").append("<li>"+username+"</li>");
			});	    
			$("#vhosts").append("</ul>")
	  },
		error: function(e, xhr){
			$("#users").append("<b>Error accessing vhosts</b>")
		}
	});
 
});