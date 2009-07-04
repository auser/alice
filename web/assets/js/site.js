$(function(){
 
 var aliceUrl = "http://localhost:9999";
 
	// Status
	$.ajax({
	  url: aliceUrl + "/control/status",
	  cache: false,
	  dataType: "json",
	  success: function(data){
			var status = data.status[0];
	    $("#status").append("<h3>Applications</h3><ul>")		
			$(status.applications).each(function(i,app){
				$("#status").append("<li>"+app+"</li>");
			});
			$("#status").append("</ul>");
			
			$("#status").append("<h3>Nodes</h3><ul>")		
			$(status.nodes).each(function(i,app){
				$("#status").append("<li>"+app+"</li>");
			});
			$("#status").append("</ul>");
			
			$("#status").append("<h3>Running nodes</h3><ul>")		
			$(status.running_nodes).each(function(i,app){
				$("#status").append("<li>"+app+"</li>");
			});
			$("#status").append("</ul>");
	  },
		error: function(e, xhr){
			$("#status").append("<b>Error accessing status</b>")
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
			$("#vhosts").append("<b>Error accessing vhosts</b>")
		}
	});
	
	// Connection status
	$.ajax({
	  url: aliceUrl + "/conn/address/port/peer_address/peer_port/state/channels/user/vhost/timeout/frame_max/recv_oct/recv_cnt/send_oct/send_cnt/send_pend",
	  cache: false,
	  dataType: "json",
	  success: function(data){
			$("#conn_status").append("<ul>")
			var conn = $(data.conn);			
			$.each(conn, function(i, obj){
				$("#conn_status").append("<h4>"+i+"</h4>");
				traverse("conn", obj, function(key, val) {
					$("#conn_status").append("<li>"+key+" = "+val+"</li>");
				});
			});
			$("#conn_status").append("</ul>")
	  },
		error: function(e, xhr){
			$("#conn_status").append("<b>Error accessing connection</b>")
		}
	});
 
});