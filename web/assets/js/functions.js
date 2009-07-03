function traverse(key, jsonObj, func) {
    if( typeof jsonObj == "object" ){
	    $.each(jsonObj, function(k,v) {
          traverse(k,v, func);
			})
		} else {
			func(key,jsonObj);
    }
};