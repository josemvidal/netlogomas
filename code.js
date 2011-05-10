function popup (name,w,h) {
 var w = w + 30;
 var h = h + 100;
 window.open(name + '/index.html', name, 'width=' + w + ',height=' + h);
 return false;
};

var showingModel = true;

$(document).ready(function(){
    $('.modelLink').hover(
	function(e){ console.log($(this)[0].id); $('#' + $(this)[0].id + 'Description').show(); }, //in
	function(e){ $('#' + $(this)[0].id + 'Description').hide();} //out
    );
    $('#aboutLink').click(function(e){
	if (showingModel){
	    $('#featuredModel').hide();
	    $('#aboutText').show();
	    showingModel = false;
	} else {
	    $('#aboutText').hide();
	    $('#featuredModel').show();
	    showingModel = true;
	}
       return false;
    });
});
