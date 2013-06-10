function mycarousel_initCallback(carousel) {

	$('#next').bind('click', function() {
        carousel.next();
        return false;
    });

    $('#prev').bind('click', function() {
        carousel.prev();
        return false;
    });



};

$(function(){
	
	$(document).pngFix();
	
	$('#slider-content ul').jcarousel({
		auto: 3,
		wrap: "last",
		scroll: 1,
		visible: 1,
		initCallback: mycarousel_initCallback,
        buttonNextHTML: null,
        buttonPrevHTML: null
	});
	
});


