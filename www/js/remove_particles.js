/* laisse les particules sur la page d'accueil, mais pour les autres pages, mets les particules en arriere afin de pouvoir manipuler les objets */

$( document ).ready(function() {
	$('ul.sidebar-menu li').on('click', function() {

		var interest = $(this).find('a').data('value');

		if (interest == 'home') {
			$(".particles-full").css("z-index", "1");
		}
		else {
			$(".particles-full").css("z-index", "-1");
		}
	});
	
});

