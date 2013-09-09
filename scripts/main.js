(function () {

    var Events = {
        contactMe: function() {
            var contactMe = document.querySelector('footer .contact-me');
            contactMe.addEventListener("click", function(e) {
                e.preventDefault();
                console.log('here');
                var twitter = document.querySelector('#twitter');
                twitter.classList.toggle('open');
            }, true);
        },
        clickOut: function() {
            document.addEventListener('click', function(e) {
                if(e.target.className != 'contact-me') {
                    var twitter = document.querySelector('#twitter');
                    twitter.classList.remove('open');
                }
            }, true);
        }
    };

    window.onload = function() {
        Events.contactMe();
        Events.clickOut();
    };
})();