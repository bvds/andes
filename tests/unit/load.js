(function() {
    mocha.setup({
        ui: 'bdd',
        timeout: 5000
    });

    var xhr = new XMLHttpRequest();

    xhr.open('GET', 'specs.txt?' + Date.now(), false);
    xhr.send(null);

    var specs = xhr.responseText.split("\n")
        .map(function(spec) {
            return spec.trim()
        })
        .filter(function(spec) {
            return !!spec && !/^\/\/|#/.test(spec)
        })
        .map(function(spec) {
            return "specs/" + spec
        });

    specs.forEach(function(spec) {
        document.write('<script type="text/javascript" src="'+(spec + '?' + Date.now())+'"></'+'script>');
    });
})();
