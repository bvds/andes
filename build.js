var fs = require('fs'),
    path = require('path'),
    UglifyJS = require('uglify-js'),
    files = [
        'core',
        'Communicator',
        'Promise',
        'Observable',
        'Camera',
        'Connection',
        'Notification',
        'Orientation',
        'invoke/Connection',
        'Invoke',
        'Sqlite'
    ],
    content = [];


files.forEach(function(file) {
    content.push(fs.readFileSync(path.join('src', file + '.js'), 'utf8'));
});

fs.writeFileSync('space.js', content.join('\n'));

result = UglifyJS.minify('space.js');

fs.writeFileSync('space.min.js', result.code);


