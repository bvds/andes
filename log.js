var express = require('express'),
    app = express();

app.configure(function() {
    app.use(express.bodyParser());
    app.use(function(req, res, next) {
        var oneof = false;
        if (req.headers.origin) {
            res.header('Access-Control-Allow-Origin', req.headers.origin);
            oneof = true;
        }
        if (req.headers['access-control-request-method']) {
            res.header('Access-Control-Allow-Methods', req.headers['access-control-request-method']);
            oneof = true;
        }
        if (req.headers['access-control-request-headers']) {
            res.header('Access-Control-Allow-Headers', req.headers['access-control-request-headers']);
            oneof = true;
        }
        if (oneof) {
            res.header('Access-Control-Max-Age', 60 * 60 * 24 * 365);
        }

        res.header('Cache-Control', 'no-cache, private, no-store, must-revalidate, max-stale=0, post-check=0, pre-check=0');

        // intercept OPTIONS method
        if (oneof && req.method == 'OPTIONS') {
            res.send(200);
        }
        else {
            next();
        }
    });
    app.use(app.router);
});

app.post('/log', function(request, response) {
    var body = request.body,
        logs;

    if (body) {
        logs = body.logs;
        logs.forEach(function(log) {
            console.log(JSON.stringify(log, null, 4));
        });

        response.send('Roger that');
    }

    response.send('Invalid');
});

app.listen(9876);
