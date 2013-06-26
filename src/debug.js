// This whole block will be removed when built and minified
if (typeof DEBUG === 'undefined') {
    DEBUG = true;

    (function() {
        var log = console.log,
            warn = console.warn,
            error = console.error,
            pendingLogs = [],
            xhrTimer = null;

        function doLog(message, type) {
            var Communicator = Ext.space.Communicator;

            if (!xhrTimer) {
                xhrTimer = setTimeout(function() {
                    var xhr = new XMLHttpRequest();

                    xhr.open('POST', 'http://localhost:9876/log', true);
                    xhr.setRequestHeader('Content-Type', 'application/json');
                    xhr.send(JSON.stringify(pendingLogs));

                    pendingLogs.length = 0;
                    xhrTimer = null;
                }, 500);
            }

            pendingLogs.push({
                timeStamp: Date.now(),
                appId: Communicator.appId,
                device: Communicator.device,
                session: Communicator.session,
                title: document.title,
                url: window.location.href,
                message: message,
                type: type || 'log'
            });
        }

        console.log = function() {
            var message = Array.prototype.slice.call(arguments).join(' ');
            doLog(message, 'log');
            return log && log.apply(console, arguments);
        };

        console.warn = function() {
            var message = Array.prototype.slice.call(arguments).join(' ');
            doLog(message, 'warn');
            return warn && warn.apply(console, arguments);
        };

        console.error = function() {
            var message = Array.prototype.slice.call(arguments).join(' ');
            doLog(message, 'error');
            return error && error.apply(console, arguments);
        };
    })();

    $expectIdSeed = 0;

    $expect = function(name, within, object, method, verifyFn) {
        var fn = object[method],
            expectations, spy, timer, id;

        if (!fn.$isSpy) {
            object[method] = spy = function() {
                var now = Date.now(),
                    i, expectation, verify;

                for (i = expectations.length - 1; i >= 0; i--) {
                    expectation = expectations[i];
                    verify = expectation.verify;

                    if (!verify || verify.apply(object, arguments)) {
                        clearTimeout(expectation.timer);
                        expectations.splice(i, 1);
                        console.log('[EXPECT][' + id + '][END] ' + expectation.name + ' after ' +
                            (now - expectation.time) + 'ms');
                    }
                }

                return fn.apply(object, arguments);
            };
            spy.$isSpy = true;
            spy.$expectations = expectations = [];
        }
        else {
            spy = fn;
            expectations = spy.$expectations;
        }

        id = ++$expectIdSeed;

        timer = setTimeout(function() {
            var i, ln, expectation, id;

            for (i = 0, ln = expectations.length; i < ln; i++) {
                expectation = expectations[i];
                if (expectation.timer === timer) {
                    id = expectation.id;
                    expectations.splice(i, 1);
                    break;
                }
            }

            console.error('[EXPECT][' + id + '][FAILED]: ' + name + ' within ' + within + 'ms');
        }, within);

        expectations.push({
            id: id,
            name: name,
            time: Date.now(),
            verifyFn: verifyFn,
            timer: timer
        });

        console.log('[EXPECT][' + id + '][START] ' + name + ' within ' + within + 'ms');
    };

    window.onerror = function(message, line, file) {
        console.error('[window.onerror][' + line + '][' + file + '] ' + message);
    }
}
