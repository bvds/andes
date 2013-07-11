var expect = chai.expect;

chai.Assertion.addMethod('match', function(match) {
    var obj = this._obj,
        diff = null,
        key, value;

    for (key in match) {
        if (match.hasOwnProperty(key)) {
            value = match[key];

            if (value !== obj[key]) {
                diff = key;
                break;
            }
        }
    }

    this.assert(
        diff === null
        , "expected #{this} to match #{exp} but '" + diff + "' key is different (" + obj[diff] + " vs. " + match[diff] + ")"
        , "expected #{this} to not match #{act}"
        , match        // expected
        , obj
    );
});

function expectError(done, promise, callback) {
    promise.then(function() {
        done("Expected an error, but success is returned");
    }, function(error) {
        try {
            callback(error);
            done();
        }
        catch (e) {
            done(e);
        }
    });
}

function expectSuccess(done, promise, callback) {
    promise.then(function(result) {
        try {
            callback(result);
            done();
        }
        catch (e) {
            done(e);
        }
    }, function(error) {
        done("Expected success, but an error is returned " + error);
    });
}

function run(url) {
    Ext.onSpaceReady(function() {
        console.log("WAAAAAAAAA");
       // mocha.run();
    });
}
