(function() {

var bridge = Ext.space.Communicator;

var CMD_GET = "Files#getFile";
var CMD_CREATE = "Files#createFile";
var CMD_REMOVE = "Files#removeFile";
var CMD_RENAME = "Files#renameFile";
var CMD_GET_CONTENTS = "Files#getFileContents";
var CMD_SET_CONTENTS = "Files#setFileContents";
var CMD_VIEW = "Files#viewFile";
var CMD_QUERY = "Files#queryFiles";

var TEST_TIMESTAMP = (new Date).getTime();
var TEST_FILE_NAME_BASE = "__unittest_" + TEST_TIMESTAMP;
var TEST_PATH = "__unittest_dir" + TEST_TIMESTAMP;
var TEST_CONTENT = "This is a test of the secure file API, and this is test content.";
var TEST_CHANGED_CONTENT = "This is a test of the secure file API, and this is the test content after it has been changed.";
var TEST_JS_CONTENT = "(function(){ console.log('w00t'); })();";

// will be a valid key to a file that gets removed, so we can make sure APIs fail
// properly when files are missing
var NONEXISTENT_FILE;

// will be a valid key to a file we create, for reuse in later tests
var TEST_KEY;


var testCollection;


var erCb = function(message) {
    return function(err){
        console.error(message, err);
        throw new Error(err);
    };
};

// generate a filename; pass it an extension (without the dot); defaults to "txt"
var makeFileName = (function() {
    var id = 0;
    return function(extension) {
        return TEST_FILE_NAME_BASE + "_" + (++id) + "." + (extension || "txt");
    };
})();

// do-nothing utility function
function NOOP() {}

// helper to calculate the current epoch timestamp in seconds
function getEpochNow() {
    return Math.floor((new Date).getTime() / 1000);
}


/*
The file objects passed back by the bridge look like:

    var file = {
        key: "",        // file's unique identifier
        name: "",       // filename
        created: long(epoch seconds),  // creation date
        modified: long(epoch seconds), // modified date
        type: "",       // MIME type
        appName: "",    // owning application
        appId: "",      // owning application ID
        path: "",       // virtual path to file
        size: long,     // size in bytes
    }
*/
describe("File Storage", function() {
    describe("Bridge API", function() {

        var keys = [],  name;

        //
        // createFile
        //
        it("createFile[name] calls onSuccess on completion", function(done) {
            name = makeFileName();
            bridge.send({
                command: CMD_CREATE,
                name: name,
                callbacks: {
                    onSuccess: function(file) {
                        expect(file).to.exist;
                        expect(file.key).to.exist;
                        expect(file.name).to.equal(name);

                        keys.push(file.key);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("createFile[name,path,type,fileData] calls onSuccess on completion", function(done) {
            name = makeFileName("txt");
            bridge.send({
                command: CMD_CREATE,
                name: name,
                path: TEST_PATH,
                type: "text/plain",
                fileData: TEST_CONTENT,
                callbacks: {
                    onSuccess: function(file) {
                        expect(file).to.exist;
                        expect(file.key).to.exist;
                        expect(file.name).to.equal(name);
                        expect(file.size).to.be.above(0);

                        keys.push(file.key);
                        TEST_KEY = file.key; // store this for later
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("createFile calls onError when filename is omitted", function(done) {
            bridge.send({
                command: CMD_CREATE,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        it("createFile[path,type,fileData] calls onError when filename is omitted", function(done) {
            bridge.send({
                command: CMD_CREATE,
                path: TEST_PATH,
                type: "text/plain",
                fileData: "This is a test of Files#createFile. If this file exists, it's an error.",
                callbacks: {
                    onSuccess: function(file) {
                        // if we get here, that's an error, but we still want to
                        // clean up the file later
                        if (file && file.key) {
                            keys.push(file.key);
                        }

                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });


        //
        // getFile
        //
        it("getFile calls onSuccess on completion", function(done) {
            var key = TEST_KEY;
            bridge.send({
                command: CMD_GET,
                key: key,
                callbacks: {
                    onSuccess: function(file) {
                        expect(file).to.exist;
                        expect(file.key).to.equal(key);
                        expect(file.name).to.have.length.above(0);

                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("getFile calls onError when key is omitted", function(done) {
            bridge.send({
                command: CMD_GET,
                callbacks: {
                    onSuccess: function(file) {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });


        //
        // removeFile
        //
        it("removeFile calls onSuccess on completion", function(done) {
            NONEXISTENT_FILE = keys[0]; // store this for later tests
            bridge.send({
                command: CMD_REMOVE,
                key: keys[0],
                callbacks: {
                    onSuccess: function() {
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("removeFile calls onError when key is omitted", function(done) {
            bridge.send({
                command: CMD_REMOVE,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        it("removeFile calls onError when the file doesn't exist", function(done) {
            bridge.send({
                command: CMD_REMOVE,
                key: NONEXISTENT_FILE,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });


        //
        // renameFile
        //
        it("renameFile calls onSuccess on successful completion", function(done) {
            name = makeFileName();
            bridge.send({
                command: CMD_RENAME,
                key: TEST_KEY,
                newName: name,
                callbacks: {
                    onSuccess: function() {
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("renameFile calls onError when renaming to an invalid name", function(done) {
            bridge.send({
                command: CMD_RENAME,
                key: TEST_KEY,
                newName: ".",
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        it("renameFile calls onError when key is omitted", function(done) {
            bridge.send({
                command: CMD_RENAME,
                newName: makeFileName(),
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        it("renameFile calls onError when newName is omitted", function(done) {
            bridge.send({
                command: CMD_RENAME,
                key: TEST_KEY,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        expect(error).to.exist;
                        done();
                    }
                }
            });
        });


        //
        // getFileContents
        //
        it("getFileContents calls onSuccess on completion", function(done) {
            bridge.send({
                command: CMD_GET_CONTENTS,
                key: TEST_KEY,
                callbacks: {
                    onSuccess: function(contents) {
                        expect(contents).to.exist;
                        expect(contents).to.have.length.above(0);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("getFileContents calls onError when key is omitted", function(done) {
            bridge.send({
                command: CMD_GET_CONTENTS,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        it("getFileContents calls onError when the file doesn't exist", function(done) {
            bridge.send({
                command: CMD_GET_CONTENTS,
                key: NONEXISTENT_FILE,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });


        //
        // setFileContents
        //
        it("setFileContents calls onSuccess on completion", function(done) {
            bridge.send({
                command: CMD_SET_CONTENTS,
                key: TEST_KEY,
                fileData: TEST_CHANGED_CONTENT,
                callbacks: {
                    onSuccess: function() {
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("setFileContents calls onError when key is omitted", function(done) {
            bridge.send({
                command: CMD_SET_CONTENTS,
                fileData: TEST_CHANGED_CONTENT,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        it("setFileContents calls onError when fileData is omitted", function(done) {
            bridge.send({
                command: CMD_SET_CONTENTS,
                key: TEST_KEY,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        it("setFileContents calls onError when the file doesn't exist", function(done) {
            bridge.send({
                command: CMD_SET_CONTENTS,
                key: NONEXISTENT_FILE,
                fileData: TEST_CHANGED_CONTENT,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });


        it("clean up prior to query tests", function(done) {
            var command = {
                command: CMD_REMOVE,
                callbacks: {
                    // we don't really care if it works or not; is that a healthy disposition?
                    onSuccess: NOOP,
                    onError: NOOP
                }
            };

            keys.forEach(function(key) {
                if (key) {
                    command.key = key;
                    bridge.send(command);
                }
            });

            // don't wait until the bridge responds with success; just move on
            keys = [];
            done();
        });


        //
        // viewFile
        //
        it("viewFile calls onSuccess on completion", function(done) {
            var key = TEST_KEY;
            bridge.send({
                command: CMD_VIEW,
                key: key,
                callbacks: {
                    onSuccess: function() {
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("viewFile calls onError when key is omitted", function(done) {
            bridge.send({
                command: CMD_VIEW,
                callbacks: {
                    onSuccess: function(file) {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        it("viewFile calls onError when the file doesn't exist", function(done) {
            bridge.send({
                command: CMD_VIEW,
                key: NONEXISTENT_FILE,
                callbacks: {
                    onSuccess: function() {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });


        //
        // queryFiles
        //

        var times = {
            start: 0,
            checkpoint: 0
        };

        // to make sure we can handle identically named files (they'll have different paths)
        var testDuplicateName = makeFileName("txt");

        var testfiles = [{
            name: testDuplicateName,
            type: "text/plain",
            content: TEST_CONTENT
        }, {
            name: testDuplicateName,
            type: "text/plain",
            content: TEST_CONTENT,
            path: TEST_PATH
        }, {
            name: makeFileName("js"),
            type: "application/javascript",
            content: TEST_JS_CONTENT
        }, {
            name: makeFileName("js"),
            type: "application/javascript",
            content: TEST_JS_CONTENT,
            path: TEST_PATH
        }];


        it("set up for the queryFiles tests", function(done) {
            var INTERVAL = 2000;
            this.timeout(testfiles.length * INTERVAL + 5000);

            // Basically what we do in this test is recursively loop through the
            // `testfiles` array for file data, and create files using the createFile
            // bridge call on each. We wait a couple of seconds before proceeding
            // after each one, to make it easier to test date-based queries. Finally,
            // we go back and modify the first file so we have a modification date
            // to query.

            times.start = getEpochNow();
            createTestFile(0);

            // create the idx'th file in `testfiles`
            function createTestFile(idx) {
                if (idx === testfiles.length) {
                    return modifyTestFiles();
                }

                var command = {
                    command: CMD_CREATE,
                    name: testfiles[idx].name,
                    type: testfiles[idx].type,
                    callbacks: {
                        onSuccess: function (file) {
                            // cache the new file's key and times, wait a bit, then move on
                            // (we're mostly ignoring the actual returned file data)
                            testfiles[idx].key = file.key;
                            testfiles[idx].modified = testfiles[idx].created = getEpochNow();
                            setTimeout(function() {
                                if (idx == 1) {
                                    // after the second file, store a checkpoint time we can use as a pivot
                                    times.checkpoint = getEpochNow() + 1;
                                }
                                createTestFile(idx+1);
                            }, INTERVAL);
                        },
                        onError: function(error) {
                            abort(); // something failed; clean up what we created so far
                        }
                    }
                };

                if (testfiles[idx].path) {
                    command.path = testfiles[idx].path;
                }

                bridge.send(command);
            }

            // modify one of the files we created, so we can do modification date queries
            function modifyTestFiles() {
                bridge.send({
                    command: CMD_SET_CONTENTS,
                    key: testfiles[0].key,
                    fileData: TEST_CHANGED_CONTENT,
                    callbacks: {
                        onSuccess: function() {
                            // finally all finished
                            testfiles[0].modified = getEpochNow();
                            done();
                        },
                        onError: function() {
                            abort();
                        }
                    }
                });
            }

            function abort() {
                testfiles.forEach(function(file) {
                    if (file.key) {
                        bridge.send({
                            command: CMD_REMOVE,
                            key: file.key,
                            callbacks: {
                                onSuccess: NOOP,
                                onError: NOOP
                            }
                        });
                    }
                });

                var filename;
                function hasKey(file) {
                    filename = file.name;
                    return !("key" in file);
                }
                if (testfiles.some(hasKey)) {
                    // failing midstream means a file creation failed
                    done("Error creating test file: " + filename);
                } else {
                    // last thing that could have failed was the file modification
                    done("Error modifying test file: " + testfiles[0].name);
                }
            }
        });

        //
        // query failures
        //
        it("query fails when query omitted", function(done) {
            bridge.send({
                command: CMD_QUERY,
                callbacks: {
                    onSuccess: function(files) {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        it("query fails when query is empty", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {},
                callbacks: {
                    onSuccess: function(files) {
                        done("Called onSuccess rather than onError");
                    },
                    onError: function(error) {
                        done();
                    }
                }
            });
        });

        //
        // query[name]
        //
        it("name-based query (fully qualified path) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: testfiles[1].name,
                    path: testfiles[1].path
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(1);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("name-based query (multiple exact matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: testDuplicateName
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        // count the files using the testDuplicateName
                        expect(files).to.have.length(testfiles.reduce(function(prev, cur) {
                            return prev + (cur.name === testDuplicateName ? 1 : 0);
                        }));
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("name-based query (multiple substring matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE // should match all files we've created
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(testfiles.length);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("name-based query (zero matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE + "___NEVER_WILL_MATCH___"
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(0);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        //
        // query[type]
        //
        it("type-based query (multiple matches) succeeds", function(done) {
            var type = "text/plain";
            bridge.send({
                command: CMD_QUERY,
                query: {
                    type: type
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        // count the files whose types match our query's type
                        expect(files).to.have.length(testfiles.reduce(function(prev, cur) {
                            return prev + (cur.type === type ? 1 : 0);
                        }, 0));
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("type-based query (zero matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    type: "___NEVER_WILL_MATCH___"
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(0);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        //
        // query[path]
        //
        it("path-based query (multiple matches) succeeds", function(done) {
            var path = TEST_PATH;
            bridge.send({
                command: CMD_QUERY,
                query: {
                    path: path
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        // count the files whose paths match our query's path
                        expect(files).to.have.length(testfiles.reduce(function(prev, cur) {
                            return prev + (cur.path === path ? 1 : 0);
                        }, 0));
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("path-based query (zero matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    path: "___NEVER_WILL_MATCH___"
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(0);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        //
        // query[createdBefore]
        //
        it("createdBefore-based query (multiple matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    createdBefore: getEpochNow() + 1
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(testfiles.length);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("createdBefore-based query (zero matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    createdBefore: 0
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(0);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        //
        // query[createdAfter]
        //
        it("createdAfter-based query (multiple matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    createdAfter: 0
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(testfiles.length);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("createdAfter-based query (zero matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    createdAfter: getEpochNow() + 1
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(0);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        //
        // query[modifiedBefore]
        //
        it("modifiedBefore-based query (non-zero matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    modifiedBefore: times.checkpoint
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        // count the files that were modified or created before the checkpoint
                        expect(files).to.have.length(testfiles.reduce(function(prev, cur) {
                            return prev + (cur.modified < times.checkpoint ? 1 : 0);
                        }, 0));
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("modifiedBefore-based query (all matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    modifiedBefore: getEpochNow() + 1
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(testfiles.length);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("modifiedBefore-based query (zero matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    modifiedBefore: 0
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(0);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        //
        // query[modifiedAfter]
        //
        it("modifiedAfter-based query (non-zero matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    modifiedAfter: times.checkpoint
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        // count the files with modification times after the checkpoint
                        expect(files).to.have.length(testfiles.reduce(function(prev, cur) {
                            return prev + (cur.modified > times.checkpoint ? 1 : 0);
                        }, 0));
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("modifiedAfter-based query (all matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    modifiedAfter: 0
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(testfiles.length);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("modifiedAfter-based query (zero matches) succeeds", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE,
                    modifiedAfter: getEpochNow() + 1
                },
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(0);
                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });


        //
        // query/sorting
        //

        // create a function that sorts by the field given, then by name, then by key,
        // all ascending (so if you want descending order, just reverse the result)
        function simpleTestSort(field) {
            return function(a, b) {
                if (a[field] < b[field]) {
                    return -1;
                } else if (a.name < b.name) {
                    return -1;
                } else if (a.key < b.key) {
                    return -1;
                } else {
                    return 1;
                }
            };
        }

        it("query properly sorts by creation timestamp (asc)", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE // should match all files we've created
                },
                sortField: "created",
                sortDirection: "asc",
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(testfiles.length);

                        // sort our testfiles data and make sure the ordering matches
                        testfiles.sort(simpleTestSort("created"));
                        for (var i=0; i<files.length; i++) {
                            expect(files[i].key).to.equal(testfiles[i].key);
                        }

                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });

        it("query properly sorts by creation timestamp (desc)", function(done) {
            bridge.send({
                command: CMD_QUERY,
                query: {
                    name: TEST_FILE_NAME_BASE // should match all files we've created
                },
                sortField: "created",
                sortDirection: "desc",
                callbacks: {
                    onSuccess: function(files) {
                        expect(files).to.exist;
                        expect(files).to.be.instanceof(Array);
                        expect(files).to.have.length(testfiles.length);

                        // sort our testfiles data and make sure the ordering matches
                        testfiles.sort(simpleTestSort("created")).reverse();
                        for (var i=0; i<files.length; i++) {
                            expect(files[i].key).to.equal(testfiles[i].key);
                        }

                        done();
                    },
                    onError: function(error) {
                        done("Error: " + error);
                    }
                }
            });
        });


        it("clean up after query tests", function(done) {
            testfiles.forEach(function(file) {
                if (file.key) {
                    bridge.send({
                        command: CMD_REMOVE,
                        key: file.key,
                        callbacks: {
                            onSuccess: NOOP,
                            onError: NOOP
                        }
                    });
                }
            });

            // don't wait until the bridge responds with success; just move on
            done();
        });
    });


    describe("Ext.space.SecureFiles Client API", function() {

        it("Ext.space.SecureFiles should exist", function(done) {
            expect(Ext.space.SecureFiles).to.exist;
            expect(Ext.space.files.Collection).to.exist;

            done();
        });

        it("Should be able to create collections", function(done) {
            testCollection = Ext.space.SecureFiles.get('myCollection');

	        expect(testCollection).to.exist;

            done();

        });

        it("Should store a string", function(done) {
            testCollection.set("first.txt", "Hello this is a test").then(function(){
                done();
            },erCb("Could not write file"));
        });


        it("Should get a string", function(done) {
            testCollection.get("first.txt").then(function(str){
                expect(str).to.equal("Hello this is a test");
                done();
            },erCb("Could not read file"));
        });

         it("Should not get a missing file", function(done) {
            testCollection.get("doesnotexist.txt").then(function(str){
                throw new Error("Got file contents for a file that does not exist.");
            }, function(err){
                done();
            });
        });

        it("has() should return false for non-existent file", function(done) {
            testCollection.has("doesnotexist.txt").then(function(result){
               expect(result).to.be.false;
               done();
            });
        });

        it("has() should return true for file that exists", function(done) {
            testCollection.has("first.txt").then(function(result){
               expect(result).to.be.true;
               done();
            });
        });

        it("Remove should remove the file", function(done) {
            testCollection.remove("first.txt").then(function(){
                testCollection.has("first.txt").then(function(result){
                   expect(result).to.be.false;
                   done();
                });
            }, erCb("Could not delete the file"));
        });

        it("count should be zero", function(done) {
            testCollection.count().then(function(count){
                expect(count).to.exist;
                expect(count).to.equal(0);
                done();
             });
        });

        it("keys should be empty", function(done) {
            testCollection.keys().then(function(keys){
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(0);
                done();
             });
        });

        it("foreach should be empty", function(done) {
            testCollection.forEach(function(key){
                console.log("callback!", key);
                throw new Error("Collection should be empty, callback should not have been called.");
            }).then(function(keys){
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(0);
                done();
            });

        });

        it("should be able to create multiple files", function(done) {
            testCollection.set("second.txt", "a").then(function(){
               testCollection.set("third.txt", "b").then(function(){
                    testCollection.set("fourth.txt", "c").then(function(){
                       done();
                    });
                });
            });
        });

        it("count should be 3", function(done) {
            testCollection.count().then(function(count){
                expect(count).to.exist;
                expect(count).to.equal(3);
                done();
             });
        });

        it("keys should return 3 files", function(done) {
            testCollection.keys().then(function(keys){
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(3);
                done();
             });
        });

        it("foreach should callback for each file", function(done) {
            var callbackCount = 0;
            testCollection.forEach(function(key){
                expect(key.getName).to.exist;
                expect(key.getName()).to.exist;
                expect(key.getContent).to.exist;
                callbackCount++;
            }).then(function(keys){
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(3);
                done();
            });

        });

        it("should be able to delete all the files in a collection", function(done) {
            testCollection.clear().then(function(){
                done();
            });

        });

        it("after clear, count should be zero", function(done) {
            testCollection.count().then(function(count){
                expect(count).to.exist;
                expect(count).to.equal(0);
                done();
             });
        });

        it("after clear, keys should be empty", function(done) {
            testCollection.keys().then(function(keys){
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(0);
                done();
             });
        });

        it("after clear, foreach should be empty", function(done) {
            testCollection.forEach(function(key){
                console.log("callback!", key);
                throw new Error("Collection should be empty, callback should not have been called.");
            }).then(function(keys){
                expect(keys).to.exist;
                expect(keys.length).to.exist;
                expect(keys.length).to.eql(0);
                done();
            });

        });

        it("after clear, should store a string", function(done) {
            testCollection.set("first.txt", "Hello this is a test").then(function(){
                done();
            },erCb("Could not write file"));
        });

        it("after clear, Should get a string", function(done) {
            testCollection.get("first.txt").then(function(str){
                expect(str).to.equal("Hello this is a test");
                done();
            },erCb("Could not read file"));
        });


    });
});

})();
