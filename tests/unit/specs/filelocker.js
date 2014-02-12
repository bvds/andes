(function() {

var bridge = Ext.space.Communicator;
var locker = Ext.space.FileLocker;

var TEST_URL = "http://www.sencha.com/img/v2/logo.png";

var CMD_DOWNLOAD = "Locker#download";
var CMD_GET_PROGRESS = "Locker#getProgress";
var CMD_CANCEL = "Locker#cancel";
var CMD_GET_DOWNLOADS = "Locker#getDownloads";
var CMD_WATCH_DOWNLOADS = "Locker#watchDownloads";

function NOOP() {}

// A "download" object looks like this:
//     {
//         downloadId: "",        // internal identifier for this download
//         url: "",               // source URL
//         mimeType: "",          // MIME type
//         bytesDownloaded: long, // progress so far
//         totalBytes: long,      // final size
//         isComplete: bool,      // simple flag, maybe not needed...?
//         dateStarted: long(epoch seconds), // when the download initiated
//         fileName: ""           // destination file path/name
//     }


describe("FileLocker", function() {

    it("module exists", function(done) {
        expect(Ext.space.FileLocker).to.exist;
        done();
    });


    /* ------------------------------------------------------------------------
     * Bridge API
     *
     */
    describe("FileLocker Bridge API", function() {

        var downloadId; // download ID for bridge API tests

        //
        // download
        //
        it("download should call onStart when initiating", function(done) {
            bridge.send({
                command: CMD_DOWNLOAD,
                url: TEST_URL,
                callbacks: {
                    onStart: function(download) {
                        console.log("onStart ============", download);
                        expect(download).to.exist;
                        expect(download.downloadId).to.exist;
                        expect(download.isComplete).to.exist;
                        expect(download.isComplete).to.equal(false);
                        done();
                    },
                    onSuccess: function() {
                        console.log("onSuccess ==========", arguments);
                        done();
                    },
                    onError: function(error) {
                        console.log("onError ============", error);
                        done();
                    }
                }
            });
        });

        it("download should call onSuccess when finished", function(done) {
            bridge.send({
                command: CMD_DOWNLOAD,
                url: TEST_URL,
                callbacks: {
                    onSuccess: function(download) {
                        expect(download).to.exist;
                        expect(download.downloadId).to.exist;
                        expect(download.isComplete).to.exist;
                        expect(download.bytesDownloaded).to.exist;
                        expect(download.totalBytes).to.exist;

                        expect(download.isComplete).to.equal(true);
                        expect(download.bytesDownload).to.equal(download.totalBytes);

                        done();
                    }
                }
            });
        });

        it("download should call onError when not provided a URL", function(done) {
            bridge.send({
                command: CMD_DOWNLOAD,
                url: "",
                callbacks: {
                    onError: function(error) {
                        expect(error).to.exist;
                        done();
                    }
                }
            });
        });


        //
        // getProgress
        //
        it("getProgress should provide a download in onSuccess", function(done) {
            bridge.send({
                command: CMD_DOWNLOAD,
                callbacks: {
                    onSuccess: function(startedDownload) {
                        downloadId = startedDownload.downloadId; // store this for future tests

                        bridge.send({
                            command: CMD_GET_PROGRESS,
                            downloadId: downloadId,
                            callbacks: {
                                onSuccess: function(download) {
                                    expect(download).to.exist;
                                    expect(download.downloadId).to.equal(downloadId);
                                    done();
                                }
                            }
                        });
                    }
                }
            });
        });

        it("getProgress should error out when downloadId is omitted", function(done) {
            bridge.send({
                command: CMD_GET_PROGRESS,
                callbacks: {
                    onError: function(error) {
                        expect(error).to.exist;
                        done();
                    }
                }
            });
        });


        //
        // cancel
        //
        it("cancel should fire the onSuccess callback", function(done) {
            // initiate a download and immediately cancel it in the onStart callback
            bridge.send({
                command: CMD_DOWNLOAD,
                url: TEST_URL,
                callbacks: {
                    onStart: function(download) {
                        // kicked off a download, now cancel it right away
                        bridge.send({
                            command: CMD_CANCEL,
                            downloadId: download.downloadId,
                            callbacks: {
                                onSuccess: function() {
                                    done();
                                }
                            }
                        });
                    }
                }
            });
        });

        it("cancel should error out when the download is already complete", function(done) {
            bridge.send({
                command: CMD_CANCEL,
                downloadId: downloadId,
                callbacks: {
                    onError: function(error) {
                        expect(error).to.exist;
                        done();
                    }
                }
            });
        });

        it("cancel should error out when downloadId is omitted", function(done) {
            bridge.send({
                command: CMD_CANCEL,
                callbacks: {
                    onError: function(error) {
                        expect(error).to.exist;
                        done();
                    }
                }
            });
        });


        //
        // getDownloads
        //
        it("getDownloads should provide an array of downloads", function(done) {
            bridge.send({
                command: CMD_GET_DOWNLOADS,
                callbacks: {
                    onSuccess: function(downloads) {
                        expect(downloads).to.be.instanceof(Array);
                        done();
                    }
                }
            });
        });


        //
        // watchDownloads
        //
        it("watchDownloads should provide an array of downloads currently in progress", function(done) {
            bridge.send({
                command: CMD_WATCH_DOWNLOADS,
                callbacks: {
                    onSuccess: function(downloads) {
                        expect(downloads).to.be.instanceof(Array);
                        done();
                    }
                }
            });
        });

    });


    /* ------------------------------------------------------------------------
     * Client API
     *
     */
    describe("FileLocker Client API", function() {

        var downloadId; // for use in the Client API tests

        describe("Promise style", function() {
            //
            // download
            //
            it("download returns an Ext.Promise", function(done) {
                var promise = locker.download({ url: TEST_URL }).then(NOOP);
                expect(promise).to.be.instanceof(Ext.Promise);
                done();
            });

            it("download completes properly", function(done) {
                locker.download({ url: TEST_URL }).then(function(download) {
                    expect(download).to.exist;
                    expect(download).to.be.instanceof(Ext.space.filelocker.Download);

                    expect(download.isComplete).to.equal(true);
                    expect(download.bytesDownloaded).to.equal(download.totalBytes);

                    done();
                });
            });

            it("download fires progress events", function(done) {
                locker.download({ url: TEST_URL }).then(null, null, function(download) {
                    expect(download).to.exist;
                    expect(download).to.be.instanceof(Ext.space.filelocker.Download);
                    done();
                });
            });

            it("download errors out without a URL", function(done) {
                locker.download({}).then(null, function(error) {
                    expect(error).to.exist;
                    done();
                });
            });


            //
            // getProgress
            //
            it("getProgress returns an Ext.Promise", function(done) {
                // should have a promise (it'll be rejected already, but that's beside the point)
                var promise = locker.getProgress({ downloadId: "" }).then(NOOP);
                expect(promise).to.be.instanceof(Ext.Promise);
                done();
            });

            it("getProgress (direct downloadId) succeeds", function(done) {
                locker.download({ url: TEST_URL }).then(function(startedDownload) {
                    downloadId = startedDownload.downloadId; // store this for future tests

                    locker.getProgress(downloadId).then(function(download) {
                        expect(download).to.exist;
                        expect(download).to.be.instanceof(Ext.space.filelocker.Download);
                        expect(download.downloadId).to.exist;
                        expect(download.downloadId).to.equal(downloadId);
                        done();
                    });
                });
            });

            it("getProgress (downloadId in an object) succeeds", function(done) {
                locker.getProgress({ downloadId: downloadId }).then(function(download) {
                    expect(download).to.exist;
                    expect(download).to.be.instanceof(Ext.space.filelocker.Download);
                    expect(download.downloadId).to.exist;
                    expect(download.downloadId).to.equal(downloadId);
                    done();
                });
            });

            it("download.getProgress succeeds", function(done) {
                locker.download({ url: TEST_URL }).then(null, null, function(startedDownload) {
                    downloadId = startedDownload.downloadId;

                    startedDownload.getProgress(downloadId).then(function(progressingDownload) {
                        expect(progressingDownload).to.exist;
                        expect(progressingDownload).to.be.instanceof(Ext.space.filelocker.progressingDownload);
                        expect(progressingDownload.downloadId).to.exist;
                        expect(progressingDownload.downloadId).to.equal(downloadId);
                        done();
                    });
                });
            });

            it("getProgress (omitted direct downloadId) fails", function(done) {
                locker.getProgress().then(null, function(error) {
                    expect(error).to.exist;
                    done();
                });
            });


            //
            // cancel
            //
            it("cancel returns an Ext.Promise", function(done) {
                // should have a promise (it'll be rejected already, but that's beside the point)
                expect(locker.cancel()).to.be.instanceof(Ext.Promise);
                done();
            });

            it("cancel (direct downloadId) should resolve properly", function(done) {
                // kick off a download and immediately cancel it as soon as it reports progress
                locker.download({ url: TEST_URL }).then(null, null, function(download) {
                    locker.cancel(download.downloadId).then(function() {
                        done();
                    });
                });
            });

            it("cancel (downloadId in an object) should resolve properly", function(done) {
                // kick off a download and immediately cancel it as soon as it reports progress
                locker.download({ url: TEST_URL }).then(null, null, function(download) {
                    locker.cancel({ downloadId: download.downloadId }).then(function() {
                        done();
                    });
                });
            });

            it("download.cancel should resolve properly", function(done) {
                // kick off a download and immediately cancel it as soon as it reports progress
                locker.download({ url: TEST_URL }).then(null, null, function(download) {
                    download.cancel().then(function(result) {
                        expect(result).to.be.true;
                        done();
                    });
                });
            });

            it("cancel should error out when the download is already complete", function(done) {
                // kick off a download and try to cancel it after it's done
                locker.download({ url: TEST_URL }).then(function(download) {
                    locker.cancel(download.downloadId).then(null, function() {
                        done();
                    });
                });
            });

            it("cancel should error out when downloadId is omitted", function(done) {
                locker.download({ url: TEST_URL }).then(null, null, function(download) {
                    locker.cancel().then(null, function() {
                        done();
                    });
                });
            });


            //
            // getDownloads
            //
            it("getDownloads returns an Ext.Promise", function(done) {
                expect(locker.getDownloads()).to.be.instanceof(Ext.Promise);
                done();
            });

            it("getDownloads should provide an array of downloads", function(done) {
                locker.getDownloads().then(function(downloads) {
                    expect(downloads).to.be.instanceof(Array);
                    for (var i=0; i<downloads.length; i++) {
                        expect(downloads[i]).to.be.instanceof(Ext.space.filelocker.Download);
                    }
                    done();
                });
            });
        });


        describe("Callback style", function() {
            //
            // download
            //
            it("download completes properly", function(done) {
                locker.download({
                    url: TEST_URL,
                    onComplete: function(download) {
                        expect(download).to.exist;
                        expect(download).to.be.instanceof(Ext.space.filelocker.Download);

                        expect(download.isComplete).to.equal(true);
                        expect(download.bytesDownloaded).to.equal(download.totalBytes);

                        done();
                    }
                });
            });

            it("download fires progress events", function(done) {
                locker.download({
                    url: TEST_URL,
                    onProgress: function(download) {
                        expect(download).to.exist;
                        expect(download).to.be.instanceof(Ext.space.filelocker.Download);
                        done();
                    }
                });
            });

            it("download errors out without a URL", function(done) {
                locker.download({
                    onError: function(error) {
                        expect(error).to.exist;
                        done();
                    }
                });
            });


            //
            // getProgress
            //
            it("getProgress (downloadId in an object) succeeds", function(done) {
                locker.getProgress({
                    downloadId: downloadId,
                    onComplete: function(download) {
                        expect(download).to.exist;
                        expect(download).to.be.instanceof(Ext.space.filelocker.Download);
                        expect(download.downloadId).to.exist;
                        expect(download.downloadId).to.equal(downloadId);
                        done();
                    }
                });
            });

            it("getProgress (omitted downloadId from parameter object) fails", function(done) {
                locker.getProgress({
                    onError: function(error) {
                        expect(error).to.exist;
                        done();
                    }
                });
            });


            //
            // cancel
            //
            it("cancel should resolve properly", function(done) {
                // kick off a download and immediately cancel it as soon as it reports progress
                locker.download({ url: TEST_URL }).then(null, null, function(download) {
                    locker.cancel({
                        downloadId: download.downloadId,
                        onComplete: function() {
                            done();
                        }
                    });
                });
            });

            it("cancel should error out when the download is already complete", function(done) {
                // kick off a download and try to cancel it after it's done
                locker.download({ url: TEST_URL }).then(function(download) {
                    locker.cancel({
                        downloadId: download.downloadId,
                        onError: function() {
                            done();
                        }
                    });
                });
            });

            it("cancel should error out when downloadId is omitted", function(done) {
                locker.download({ url: TEST_URL }).then(null, null, function(download) {
                    locker.cancel({
                        onError: function() {
                            done();
                        }
                    });
                });
            });


            //
            // getDownloads
            //
            it("getDownloads should provide an array of downloads", function(done) {
                locker.getDownloads({
                    onComplete: function(downloads) {
                        expect(downloads).to.be.instanceof(Array);
                        for (var i=0; i<downloads.length; i++) {
                            expect(downloads[i]).to.be.instanceof(Ext.space.filelocker.Download);
                        }
                        done();
                    }
                });
            });
        });

    });

});

})();
