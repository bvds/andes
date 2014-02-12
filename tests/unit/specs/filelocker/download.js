
describe("Ext.space.filelocker.Download", function() {

    // this data doesn't make actual sense, but it works for testing purposes
    var TEST_DATA = {
        downloadId: "-",
        url: "-",
        mimeType: "-",
        bytesDownloaded: 1000,
        totalBytes: 2000,
        isComplete: false,
        dateStarted: new Date(),
        fileName: "-"
    };

    var UPDATE_DATA = {
        bytesDownloaded: 2000,
        isComplete: true,
        fileName: "foo"
    };

    it("module exists", function(done) {
        expect(Ext.space.filelocker.Download).to.exist;
        done();
    });

    it("properly instantiates as empty", function(done) {
        var d = new Ext.space.filelocker.Download();

        expect(d.downloadId).to.be.null;
        expect(d.url).to.be.null;
        expect(d.mimeType).to.be.null;
        expect(d.bytesDownloaded).to.equal(0);
        expect(d.totalBytes).to.equal(0);
        expect(d.isComplete).to.be.false;
        expect(d.dateStarted).to.be.null;
        expect(d.fileName).to.be.null;

        done();
    });

    it("populates via constructor", function(done) {
        var d = new Ext.space.filelocker.Download(TEST_DATA);

        expect(d.downloadId).to.equal(TEST_DATA.downloadId);
        expect(d.url).to.equal(TEST_DATA.url);
        expect(d.mimeType).to.equal(TEST_DATA.mimeType);
        expect(d.bytesDownloaded).to.equal(TEST_DATA.bytesDownloaded);
        expect(d.totalBytes).to.equal(TEST_DATA.totalBytes);
        expect(d.isComplete).to.equal(TEST_DATA.isComplete);
        expect(d.dateStarted).to.equal(TEST_DATA.dateStarted);
        expect(d.fileName).to.equal(TEST_DATA.fileName);

        done();
    });

    it("populates via _updateWith", function(done) {
        var d = new Ext.space.filelocker.Download();

        expect(d.downloadId).to.be.null;
        expect(d.url).to.be.null;
        expect(d.mimeType).to.be.null;
        expect(d.bytesDownloaded).to.equal(0);
        expect(d.totalBytes).to.equal(0);
        expect(d.isComplete).to.be.false;
        expect(d.dateStarted).to.be.null;
        expect(d.fileName).to.be.null;

        d._updateWith(UPDATE_DATA);

        expect(d.downloadId).to.be.null; // i.e., hasn't changed
        expect(d.bytesDownloaded).to.equal(UPDATE_DATA.bytesDownloaded);
        expect(d.isComplete).to.equal(UPDATE_DATA.isComplete);
        expect(d.fileName).to.equal(UPDATE_DATA.fileName);

        done();
    });

    // Note: d.getProgress() and d.cancel() are tested in the FileLocker module

});
