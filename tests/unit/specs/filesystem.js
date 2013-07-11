(function() {

var testFileSystem = null;
var testDir = null;
var subDir = null;
var testFile = null;

describe("FileSystem", function() {
    describe("Ext.space.FileSystem", function() {
        it("Ext.space.FileSystem should exist", function(done) {
                expect(Ext.space.FileSystem).to.exist;
                done();
        });

        it("should be able to request a file system", function(done) {
            Ext.space.FileSystem.requestFileSystem({
                success: function(filesystem) {
                    console.log("success callback", arguments);

                    expect(filesystem).to.exist;
                    expect(filesystem.getRoot).to.exist;

                    testFileSystem = filesystem;

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to get a root of a file system", function(done) {
            var root = testFileSystem.getRoot();

            expect(root).to.exist;
            expect(root.isFile()).to.be.false;
            expect(root.isDirectory()).to.be.true;
            expect(root.getName()).to.equal("/");
            expect(root.getFullPath()).to.equal("/");
            expect(root.getFileSystem()).to.equal(testFileSystem);

            root.getDirectory({
                path: "testDir",
                success: function(dir) {
                    console.log("success callback", arguments);

                    expect(dir).to.exist;

                    dir.removeRecursively({
                        success: function() {
                            console.log("success callback", arguments);

                            done();
                        },
                        failure: function(err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    expect(err).to.exist;
                    expect(err).to.equal("Could not get entry: not found");

                    done();
                }
            });
        });

        it("should be able to create a directory", function(done) {
            testFileSystem.getRoot().getDirectory({
                path: "testDir",
                options: {
                    create: true,
                    exclusive: true
                },
                success: function(dir) {
                    console.log("success callback", arguments);

                    expect(dir).to.exist;
                    expect(dir.isFile()).to.be.false;
                    expect(dir.isDirectory()).to.be.true;
                    expect(dir.getName()).to.equal("testDir");
                    expect(dir.getFullPath()).to.equal("/testDir");
                    expect(dir.getFileSystem()).to.equal(testFileSystem);

                    testDir = dir;

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to create a subdirectory in a directory", function(done) {
            testDir.getDirectory({
                path: "subDir",
                options: {
                    create: true,
                },
                success: function(dir) {
                    console.log("success callback", arguments);

                    expect(dir).to.exist;
                    expect(dir.isFile()).to.be.false;
                    expect(dir.isDirectory()).to.be.true;
                    expect(dir.getName()).to.equal("subDir");
                    expect(dir.getFullPath()).to.equal("/testDir/subDir");
                    expect(dir.getFileSystem()).to.equal(testFileSystem);

                    subDir = dir;

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to get a parent of a directory", function(done) {
            subDir.getParent({
                success: function(parent) {
                    console.log("success callback", arguments);

                    expect(parent).to.exist;
                    expect(parent.isFile()).to.be.false;
                    expect(parent.isDirectory()).to.be.true;
                    expect(parent.getName()).to.equal("testDir");
                    expect(parent.getFullPath()).to.equal("/testDir");
                    expect(parent.getFileSystem()).to.equal(testFileSystem);

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to create files in a directory", function(done) {
            subDir.getFile({
                path: "a.txt",
                options: {
                    create: true,
                },
                success: function(file) {
                    console.log("success callback", arguments);

                    expect(file).to.exist;

                    subDir.getFile({
                        path: "b.txt",
                        options: {
                            create: true,
                        },
                        success: function(file) {
                            console.log("success callback", arguments);

                            expect(file).to.exist;

                            subDir.getFile({
                                path: "c.txt",
                                options: {
                                    create: true,
                                },
                                success: function(file) {
                                    console.log("success callback", arguments);

                                    expect(file).to.exist;

                                    done();
                                },
                                failure: function(err) {
                                    console.log("error callback", arguments);

                                    throw err;
                                    done();
                                }
                            });
                        },
                        failure: function(err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to read files and subdirectories in a directory", function(done) {
            testDir.readEntries({
                success: function(subdirectories) {
                    console.log("success callback", arguments);

                    expect(subdirectories).to.exist;
                    expect(subdirectories.length).to.equal(1);

                    expect(subdirectories[0]).to.exist;
                    expect(subdirectories[0].isFile()).to.be.false;
                    expect(subdirectories[0].isDirectory()).to.be.true;
                    expect(subdirectories[0].getName()).to.equal("subDir");
                    expect(subdirectories[0].getFullPath()).to.equal("/testDir/subDir");
                    expect(subdirectories[0].getFileSystem()).to.equal(testFileSystem);

                    subdirectories[0].readEntries({
                        success: function(files) {
                            console.log("success callback", arguments);

                            expect(files).to.exist;
                            expect(files.length).to.equal(3);

                            files.sort(function(file1, file2) {
                                if (file1.getName() < file2.getName()) {
                                    return -1;
                                } else if (file1.getName() > file2.getName()) {
                                    return 1
                                }
                                return 0;
                            });

                            expect(files[0]).to.exist;
                            expect(files[0].isFile()).to.be.true;
                            expect(files[0].getOffset()).to.equal(0);
                            expect(files[0].isDirectory()).to.be.false;
                            expect(files[0].getName()).to.equal("a.txt");
                            expect(files[0].getFullPath()).to.equal("/testDir/subDir/a.txt");
                            expect(files[0].getFileSystem()).to.equal(testFileSystem);

                            expect(files[1]).to.exist;
                            expect(files[1].isFile()).to.be.true;
                            expect(files[1].getOffset()).to.equal(0);
                            expect(files[1].isDirectory()).to.be.false;
                            expect(files[1].getName()).to.equal("b.txt");
                            expect(files[1].getFullPath()).to.equal("/testDir/subDir/b.txt");
                            expect(files[1].getFileSystem()).to.equal(testFileSystem);

                            expect(files[2]).to.exist;
                            expect(files[2].isFile()).to.be.true;
                            expect(files[2].getOffset()).to.equal(0);
                            expect(files[2].isDirectory()).to.be.false;
                            expect(files[2].getName()).to.equal("c.txt");
                            expect(files[2].getFullPath()).to.equal("/testDir/subDir/c.txt");
                            expect(files[2].getFileSystem()).to.equal(testFileSystem);

                            done();
                        },
                        failure: function(err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to move a file", function(done) {
            var checkMove = function() {
                testDir.getFile({
                    path: "a.txt",
                    success: function(file) {
                        console.log("success callback", arguments);

                        expect(file).to.exist;

                        testFile = file;

                        subDir.getFile({
                            path: "a.txt",
                            success: function(file) {
                                console.log("success callback", arguments);

                                throw "should fail with error on nonexistent file";
                                done();
                            },
                            failure: function(err) {
                                console.log("error callback", arguments);

                                expect(err).to.exist;
                                expect(err).to.equal("Could not get entry: not found");

                                done();
                            }
                        });
                    },
                    failure: function(err) {
                        console.log("error callback", arguments);

                        throw err;
                        done();
                    }
                });
            }

            subDir.getFile({
                path: "a.txt",
                success: function(file) {
                    console.log("success callback", arguments);

                    expect(file).to.exist;

                    file.moveTo({
                        parent: testDir,
                        success: function(file) {
                            console.log("success callback", arguments);

                            expect(file).to.exist;

                            checkMove();
                        },
                        failure: function(err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to copy a directory", function(done) {
            var checkCopy = function() {
                testDir.getDirectory({
                    path: "subDir_copy",
                    success: function(dir) {
                        console.log("success callback", arguments);

                        expect(dir).to.exist;

                        dir.readEntries({
                            success: function(files) {
                                console.log("success callback", arguments);

                                expect(files).to.exist;
                                expect(files.length).to.equal(2);

                                files.sort(function(file1, file2) {
                                    if (file1.getName() < file2.getName()) {
                                        return -1;
                                    } else if (file1.getName() > file2.getName()) {
                                        return 1
                                    }
                                    return 0;
                                });

                                expect(files[0]).to.exist;
                                expect(files[0].isFile()).to.be.true;
                                expect(files[0].getOffset()).to.equal(0);
                                expect(files[0].isDirectory()).to.be.false;
                                expect(files[0].getName()).to.equal("b.txt");
                                expect(files[0].getFullPath()).to.equal("/testDir/subDir_copy/b.txt");
                                expect(files[0].getFileSystem()).to.equal(testFileSystem);

                                expect(files[1]).to.exist;
                                expect(files[1].isFile()).to.be.true;
                                expect(files[1].getOffset()).to.equal(0);
                                expect(files[1].isDirectory()).to.be.false;
                                expect(files[1].getName()).to.equal("c.txt");
                                expect(files[1].getFullPath()).to.equal("/testDir/subDir_copy/c.txt");
                                expect(files[1].getFileSystem()).to.equal(testFileSystem);

                                testDir.getDirectory({
                                    path: "subDir",
                                    success: function(dir) {
                                        console.log("success callback", arguments);

                                        expect(dir).to.exist;

                                        done();
                                    },
                                    failure: function(err) {
                                        console.log("error callback", arguments);

                                        throw err;
                                        done();
                                    }
                                });
                            },
                            failure: function(err) {
                                console.log("error callback", arguments);

                                throw err;
                                done();
                            }
                        });
                    },
                    failure: function(err) {
                        console.log("error callback", arguments);

                        throw err;
                        done();
                    }
                });
            }

            subDir.copyTo({
                parent: testDir,
                newName: "subDir_copy",
                success: function(dir) {
                    console.log("success callback", arguments);

                    expect(dir).to.exist;

                    checkCopy();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to remove a file", function(done) {
            testFileSystem.getRoot().getFile({
                path: "testDir/subDir/b.txt",
                success: function(file) {
                    console.log("success callback", arguments);

                    expect(file).to.exist;

                    file.remove({
                        success: function() {
                            console.log("success callback", arguments);

                            done();
                        },
                        failure: function(err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to recursively remove a directory", function(done) {
            testFileSystem.getRoot().getDirectory({
                path: "testDir/subDir_copy",
                success: function(dir) {
                    console.log("success callback", arguments);

                    expect(dir).to.exist;

                    dir.removeRecursively({
                        success: function() {
                            console.log("success callback", arguments);

                            done();
                        },
                        failure: function(err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to write to a file", function(done) {
            testFile.write({
                data: "Hello World!",
                success: function() {
                    console.log("success callback", arguments);

                    expect(testFile.getOffset()).to.equal(12);

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to set a byte offset into a file ", function(done) {
            testFile.seek({
                offset: 6,
                success: function() {
                    console.log("success callback", arguments);

                    expect(testFile.getOffset()).to.equal(6);

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to read from a file", function(done) {
            testFile.read({
                length: 5,
                success: function(data) {
                    console.log("success callback", arguments);

                    expect(data).to.exist;
                    expect(data).to.equal("World");

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to truncate a file", function(done) {
            var checkTruncate = function() {
                testFile.seek({
                    offset: 0,
                    success: function() {
                        console.log("success callback", arguments);

                        expect(testFile.getOffset()).to.equal(0);

                        testFile.read({
                            success: function(data) {
                                console.log("success callback", arguments);

                                expect(data).to.exist;
                                expect(data).to.equal("Hello");

                                done();
                            },
                            failure: function(err) {
                                console.log("error callback", arguments);

                                throw err;
                                done();
                            }
                        });
                    },
                    failure: function(err) {
                        console.log("error callback", arguments);

                        throw err;
                        done();
                    }
                });
            }

            testFile.truncate({
                size: 5,
                success: function() {
                    console.log("success callback", arguments);

                    expect(testFile.getOffset()).to.equal(5);

                    checkTruncate();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });
    });
});

})();
