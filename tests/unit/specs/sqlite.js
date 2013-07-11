(function() {

var testDatabase = null;

describe("Sqlite", function() {
    describe("Ext.space.Sqlite", function() {
        it("Ext.space.Sqlite should exist", function(done) {
            expect(Ext.space.Sqlite).to.exist;

            done();
        });

        it("should be able to open a database", function(done) {

            //replace with sencha class once workspace is functional.
            //testDatabase = openDatabase('test_Db', '1.0', 'Sencha Test', 5 * 1024 * 1024);

            testDatabase = Ext.space.Sqlite.openDatabase({
                name: 'sencha_test',
                version: '1',
                displayName: 'workspace unit test db',
                estimatedSize: 5 * 1024 * 1024
            });

            expect(testDatabase).to.exist;
            expect(testDatabase.transaction).to.exist;
            expect(testDatabase.readTransaction).to.exist;

            done();
        });

        it("should be able to create a transaction", function(done) {
            testDatabase.transaction({
                callback: function(transaction) {
                    expect(transaction).to.exist;
                    expect(transaction.executeSql).to.exist;
                },
                success:  function() {
                    console.log("success callback", arguments);

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to drop a table", function(done) {
            testDatabase.transaction({
                callback: function(transaction) {
                    console.log("drop table transaction begin");

                    expect(transaction).to.exist;

                    transaction.executeSql({
                        sqlStatement: "DROP TABLE IF EXISTS testTable",
                        arguments: [],
                        callback: function(tx, results) {
                            expect(tx).to.exist;

                            expect(results).to.exist;
                        },
                        failure: function(tx, err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });

                    console.log("drop table transaction end");
                },
                success:  function() {
                    console.log("success callback", arguments);

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to create a table", function(done) {
            testDatabase.transaction({
                callback: function(transaction) {
                    console.log("create table transaction begin");

                    expect(transaction).to.exist;

                    transaction.executeSql({
                        sqlStatement: "CREATE TABLE IF NOT EXISTS testTable (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, text TEXT)",
                        arguments: [],
                        callback: function(tx, results) {
                            expect(tx).to.exist;

                            expect(results).to.exist;
                        },
                        failure: function(tx, err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });

                    console.log("create table transaction end");
                },
                success:  function() {
                    console.log("success callback", arguments);

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to insert records into a table", function(done) {
            testDatabase.transaction({
                callback: function(transaction) {
                    console.log("insert transaction begin");

                    expect(transaction).to.exist;

                    var executeSqlNumber = 0;

                    transaction.executeSql({
                        sqlStatement: "insert into testTable(text) values('a');",
                        callback: function(tx, results) {
                            expect(++executeSqlNumber).to.equal(1);

                            expect(tx).to.exist;

                            expect(results).to.exist;
                            expect(results.rowsAffected).to.equal(1);
                        },
                        failure: function(tx, err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });

                    transaction.executeSql({
                        sqlStatement: "insert into testTable(text) values('b');",
                        arguments: [],
                        callback: function(tx, results) {
                            expect(++executeSqlNumber).to.equal(2);

                            expect(tx).to.exist;

                            expect(results).to.exist;
                            expect(results.rowsAffected).to.equal(1);
                        },
                        failure: function(tx, err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });

                    transaction.executeSql({
                        sqlStatement: "insert into testTable(text) values(?);",
                        arguments: ['c'],
                        callback: function(tx, results) {
                            expect(++executeSqlNumber).to.equal(3);

                            expect(tx).to.exist;

                            expect(results).to.exist;
                            expect(results.rowsAffected).to.equal(1);
                        },
                        failure: function(tx, err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });

                    console.log("insert transaction end");
                },
                success:  function() {
                    console.log("success callback", arguments);

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able select data from a table", function(done) {
            testDatabase.transaction({
                callback: function(transaction) {
                    console.log("select transaction begin");

                    expect(transaction).to.exist;

                    transaction.executeSql({
                        sqlStatement: "select * from testTable",
                        arguments: [],
                        callback: function(tx, results) {
                            expect(tx).to.exist;

                            expect(results).to.exist;
                            expect(results.rowsAffected).to.equal(0);
                            expect(results.rows).to.exist;

                            expect(results.rows.names).to.exist;
                            expect(results.rows.names.length).to.equal(2);
                            expect(results.rows.names[0]).to.equal("id");
                            expect(results.rows.names[1]).to.equal("text");

                            expect(results.rows.rows).to.exist;
                            expect(results.rows.rows.length).to.equal(3);

                            expect(results.rows.rows[0][0] == 1).to.be.true;
                            expect(results.rows.rows[0][1]).to.equal("a");

                            expect(results.rows.rows[1][0] == 2).to.be.true;
                            expect(results.rows.rows[1][1]).to.equal("b");

                            expect(results.rows.rows[2][0] == 3).to.be.true;
                            expect(results.rows.rows[2][1]).to.equal("c");
                        },
                        failure: function(tx, err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });

                    console.log("select transaction end");
                },
                success:  function() {
                    console.log("success callback", arguments);

                    done();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to update a record", function(done) {
            var checkUpdate = function() {
                testDatabase.transaction({
                    callback: function(transaction) {
                        console.log("select after update transaction begin");

                        expect(transaction).to.exist;

                        transaction.executeSql({
                            sqlStatement: "select * from  testTable where id = 1;",
                            arguments: [],
                            callback: function(tx, results) {
                                expect(tx).to.exist;

                                expect(results).to.exist;
                                expect(results.rows).to.exist;
                                expect(results.rows.rows).to.exist;
                                expect(results.rows.rows.length).to.equal(1);
                                expect(results.rows.rows[0][0] == 1).to.be.true;
                                expect(results.rows.rows[0][1]).to.equal("updated");
                            },
                            failure: function(tx, err) {
                                console.log("error callback", arguments);

                                throw err;
                                done();
                            }
                        });

                        console.log("select after update transaction end");
                    },
                    success:  function() {
                        console.log("success callback", arguments);

                        done();
                    },
                    failure: function(err) {
                        console.log("error callback", arguments);

                        throw err;
                        done();
                    }
                });
            }

            testDatabase.transaction({
                callback: function(transaction) {
                    console.log("update transaction begin");

                    expect(transaction).to.exist;

                    transaction.executeSql({
                        sqlStatement: "update testTable set text = 'updated' where id = 1;",
                        arguments: [],
                        callback: function(tx, results) {
                            expect(tx).to.exist;

                            expect(results).to.exist;
                            expect(results.rowsAffected).to.equal(1);
                        },
                        failure: function(tx, err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });

                    console.log("update transaction end");
                },
                success:  function() {
                    checkUpdate();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should be able to delete a record", function(done) {
            var checkDelete = function() {
                testDatabase.transaction({
                    callback: function(transaction) {
                        console.log("select after delete transaction begin");

                        expect(transaction).to.exist;

                        transaction.executeSql({
                            sqlStatement: "select * from  testTable where id = 1;",
                            arguments: [],
                            callback: function(tx, results) {
                                expect(tx).to.exist;

                                expect(results).to.exist;
                                expect(results.rows).to.exist;
                                expect(results.rows.rows).to.exist;
                                expect(results.rows.rows.length).to.equal(0);
                            },
                            failure: function(tx, err) {
                                console.log("error callback", arguments);

                                throw err;
                                done();
                            }
                        });

                        console.log("select after delete transaction end");
                    },
                    success:  function() {
                        console.log("success callback", arguments);

                        done();
                    },
                    failure: function(err) {
                        console.log("error callback", arguments);

                        throw err;
                        done();
                    }
                });
            }

            testDatabase.transaction({
                callback: function(transaction) {
                    console.log("delete transaction begin");

                    expect(transaction).to.exist;

                    transaction.executeSql({
                        sqlStatement: "delete from testTable where id = 1;",
                        arguments: [],
                        callback: function(tx, results) {
                            expect(tx).to.exist;

                            expect(results).to.exist;
                            expect(results.rowsAffected).to.equal(1);
                        },
                        failure: function(tx, err) {
                            console.log("error callback", arguments);

                            throw err;
                            done();
                        }
                    });

                    console.log("delete transaction end");
                },
                success:  function() {
                    checkDelete();
                },
                failure: function(err) {
                    console.log("error callback", arguments);

                    throw err;
                    done();
                }
            });
        });

        it("should throw an error on invalid Sql", function(done) {
            testDatabase.transaction({
                callback: function(transaction) {
                    console.log("invalid Sql transaction begin");

                    expect(transaction).to.exist;

                    transaction.executeSql({
                        sqlStatement: "DROP TABLE IF NOT EXISTS testTable",
                        arguments: [],
                        callback: function(tx, results) {
                            console.log("success callback", arguments);

                            throw "should fail with error on invalid Sql";
                            done();
                        },
                        failure: function(tx, err) {
                            console.log("error callback", arguments);

                            expect(tx).to.exist;

                            expect(err).to.exist;
                            expect(err).to.contain("Could not execute statement: near \"NOT\": syntax error");
                        }
                    });

                    console.log("invalid Sql transaction end");
                },
                success:  function() {
                    console.log("success callback", arguments);

                    done();
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
