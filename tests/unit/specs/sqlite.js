(function() {

var testDatabase = null;

describe("Sqlite", function() {
    describe("Ext.space.Sqlite", function() {
        it("Ext.space.Sqlite should exist", function(done) {
            expect(Ext.space.Sqlite).to.exist;

            done();
        });

        it("should be able to open a database", function(done) {
            Ext.space.Sqlite.openDatabase({
                name: 'sencha_test',
                version: '1',
                displayName: 'workspace unit test db',
                estimatedSize: 5 * 1024 * 1024
            }).then(function(db) {
                testDatabase = db;

                expect(testDatabase).to.exist;
                expect(testDatabase.transaction).to.exist;
                expect(testDatabase.readTransaction).to.exist;
            }).then(done).error(done);
        });

        it("should be able to create a transaction", function(done) {
            testDatabase.transaction().then(function(t) {
                expect(t).to.exist;
                expect(t.executeSql).to.exist;
                expect(t.id > 0).to.be.true;

                return t.run();
            }).then(done).error(done);
        });

        it("should be able to drop a table", function(done) {
            testDatabase.transaction().then(function(t) {
                expect(t).to.exist;

                t.executeSql("DROP TABLE IF EXISTS testTable").then(function(rs) {
                    expect(rs).to.exist;
                });

                return t.run();
            }).then(done).error(done);
        });

        it("should be able to create a table", function(done) {
            testDatabase.transaction().then(function(t) {
                expect(t).to.exist;

                t.executeSql("CREATE TABLE IF NOT EXISTS testTable (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, text TEXT)").then(function(rs) {
                    expect(rs).to.exist;
                });

                return t.run();
            }).then(done).error(done);
        });

        it("should be able to insert records into a table", function(done) {
            testDatabase.transaction().then(function(t) {
                expect(t).to.exist;

                var executeSqlNumber = 0;

                t.executeSql("insert into testTable(text) values('a');").then(function(rs) {
                    expect(++executeSqlNumber).to.equal(1);
                    expect(rs).to.exist;
                    expect(rs.rowsAffected).to.equal(1);
                });

                t.executeSql("insert into testTable(text) values('b');").then(function(rs) {
                    expect(++executeSqlNumber).to.equal(2);
                    expect(rs).to.exist;
                    expect(rs.rowsAffected).to.equal(1);
                });

                t.executeSql("insert into testTable(text) values(?);", ['c']).then(function(rs) {
                    expect(++executeSqlNumber).to.equal(3);
                    expect(rs).to.exist;
                    expect(rs.rowsAffected).to.equal(1);
                });

                return t.run();
            }).then(done).error(done);
        });

        it("should be able select data from a table", function(done) {
            testDatabase.transaction().then(function(t) {
                expect(t).to.exist;

                t.executeSql("select * from testTable").then(function(rs) {
                    expect(rs).to.exist;

                    expect(rs.rowsAffected).to.equal(0);
                    expect(rs.rows).to.exist;

                    expect(rs.rows.names).to.exist;
                    expect(rs.rows.names.length).to.equal(2);
                    expect(rs.rows.names[0]).to.equal("id");
                    expect(rs.rows.names[1]).to.equal("text");

                    expect(rs.rows.rows).to.exist;
                    expect(rs.rows.rows.length).to.equal(3);

                    expect(rs.rows.rows[0][0] == 1).to.be.true;
                    expect(rs.rows.rows[0][1]).to.equal("a");

                    expect(rs.rows.rows[1][0] == 2).to.be.true;
                    expect(rs.rows.rows[1][1]).to.equal("b");

                    expect(rs.rows.rows[2][0] == 3).to.be.true;
                    expect(rs.rows.rows[2][1]).to.equal("c");
                });

                return t.run();
            }).then(done).error(done);
        });

        it("should be able to update a record", function(done) {
            testDatabase.transaction().then(function(t) {
                expect(t).to.exist;

                t.executeSql("update testTable set text = 'updated' where id = 1").then(function(rs) {
                    expect(rs).to.exist;
                    expect(rs.rowsAffected).to.equal(1);
                });

                return t.run().then(function() {
                    return testDatabase.transaction().then(function(t2) {
                        expect(t2).to.exist;

                        t2.executeSql("select * from testTable where id = 1").then(function(rs) {
                            expect(rs).to.exist;
                            expect(rs.rows).to.exist;
                            expect(rs.rows.rows).to.exist;
                            expect(rs.rows.rows.length).to.equal(1);
                            expect(rs.rows.rows[0][0] == 1).to.be.true;
                            expect(rs.rows.rows[0][1]).to.equal("updated");
                        });

                        return t2.run();
                    });
                });
            }).then(done).error(done);
        });

        it("should be able to delete a record", function(done) {
            testDatabase.transaction().then(function(t) {
                expect(t).to.exist;

                t.executeSql("delete from testTable where id = 1").then(function(rs) {
                    expect(rs).to.exist;
                    expect(rs.rowsAffected).to.equal(1);
                });

                return t.run().then(function() {
                    return testDatabase.transaction().then(function(t2) {
                        expect(t2).to.exist;

                        t2.executeSql("select * from testTable where id = 1").then(function(rs) {
                            expect(rs).to.exist;
                            expect(rs.rows).to.exist;
                            expect(rs.rows.rows).to.exist;
                            expect(rs.rows.rows.length).to.equal(0);
                        });

                        return t2.run();
                    });
                });
            }).then(done).error(done);
        });

        it("should throw an error on invalid Sql", function(done) {
            testDatabase.transaction().then(function(t) {
                expect(t).to.exist;

                t.executeSql("DROP TABLE IF NOT EXISTS testTable");

                return t.run();
            }).then(function() {
                done('should fail with error on invalid Sql');
            }).error(function(e) {
                try {
                    expect(e).to.exist;
                    expect(e).to.contain("Could not execute statement: near \"NOT\": syntax error");
                    done();
                } catch(err) {
                    done(err);
                }
            });
        });
    });
});

})();
