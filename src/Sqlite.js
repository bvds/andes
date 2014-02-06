/**
 * @private
 * The SqlResultSetRowList class which is used to represent rows returned by Sql statements.
 */
Ext.define('Ext.space.sqlite.SqlResultSetRowList', {
    names: null,
    rows: null,

    constructor: function(data) {
        this.names = data.names;
        this.rows = data.rows;
    },

    /**
     * Returns the number of rows returned by the Sql statement.
     *
     * @return {Number}
     * The number of rows.
     */
    getLength: function() {
        return this.rows.length;
    },

    /**
     * Returns a row at specified index returned by the Sql statement.
     * If there is no such row, returns null.
     *
     * @param {Number} index
     * The index of a row. This is required.
     *
     * @return {Object}
     * The row.
     */
    item: function(index) {
        if (index < this.getLength()) {
            var item = {};
            var row = this.rows[index];
            this.names.forEach(function(name, index) {
                item[name] = row[index];
            });

            return item;
        }

        return null;
    }
});

/**
 * @private
 * The SqlResultSet class which is used to represent Sql statements results.
 */
Ext.define('Ext.space.sqlite.SqlResultSet', {
    insertId: 0,
    rowsAffected: 0,
    rows: null,

    constructor: function(data) {
        this.insertId = data.insertId;
        this.rowsAffected = data.rowsAffected;
        this.rows = new Ext.space.sqlite.SqlResultSetRowList(data);
    },

    /**
     * Returns the row ID of the last row that the Sql statement inserted into the database, if the statement inserted any rows.
     * If the statement did not insert a row, throws an exception.
     *
     * @return {Number}
     * The inserted row ID.
     */
    getInsertId: function() {
        if (this.insertId != 0) {
            return this.insertId;
        } else {
            throw new Error('Ext.space.sqlite.SqlResultSet#getInsertId: An SqlTransaction did not insert a row.');
            return null;
        }
    },

    /**
     * Returns the number of rows that were changed by the Sql statement.
     * If the statement did not change any rows, returns zero.
     *
     * @return {Number}
     * The number of rows affected.
     */
    getRowsAffected: function() {
        return this.rowsAffected;
    },

    /**
     * Returns a {@link Ext.space.sqlite.SqlResultSetRowList} instance representing rows returned by the Sql statement.
     *
     * @return {Ext.space.sqlite.SqlResultSetRowList}
     * The rows.
     */
    getRows: function() {
        return this.rows;
    }
});

/**
 *
 * @private
 * The SqlTransaction class which is used to execute Sql statements.
 */
Ext.define('Ext.space.sqlite.SqlTransaction', {
    id: 0,
    database: null,
    valid: true,
    statements: null,
    promise: null,

    constructor: function(id, database) {
        this.id = id;
        this.database = database;
        this.statements = [];
        this.promise = new Ext.Promise();
    },

    /**
     * Executes a Sql statement.
     *
     * @param {String} sql
     * The Sql statement to execute. This is required.
     *
     * @param {Array} args
     * The arguments array to bind each '?' placeholder in the Sql statement. This is optional.
     *
     * @return {Ext.Promise}
     * The promise that is resolved when the Sql statement has finished executing.
     */
    executeSql: function(sql, args) {
        if (!this.valid) {
            throw new Error('Ext.space.sqlite.SqlTransaction#executeSql: An attempt was made to use a SqlTransaction that is no longer usable.');
            return null;
        }

        if (sql == null) {
            throw new Error('Ext.space.sqlite.SqlTransaction#executeSql: You must specify `sql` for the transaction.');
            return null;
        }

        var promise = new Ext.Promise();

        this.statements.push({
            sql: sql,
            args: args,
            promise: promise
        });

        return promise;
    },

    begin: function() {
        var me = this,
            promise = new Ext.Promise(),
            error;

        if (me.valid) {
            me.valid = false;
            error = Ext.space.Communicator.send({
                command: 'Sqlite#beginTransaction',
                transactionId: me.id,
                callbacks: {
                    success: function() {
                        promise.fulfill();
                    },
                    failure: function(e) {
                        promise.reject(e);
                        me.rollback(e);
                    }
                }
            }, true);

            if (error) {
                promise.reject(error);
                me.rollback(error);
            } else if (error === '') {
                promise.fulfill();
            }
        } else {
            promise.reject('Ext.space.sqlite.SqlTransaction#begin: Transaction has already been started');
        }

        return promise;
    },

    execute: function() {
        var me = this;

        function next(stmt) {
            if (!stmt) {
                me.commit();
                return;
            }

            var result = Ext.space.Communicator.send({
                command: 'Sqlite#executeStatement',
                transactionId: me.id,
                databaseId: me.database.id,
                version: me.database.version,
                sqlStatement: stmt.sql,
                arguments: JSON.stringify(stmt.args),
                callbacks: {
                    success: function(rs) {
                        // Protect against a DB deadlock in case promise handler throws an exception.
                        try {
                            stmt.promise.fulfill(new Ext.space.sqlite.SqlResultSet(rs));
                            next(me.statements.shift());
                        } catch(e) {
                            stmt.promise.reject(e);
                            me.rollback(e);
                        }
                    },
                    failure: function(e) {
                        // Protect against a DB deadlock in case promise handler throws an exception.
                        try {
                            stmt.promise.reject(result.error);
                        } catch(e) {}
                        me.rollback(e);
                    }
                }
            }, true);

            if (result) {
                if (result.error) {
                    // Protect against a DB deadlock in case promise handler throws an exception.
                    try {
                        stmt.promise.reject(result.error);
                    } catch(e) {}
                    me.rollback(result.error);
                } else {
                    // Protect against a DB deadlock in case promise handler throws an exception.
                    try {
                        stmt.promise.fulfill(new Ext.space.sqlite.SqlResultSet(result));
                        next(me.statements.shift());
                    } catch(e) {
                        stmt.promise.reject(e);
                        me.rollback(e);
                    }
                }
            }
        }

        next(me.statements.shift());
    },

    commit: function() {
        var me = this,
            error = Ext.space.Communicator.send({
                command: 'Sqlite#commitTransaction',
                transactionId: me.id,
                callbacks: {
                    success: function() {
                        me.promise.fulfill();
                    },
                    failure: me.rollback
                },
                scope: me
            }, true);

        if (error) {
            me.rollback(error);
        } else if (error === '') {
            me.promise.fulfill();
        }
    },

    rollback: function(e) {
        Ext.space.Communicator.send({
            command: 'Sqlite#rollbackTransaction',
            transactionId: this.id
        }, true);

        this.promise.reject(e);
    },

    /**
     * Batch executes all queued Sql statements inside a transaction, handling errors and commit/rollback automatically.
     *
     * @return {Ext.Promise}
     * The promise that is resolved when the transaction has been committed or rolled back.
     */
    run: function() {
        this.begin().then(this, this.execute);
        return this.promise;
    }
});

/**
 * @private
 *
 * The Database class which is used to perform transactions.
 */
Ext.define('Ext.space.sqlite.Database', {
    id: 0,
    version: null,

    constructor: function(id, version) {
        this.id = id;
        this.version = version;
    },

    /**
     * Returns the current version of the database.
     *
     * @return {Ext.Promise}
     * The promise that will resolve when the version is returned.
     */
    getVersion: function() {
        var me = this,
            result = new Ext.Promise(),
            version = Ext.space.Communicator.send({
                command: 'Sqlite#getVersion',
                databaseId: this.id,
                callbacks: {
                    success: function(vesion) {
                        me.version = version;
                        result.fulfill(version);
                    },
                    failure: function(e) {
                        result.reject(e);
                    }
                }
            }, true);

        // A version could technically be an empty string, and that's valid.
        if (version || version === '') {
            me.version = version;
            result.fulfill(version);
        }

        return result;
    },

    /**
     * Performs a {@link Ext.space.sqlite.SqlTransaction} instance with a read/write mode.
     *
     * @return {Ext.Promise}
     * The promise that is resolved when the transaction has successfully been created.
     */
    transaction: function(config) {
        config = config || {};
        var promise = new Ext.Promise(),
            me = this;

        Ext.space.Communicator.send({
            command: 'Sqlite#createTransaction',
            databaseId: this.id,
            readOnly: config.readOnly,
            callbacks: {
                success: function(id) {
                    promise.fulfill(new Ext.space.sqlite.SqlTransaction(id, me));
                },
                failure: function(e) {
                    promise.reject(e);
                }
            }
        });

        return promise;
    },

    /**
     * Works same as {@link Ext.space.sqlite.Database#transaction}, but performs a {@link Ext.space.sqlite.SqlTransaction} instance in read-only mode.
     */
    readTransaction: function(config) {
        return this.transaction(Ext.apply(config || {}, {
            readOnly: true
        }));
    },

    /**
     * Verifies and changes the version of the database at the same time as doing a schema update with a {@link Ext.space.sqlite.SqlTransaction} instance.
     *
     * @param {String} version
     * The new version of the database. This is required.
     *
     * @return {Ext.Promise}
     * The promise that is resolved when the database version has been changed, passing in the {Ext.space.sqlite.SqlTransaction} instance
     * that is bound to the version change operation.
     */
    changeVersion: function(version) {
        var me = this;

        if (version == null) {
            throw new Error('Ext.space.sqlite.Database#changeVersion: You must specify a `version` for the database.');
            return null;
        }

        var promise = new Ext.Promise(),
            reject = function(e) {
                promise.reject(e);
            };

        me.getVersion().then(function(v) {
            if (v != me.version) {
                reject('Ext.space.sqlite.Database#changeVersion: Unable to change version due to a version mismatch');
                return;
            }

            return me.transaction().then(function(t) {
                return t.begin().then(function() {
                    var result = Ext.space.Communicator.send({
                        command: 'Sqlite#setVersion',
                        databaseId: me.id,
                        version: version,
                        callbacks: {
                            success: function() {
                                me.version = version;
                                promise.fulfill(t);
                            },
                            failure: reject
                        }
                    }, true);

                    if (result) {
                        me.version = version;
                        promise.fulfill(t);
                    } else if (result === '') {
                        reject('Ext.space.sqlite.Database#changeVersion: Unable to change version');
                    }
                });
            });
        }).error(reject);

        return promise;
    }
});


/**
* @private
*
*/
Ext.define('Ext.space.Sqlite', {
    singleton: true,

    /**
     * Opens an instance of {@link Ext.space.sqlite.Database}. If the database with specified name does not exist, it will be created.
     *
     * @param {Object} config
     * The object which contains the following config options:
     *
     * @param {String} config.name
     * The name of the database to open. This is required.
     *
     * @param {String} config.version
     * The version of the database to open. This is required.
     *
     * @param {String} config.displayName
     * The display name of the database to open. This is required.
     *
     * @param {Number} config.estimatedSize
     * The estimated size of the database to open. This is required.
     *
     * @return {Ext.Promise}
     * The promise that will resolve when the {@link Ext.space.sqlite.Database} is opened and returned.
     */
    openDatabase: function(config) {
        if (config.name == null) {
            throw new Error('Ext.space.Sqlite#openDatabase: You must specify a `name` of the database.');
            return null;
        }

        if (config.version == null) {
            throw new Error('Ext.space.Sqlite#openDatabase: You must specify a `version` of the database.');
            return null;
        }

        if (config.displayName == null) {
            throw new Error('Ext.space.Sqlite#openDatabase: You must specify a `displayName` of the database.');
            return null;
        }

        if (config.estimatedSize == null) {
            throw new Error('Ext.space.Sqlite#openDatabase: You must specify a `estimatedSize` of the database.');
            return null;
        }

        var promise = new Ext.Promise(),
            createDatabase = function(db) {
                return new Ext.space.sqlite.Database(db.id, db.version);
            };

        var result = Ext.space.Communicator.send({
            command: 'Sqlite#openDatabase',
            name: config.name,
            version: config.version,
            displayName: config.displayName,
            estimatedSize: config.estimatedSize,
            callbacks: {
                success: function(db) {
                    promise.fulfill(createDatabase(db));
                },
                failure: function(e) {
                    promise.reject(e);
                }
            }
        }, true);

        if (result) {
            if (result.error) {
                promise.reject(result.error);
            } else {
                promise.fulfill(createDatabase(result));
            }
        }

        return promise;
    }
});
