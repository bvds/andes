/**
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
 * The SqlTransaction class which is used to execute Sql statements.
 */
Ext.define('Ext.space.sqlite.SqlTransaction', {
    id: 0,
    active: false,
    statements: null,

    constructor: function(id) {
        this.id = id;
        this.statements = [];
    },

    /**
     * Executes an Sql statement.
     *
     * @param {Object} config
     * The object which contains the following config options:
     *
     * @param {String} config.sqlStatement
     * The Sql statement to execute. This is required.
     *
     * @param {Array} config.arguments
     * The arguments array to bind each '?' placeholder in the Sql statement. This is optional.
     *
     * @param {Function} config.callback
     * The callback to be called when the Sql statement succeeded. This is optional.
     *
     * @param {Ext.space.sqlite.SqlTransaction} config.callback.transaction
     * The transaction of the Sql statement.
     *
     * @param {Ext.space.sqlite.SqlTransaction} config.callback.resultSet
     * The result of the Sql statement.
     *
     * @param {Function} config.failure
     * The callback to be called when an error occurred. This is optional.
     * If the callback returns false, next Sql statement will be executed.
     *
     * @param {Ext.space.sqlite.SqlTransaction} config.failure.transaction
     * The transaction of the Sql statement.
     *
     * @param {Object} config.failure.error
     * The occurred error.
     *
     * @param {Object} config.scope
     * The scope object
     */
    executeSql: function(config) {
        if (!this.active) {
            throw new Error('Ext.space.sqlite.SqlTransaction#executeSql: An attempt was made to use a SqlTransaction that is no longer usable.');
            return null;
        }

        if (config.sqlStatement == null) {
            throw new Error('Ext.space.sqlite.SqlTransaction#executeSql: You must specify a `sqlStatement` for the transaction.');
            return null;
        }

        this.statements.push({
            sqlStatement: config.sqlStatement,
            arguments: config.arguments,
            callback: config.callback,
            failure: config.failure,
            scope: config.scope
        });
    }
});

/**
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
     * @return {String}
     * The current database version.
     */
    getVersion: function() {
        return Ext.space.Communicator.send({
            command: 'Sqlite#getVersion',
            databaseId: this.id
        }, true);
    },

    /**
     * Performs a {@link Ext.space.sqlite.SqlTransaction} instance with a read/write mode.
     *
     * @param {Object} config
     * The object which contains the following config options:
     *
     * @param {Function} config.callback
     * The callback to be called when the transaction has been created. This is required.
     *
     * @param {Ext.space.sqlite.SqlTransaction} config.callback.transaction
     * The created transaction.
     *
     * @param {Function} config.success
     * The callback to be called when the transaction has been successfully commited. This is optional.
     *
     * @param {Function} config.failure
     * The callback to be called when an error occurred and the transaction has been rolled back. This is optional.
     *
     * @param {Object} config.failure.error
     * The occurred error.
     *
     * @param {Object} config.scope
     * The scope object
     */
    transaction: function(config) {
        if (!config.callback) {
            throw new Error('Ext.space.sqlite.Database#transaction: You must specify a `callback` callback.');
            return null;
        }

        var me = this;
        Ext.space.Communicator.send({
            command: 'Sqlite#createTransaction',
            databaseId: this.id,
            readOnly: config.readOnly,
            callbacks: {
                success: function(id) {
                    var exception = null;
                    var error = null;
                    var transaction = new Ext.space.sqlite.SqlTransaction(id);

                    error = Ext.space.Communicator.send({
                        command: 'Sqlite#beginTransaction',
                        transactionId: transaction.id
                    }, true);

                    if (!error && config.preflight) {
                        error = config.preflight.call(config.scope || this);
                    }

                    if (!error) {
                        try {
                            transaction.active = true;
                            config.callback.call(config.scope || this, transaction); // may throw exception
                        } catch (e) {
                            exception = e;
                        } finally {
                            transaction.active = false;
                        }
                    }

                    var statements = transaction.statements;

                    while (!(exception || error) && statements.length > 0) {
                        var statement = statements.shift();
                        var result = Ext.space.Communicator.send({
                            command: 'Sqlite#executeStatement',
                            transactionId: transaction.id,
                            databaseId: me.id,
                            version: me.version,
                            sqlStatement: statement.sqlStatement,
                            arguments: JSON.stringify(statement.arguments)
                        }, true);

                        if (result) {
                            if (result.error) {
                                error = result.error;
                            } else if (statement.callback) {
                                var resultSet = new Ext.space.sqlite.SqlResultSet(result);

                                try {
                                    transaction.active = true;
                                    statement.callback.call(statement.scope || this, transaction, resultSet); // may throw exception
                                } catch (e) {
                                    exception = e;
                                } finally {
                                    transaction.active = false;
                                }
                            }
                        }

                        if (error && statement.failure) {
                            try {
                                transaction.active = true;
                                if (!statement.failure.call(statement.scope || this, transaction, error)) { // may throw exception
                                    error = null;
                                }
                            } catch (e) {
                                exception = e;
                            } finally {
                                transaction.active = false;
                            }
                        }
                    }

                    if (!(exception || error)) {
                        error = Ext.space.Communicator.send({
                            command: 'Sqlite#commitTransaction',
                            transactionId: transaction.id
                        }, true);

                        if (!error) {
                            if (config.postflight) {
                                config.postflight.call(config.scope || this);
                            }

                            if (config.success) {
                                config.success.call(config.scope || this);
                            }
                        }
                    }

                    if (exception || error) {
                        statements.splice(0, statements.length);

                        Ext.space.Communicator.send({
                            command: 'Sqlite#rollbackTransaction',
                            transactionId: transaction.id
                        }, true);

                        if (exception) {
                            throw exception;
                        } else if (config.failure) {
                            config.failure.call(config.scope || this, error);
                        }
                    }
                },
                failure: function(error) {
                    if (config.failure) {
                        config.failure.call(config.scope || this, error);
                    }
                }
            },
            scope: config.scope || this
        });
    },

    /**
     * Works same as {@link Ext.space.sqlite.Database#transaction}, but performs a {@link Ext.space.sqlite.SqlTransaction} instance with a read-only mode.
     */
    readTransaction: function(config) {
        this.transaction(Ext.apply(config, {
            readOnly: true
        }));
    },

    /**
     * Verifies and changes the version of the database at the same time as doing a schema update with a {@link Ext.space.sqlite.SqlTransaction} instance.
     *
     * @param {Object} config
     * The object which contains the following config options:
     *
     * @param {String} config.oldVersion
     * The current version of the database. This is required.
     *
     * @param {String} config.newVersion
     * The new version of the database. This is required.
     *
     * @param {Function} config.callback
     * The callback to be called when the transaction has been created. This is optional.
     *
     * @param {Ext.space.sqlite.SqlTransaction} config.callback.transaction
     * The created transaction.
     *
     * @param {Function} config.success
     * The callback to be called when the transaction has been successfully commited. This is optional.
     *
     * @param {Function} config.failure
     * The callback to be called when an error occurred and the transaction has been rolled back. This is optional.
     *
     * @param {Object} config.failure.error
     * The occurred error.
     *
     * @param {Object} config.scope
     * The scope object
     */
    changeVersion: function(config) {
        if (config.oldVersion == null) {
            throw new Error('Ext.space.Sqlite#changeVersion: You must specify an `oldVersion` of the database.');
            return null;
        }

        if (config.newVersion == null) {
            throw new Error('Ext.space.Sqlite#changeVersion: You must specify a `newVersion` of the database.');
            return null;
        }

        this.transaction(Ext.apply(config, {
            preflight: function() {
                return config.oldVersion == this.getVersion() ? null : 'Unable to change version: version mismatch';
            },
            postflight: function() {
                var result = Ext.space.Communicator.send({
                    command: 'Sqlite#setVersion',
                    databaseId: this.id,
                    version: config.newVersion
                }, true);

                if (result) {
                    this.version = config.newVersion;
                }
            }
        }));
    }
});

Ext.define('Ext.space.Sqlite', {
    singleton: true,

    /**
     * Returns a {@link Ext.space.sqlite.Database} instance. If the database with specified name does not exist, it will be created.
     * If the creationCallback is provided, the database is created with the empty string as its version regardless of the specified version.
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
     * @param {Function} config.creationCallback
     * The callback to be called when the database has been created. This is optional.
     *
     * @param {Ext.space.sqlite.Database} config.creationCallback.database
     * The created database with the empty string as its version regardless of the specified version.
     *
     * @param {Object} config.scope
     * The scope object. This is optional.
     *
     * @return {Ext.space.sqlite.Database}
     * The opened database, null if an error occured.
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

        var database = null;

        var result = Ext.space.Communicator.send({
            command: 'Sqlite#openDatabase',
            name: config.name,
            version: config.version,
            displayName: config.displayName,
            estimatedSize: config.estimatedSize,
            callbacks: {
                // `creationCallback != null` is checked for internal logic in native plugin code
                creationCallback: !config.creationCallback ? null : function() {
                    config.creationCallback.call(config.scope || this, database);
                }
            },
            scope: config.scope || this
        }, true);

        if (result) {
            if (result.error) {
                throw new Error(result.error);
                return null;
            }

            database = new Ext.space.sqlite.Database(result.id, result.version);

            return database;
        }

        return null;
    }
});
