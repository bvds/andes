#
#  If file exists, read in database login information.
#  See Documentation/server.html for instructions.
#
dbLogin = ../../db_user_password
ifeq ($(shell test -f ${dbLogin} && echo 1),1)
  dbUser := $(shell sed -n -e 1p ${dbLogin})
  dbPass := $(shell sed -n -e 2p  ${dbLogin})
  dbName := $(or $(shell sed -n -e 3p ${dbLogin})andes_test)
  dbFrom := "Reading database login from ${dbLogin}"
else
  dbUser := root
  dbPass := 
  dbName := andes3
  dbFrom := "See Documentation/server.html for instructions on creating ${dbLogin}."
endif
versionFile := db_update_version
version := $(shell test -f ${versionFile} && cat ${versionFile} || echo "0")
#  should be incremented as changes are made to database
installVersion := 1

install:
	@echo "This will destroy any existing database!"
	@echo "Enter mysql password:"
	mysql -u  ${dbUser} -p < AndesDatabaseCreationSQL.sql
	echo "${installVersion}" > ${versionFile}

try:
	@echo "${dbFrom} ${version} ${installVersion}"
	mysql -u ${dbUser} -p'${dbPass}' -e 'SHOW TABLES;' ${dbName}

update:
	@echo ${dbFrom}
ifeq ($(shell test ${version} -lt 1 && echo 1),1)
	$(MAKE) rename-database
	echo 1 > ${versionFile}
endif
ifeq ($(shell test ${version} -lt 2 && echo 1),1)
	mysqldump -v -u ${dbUser} -p'${dbPass}' ${dbName} > \
	   ${dbName}-${version}-dump.sql
	mysql -u ${dbUser} -p'${dbPass}' ${dbName} < update-2.sql
	echo 2 > ${versionFile}
endif


# Test for mysql database name based on default mysql
# file locations in Linux.
# Once everyone's database is updated, this can be removed, Bug #1773.
# On Ubuntu, /var/lib/mysql is not readable, but all Ubuntu 
# installations are younger than the database rename.
OLD_DBNAME=$(shell test -r /var/lib/mysql && cd /var/lib/mysql && test -d andes -a ! -d andes3 && echo "1")
rename-database:
ifeq (${OLD_DBNAME},1)
	@echo "Your Andes database name needs to be updated."
	@echo "The following will rename the database from andes to andes3."
	test -f andes.sql && mv andes.sql andes.sql.bak || test 1
	@echo "Dump 'andes' database."
	mysqldump -u  ${dbUser} -p'${dbPass}' -v andes > andes.sql
	@echo "Create 'andes3' database."
	mysqladmin -u  ${dbUser} -p'${dbPass}' create ${dbName}
	@echo "Load new database.
	mysql -u  ${dbUser} -p'${dbPass}' ${dbName} < andes.sql
	@echo "Drop old database."
	mysqladmin -u  ${dbUser} -p'${dbPass}' drop andes
endif
