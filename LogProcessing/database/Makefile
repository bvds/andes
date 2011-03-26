#
#  If file exists, read in database login information.
#  See Documentation/server.html for instructions.
#
dbLogin = ../../db_user_password
ifeq ($(shell test -f ${dbLogin} && echo 1),1)
  dbUser := $(shell sed -n -e 1p ${dbLogin})
  dbPass := $(shell sed -n -e 2p  ${dbLogin})
# Use "if"; doing this as an "or" doesn't work in OS X
  dbNameIn := $(shell sed -n -e 3p ${dbLogin})
  dbName := $(if ${dbNameIn},${dbNameIn},andes3)
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
installVersion := 2

install:
	@echo "This will destroy any existing database!"
	@echo "Enter mysql password:"
	mysql -u  ${dbUser} -p < AndesDatabaseCreationSQL.sql
	echo "${installVersion}" > ${versionFile}

try:
	@echo "Test out access to database via login info."
	@echo "${dbFrom} version=${version} installVersion=${installVersion}"
	mysql -u ${dbUser} -p'${dbPass}' -e 'SHOW TABLES;' ${dbName}

update:
	@echo "${dbFrom}" 
ifeq ($(shell test ${version} -lt 1 && echo 1),1)
	$(MAKE) rename-database
	echo 1 > ${versionFile}
endif
ifeq ($(shell test ${version} -lt 2 && echo 1),1)
	mysqldump -u ${dbUser} -p'${dbPass}' ${dbName} > \
	   ${dbName}-${version}-dump.sql
	mysql -u ${dbUser} -p'${dbPass}' ${dbName} < modify_CLASS_INFORMATION.sql
	mysql -u ${dbUser} -p'${dbPass}' ${dbName} < modify_PROBLEM_ATTEMPT.sql
	mysql -u ${dbUser} -p'${dbPass}' ${dbName} < create_STEP_TRANSACTION.sql
	mysql -u ${dbUser} -p'${dbPass}' ${dbName} < create_STUDENT_STATE.sql
	mysql -u ${dbUser} -p'${dbPass}' andes_test < modify-andes-test-2.sql
	echo 2 > ${versionFile}
endif
ifeq ($(shell test ${version} -lt 3 && echo 1),1)
	mysqldump -u ${dbUser} -p'${dbPass}' ${dbName} > \
	   ${dbName}-${version}-dump.sql
	mysql -u ${dbUser} -p'${dbPass}' ${dbName} < modify_3.sql
	mysql -u ${dbUser} -p'${dbPass}' andes_test < modify_3.sql
	@echo "this update is safe to rerun, so leave in old version."
endif

# This is just for debugging update script.
reset:
	test -f pitt-laptop1-jan-2011.sql
	test -f andes-test-march-2011.sql
	mysqldump -u  ${dbUser} -p'${dbPass}' ${dbName} > ${dbName}-reset.sql
	mysqladmin -u  ${dbUser} -p'${dbPass}' drop ${dbName}
	mysqladmin -u  ${dbUser} -p'${dbPass}' create ${dbName}
	mysql -u ${dbUser} -p'${dbPass}' ${dbName} < pitt-laptop1-jan-2011.sql
	mysqladmin -u  ${dbUser} -p'${dbPass}' drop andes_test
	mysqladmin -u  ${dbUser} -p'${dbPass}' create andes_test
	mysql -u ${dbUser} -p'${dbPass}' andes_test < andes-test-march-2011.sql
	echo 1 > ${versionFile}

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
