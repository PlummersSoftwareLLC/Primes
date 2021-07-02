#!/bin/bash
# Init a new clean database
# This is done here because this install command performs actions that 
# are not persistent in the Docker image. So it can not be easy changed to a 
# Docker run command
mysql_install_db --user=mysql --basedir=/usr --datadir=/var/lib/mysql &>/dev/null

# Start server process
mariadbd  &>/dev/null &

# Wait for the server process to complete
for i in {30..0}; do
	if mysql -umysql --database=mysql <<<'SELECT 1' &> /dev/null; then
		break
	fi
	sleep 1
done
if [ "$i" = 0 ]; then
	echo "Unable to start server."
    exit
fi

# run the solutions
mysql -umysql --raw <primes_1.sql
mysql -umysql --raw <primes_2.sql
mysql -umysql --raw <primes_3.sql
