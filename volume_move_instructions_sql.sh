#!/bin/bash

# to pack volume into tar
docker run --rm --volumes-from sqldb_db_1 -v $(pwd):/unsynced ubuntu tar cvf /unsynced/backup.tar /var/lib/postgresql/data


# to pack data into volume
docker volume create sql_data_os_monitor
docker run --mount source=sql_data_os_monitor,target=/var/lib/postgresql/data --name tmp_db ubuntu /bin/bash
docker run --rm --volumes-from tmp_db --mount type=bind,source=$(pwd),target=/home/loca ubuntu bash -c "cd /var/lib/postgresql/data && tar xvf /home/loca/backup.tar --strip 4"
