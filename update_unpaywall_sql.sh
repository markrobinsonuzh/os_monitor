#!/bin/bash
# script to automatically download changefiles from unpaywall and update the local postgresql database

# number of most recent files to insert into database:
NRFILES=3
INTERVAL=week
if [ "$INTERVAL" = "week" ];
then
	# download overview file of all changefiles
	wget -O /home/data_feed_main.json --no-check-certificate https://api.unpaywall.org/feed/changefiles?api_key=ibDEKRZbA2
	# parse filenames into array
	mapfile -t urls_arr < <( jq -cr '.[] | sort_by(.from_date) | reverse | .[]  |  .url | match(".*jsonl.*") | .string' /home/data_feed_main.json | head -n $NRFILES | tac)
else
        # download overview file of all changefiles
	wget -O /home/data_feed_main.json --no-check-certificate "https://api.unpaywall.org/feed/changefiles?api_key=ibDEKRZbA2&interval=day"
        # parse filenames into array
        mapfile -t urls_arr < <( jq -cr '.[] | sort_by(.from_date) | reverse | .[]  |  .url | match(".*jsonl.*") | .string' /home/data_feed_main.json | tail -n $NRFILES )
fi
# loop through filenames
for i in "${urls_arr[@]}"
do
	echo "download file: $i"
	wget -q -O /home/data_feed.json.gz --no-check-certificate ${i}
	status1=$?
	if [ $status1 -eq 0 ];
	then 
		echo "        download was successful" 
	else 
		echo "        download failed with exit code $status1"
		exit 1
	fi

	echo "    $(date) gunzip file " 
	gunzip -f /home/data_feed.json.gz
	status2=$?
        if [ $status2 -eq 0 ];
        then
                echo "        gunzip was successful"
        else
                echo "        gunzip failed with exit code $status2"
                exit 1
        fi
	
	echo "    $(date) change \\\" to \" for correct parsing"
	#sed -i 's/\\\\/\\/g' /home/data_feed.json
	sed -i 's/\$\$[^$]*\$\$//g' /home/data_feed.json
        status22=$?
        if [ $status22 -eq 0 ];
        then
                echo "        sed was successful"
        else
                echo "        sed failed with exit code $status22"
                exit 1
        fi

	echo "    $(date) remove latex equations"
	sed -i 's/\$[^$]*\$//g' /home/data_feed.json
        status23=$?
        if [ $status23 -eq 0 ];
        then
                echo "        sed was successful"
        else
                echo "        sed failed with exit code $status23"
                exit 1
        fi

	echo "    $(date) jq for processing"
        # process json format into two column (doi and oa status) csv file (not comma separated but single whitespace separated, important for insertion into db)	
	jq -r '{doi: .doi | ascii_downcase, oa_status: .oa_status} | .doi , .oa_status' /home/data_feed.json | awk '{if(NR%2!=0) printf("%-10s ",$0); else printf("%-10s",$0); if(NR%2==0) printf("\n");}' | awk '{$1=$1;print}' > /home/data_feed.csv
        status3=$?
        if [ $status3 -eq 0 ];
        then
                echo "        jq processing was successful"
        else
                echo "        jq processing failed with exit code $status3"
                exit 1
        fi

	# run sql script to insert
	echo "    $(date) insert into postgresql"
	psql -U shiny -d oa -a -f /home/upsert_unpaywall.sql
        status4=$?
        if [ $status4 -eq 0 ];
        then
                echo "        postgresql insertion was successful" 
        else
                echo "        postgresql insertion failed with exit code $status4"
                exit 1
        fi
	
	echo "file ${i} successfully inserted into postgresql"
	echo "..............................................."
	echo ""	
done
echo "done"
echo "remove downloaded files"
rm /home/data_feed_main.json 
rm /home/data_feed.json 
rm /home/data_feed.csv
