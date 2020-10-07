#!/bin/bash

wget -O /home/data_feed_main.json --no-check-certificate https://api.unpaywall.org/feed/changefiles?api_key=ibDEKRZbA2
mapfile -t urls_arr < <( jq -cr '.[] | sort_by(.from_date) | reverse | .[]  |  .url | match(".*jsonl.*") | .string' /home/data_feed_main.json | head -n 1)
for i in "${urls_arr[@]}"
do
	echo "download file: $i"
	wget -O /home/data_feed.json.gz --no-check-certificate ${i} 
	echo "gunzip file " 
	gunzip -f /home/data_feed.json.gz 
	echo "jq for processing" 
	# jq -c '{doi: .doi | ascii_downcase, oa_status: .oa_status}' /home/data_feed.json > /home/data_feed_sub.json 
	#jq -r ".doi | ascii_downcase, .oa_status " /home/data_feed.json | awk '{if(NR%2!=0) printf("%-10s, ",$0); else printf("%-10s ",$0); if(NR%2==0) printf("\n");}' > /home/data_feed.csv
	 jq -r '{doi: .doi | ascii_downcase, oa_status: .oa_status} | .doi , .oa_status' /home/data_feed.json | awk '{if(NR%2!=0) printf("%-10s ",$0); else printf("%-10s",$0); if(NR%2==0) printf("\n");}' | awk '{$1=$1;print}' > /home/data_feed.csv

	# run sql here
	psql -U shiny -d oa -a -f /home/upsert_unpaywall.sql 	
	
	#rm /home/data_feed_sub.json 
	#rm /home/data_feed.json 
	echo "file ${i} successfully inserted into postgresql"
	echo "..."	
done
echo "done"

