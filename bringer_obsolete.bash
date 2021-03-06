#!/bin/bash

# todo: 
# re-implement "bringing"
# implement closing
# check double/single quoting etc.
# todos below 

function echoExtraEntries
{
    function echoSortLines
    {
	function echoSortLinesForDesktop
	{
	    function echoOnOneLine
	    {
		declare firstDone
		declare s
		while read s
		do
		    if [ ! $firstDone ] 
		    then
			firstDone=dummy
		    else
			echo -n " --- "
		    fi
		    echo -n "$s"
		done
	    }
	    
	    function echoPrefix
	    {
		declare s
		while read s
		do
    		    echo "$1:$2:$s"
		done
	    }
	    
	    declare commandWindowPairs=`echo -e "$1" | sort`
	    declare sortCommands=`echo "$commandWindowPairs" | cut -d : -f 1 | echoOnOneLine`
	    echo "$commandWindowPairs" | echoPrefix "$sortCommands" "$2"
	}
	
	function getCommand
	{
	    ps -p $1 -o command= | sed "s/^[^ ]*\///"
	}
	
	declare -i lastDesktop=-1
	declare commandWindowPairs
	declare -i window
	declare -i desktop
	declare -i pid
	declare command
	declare line
	while read line
	do
	    window=`echo "$line" | cut -d " " -f 1`
	    desktop=`echo "$line" | cut -d " " -f 3`
	    pid=`echo "$line" | cut -d " " -f 4`
        # title=`echo "$line" | cut -d " " -f 1,2,3,4,5,6 --complement` 
	    command=`getCommand $pid`
	    if [ $desktop -ne $lastDesktop ]
	    then
		if [ $lastDesktop -ne -1 ]
		then
		    echoSortLinesForDesktop "$commandWindowPairs" "$lastDesktop"
 		fi
		commandWindowPairs=$command:$window
		lastDesktop=$desktop
	    else
		commandWindowPairs="$commandWindowPairs\n$command:$window"
	    fi
	done
	echoSortLinesForDesktop "$commandWindowPairs" "$lastDesktop"
    }
    
    function echoAllLines
    {
	declare command
	declare -i window
	declare -i desktop
	declare -i lastDesktop=-1
	declare commands
	declare firstDone
	declare -i visibleDesktop=`wmctrl -d | grep \* | cut -d " " -f 1`
	declare line
	while read line
	do
	    command=`echo "$line" | cut -d : -f 3`
	    window=`echo "$line" | cut -d : -f 4`
	    desktop=`echo "$line" | cut -d : -f 2`
	    commands=`echo "$line" | cut -d : -f 1`
	    if [ $desktop -ne $lastDesktop ] 
	    then
		if [ $firstDone ]
		then
		    echo
		else
		    firstDone=dummy
		fi
		echo -n "g $window "
 		if [ $desktop -eq $visibleDesktop ] 
		then
		    echo -n "*"
		fi
		echo -n "$commands"
	    fi
	    lastDesktop=$desktop
	done
	echo

	function echoFrequencies
	{
	    declare -Ai frequencies
	    declare line
	    while read line
	    do
		frequencies["$line"]=${frequencies["$line"]}+1
	    done
	    for key in "${!frequencies[@]}"
	    do
		echo ${frequencies["$key"]} "$key"
	    done
	}

	function echoMFU
	{
	    declare line
	    declare firstDone
	    while read line
	    do
		echo -n "h "
		if [ $firstDone ]
		then
		    echo "$line" | cut -d " " --complement -f 1
		else
		    echo "`echo "$line" | cut -d " " --complement -f 1` --- `date +%H:%M`" # todo: enough hyphens to reach right side
		    firstDone=dummy
		fi
	    done
	}

	cat .bringerHistory | echoFrequencies | sort -nr | echoMFU
	echo "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
    }

    wmctrl -lp | echoSortLines | sort | echoAllLines
}

declare line=`(echoExtraEntries;dmenu_path) | dmenu -i -l 11` 
declare -i window
case "$line" in # todo: improve the case patterns
    "g "*)
	window=`echo "$line" | cut -d " " -f 2`
	wmctrl -ia $window
	;;
    # *bring*) # conflicts with "bringer"
    # 	window=`echo "$line" | cut -d " " -f 3`
    # 	wmctrl -iR $window
    # 	;;
    "h "*)
	cp .bringerHistory .bringerHistoryTmp
	declare command=`echo "$line" | sed 's/^h \(.*\)\(---.*\|$\)/\1/'`
	echo "$command" > .bringerHistory
	cat .bringerHistoryTmp >> .bringerHistory
	exec $command
	;;
    ?*) # non-empty string
	cp .bringerHistory .bringerHistoryTmp
	echo "$line" > .bringerHistory
	cat .bringerHistoryTmp >> .bringerHistory
	exec $line
	;;
esac
