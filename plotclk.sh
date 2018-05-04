#!/bin/sh -e
#
# Plot mfpcpu.py output
#
# By Ben G. AKA Ben/OVR
#

which gnuplot >/dev/null

if [ -z "${recdecode-}" ]; then
    recdecode=$(dirname "$0")/recdecode.py
fi

if [ -z "$1" ] || [ ! -r "$1" ]; then
    set -- -h
fi

case "$1" in
    -h | --help | --usage)
	cat <<EOF
Usage: plotclk.sh file.rec [windows-seconds]

  Create file.png using recdecode.py  and gnuplot
EOF
	exit 1
esac
	
inp=$(mktemp)
trap "rm -- $inp" EXIT 

if [[ "$1" = *.* ]]; then
    pic="${1%.*}".png
    title="$(basename "${1%.*}")"
else
    pic="${1}.png"
    title="$(basename "${1}")"
fi

set -o pipefail

${recdecode} "$@" >$inp

cat <<EOF | gnuplot

    reset

    set term pngcairo \
    	enhanced \
    	color \
     	font "arial,9" \
    	background "#081014" \
    	size 1024, 512

    set output "$pic"

    set style line 48 lt 1 lc rgb "#FF0000" lw 1
    set style line 49 lt 1 lc rgb "grey"   lw 1 dashtype 3
    set style line 50 lt 1 lc rgb "green"  lw 1
    set style line 51 lt 1 lc rgb "white"  lw 1
    set style line 52 lt 1 lc rgb "yellow" lw 1

    set title "${title} - CPU clock estimation using MFP:VBL ratio" \
    	tc "white" font "arial,12" enhanced

    set xlabel 'Elapsed time (hours)'   textcolor "white"
    set ylabel 'Estimated CPU clocks (hz)' textcolor "white"

    set key default right bottom autotitle nobox tc "white"

    set xtics format "%2.0f"
    set ytics format "%9.1f"
    set grid ls 49
    set border ls 51
    set autoscale

    # set xrange [23.894:24.01]
    # set yrange [8010860:8010940]
    # set yrange [8010630:8010720]
    #set yrange [8021247.5:8021248.5]

    set object 1 rectangle from graph 0,0 to graph 1,1 behind fc rgb "#203050" 

    plot \
    	 "$inp" using 1:2 with lines smooth bezier ls 50 t "Average" ,\
    	 "$inp" using 1:3 with lines smooth bezier ls 52 t "Instant"

    exit

EOF
