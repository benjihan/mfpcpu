#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# MFP / CPU clock ratio
#
# By http://sourceforge.net/users/benjihan

from struct import unpack
from getopt import gnu_getopt as getopt
import sys
import fractions as F

def clock(mfp,vbl):
    return (vbl * 0x5bb3 << 14) / float(mfp)

def hour(mfp):
    return mfp / 2400.0 / 3600.0

def print_usage():
    print("Usage: recdecode [file.rec [window-seconds]]\n"
          "\n"
          "  Decode mfpcpu.prg .rec files")

def main(argc, argv):

    inp = sys.stdin             # default input is stdin
    sec = 60.0                  # default window is 20 seconds

    if argc > 1:
        if argv[1] in [ '-h','--help','--usage' ] or argc > 3:
            print_usage()
            return 0
        if argv[1] not in [ '-', '/dev/stdin', 'stdin:', ]:
            inp = open(argv[1],'rb')

    if argc == 3: sec = float(argv[2])

    winlen = sec / 3600.0       # in hours

    dat = []
    while True:
        b = inp.read(8)
        if not b or len(b)!= 8: break
        mfp,vbl = unpack('>II',b)
        if not vbl or not mfp or abs(clock(mfp,vbl)-8010000) > 20000:
            continue
        if dat:
            cpuhz = clock(mfp-dat[-1][0], vbl-dat[-1][1])
        else:
            cpuhz = clock(mfp, vbl)
        dat.append( (mfp,vbl,cpuhz) )
    dat.sort()
    if len(dat) < 4 : return 0

    deltas = [ ( dat[i-1][0]-dat[i][0],
                 dat[i-1][1]-dat[i][1],
                 dat[i-1][2]-dat[i][2] )
               for i in range(1,len(dat)) ]

    # for delta in  deltas:
    #     print("%f %f" % ( clock(delta[0],delta[1]), delta[2]) ) 

    avg_total_cpu = clock( dat[-1][0],dat[-1][1] )
    avg_insta_cpu = sum([d[2] for d in dat]) / float(len(dat))
    avg_delta_cpu = sum([abs(d[2]) for d in deltas]) / float(len(deltas))
    max_delta_cpu = max([abs(d[2]) for d in deltas])

    print("avg_total_cpu:",avg_total_cpu,file=sys.stderr)
    print("avg_insta_cpu:",avg_insta_cpu,file=sys.stderr)
    print("avg_delta_cpu:",avg_delta_cpu,file=sys.stderr)
    print("max_delta_cpu:",max_delta_cpu,file=sys.stderr)

    # | Machines    | Total CPU | Instant Avg | Delta Avg |
    # |-------------|-----------|-------------|-----------|
    # | Hatari-204: | 8021248.0 | 8021236.0   |  50.5     |
    # | SSSE-394    | 8022000.6 | 8022002.2   | 110.0     |
    # | 1040-stf-FR | 8010687.2 | 8010686.8   |   4.1     |
    # | 520-stf-FR  | 8021548.7 | 8021553.0   |  12.3     |
    
    res = []
    for i in range(0,len(dat)-1):
        if hour(dat[i][0]) < winlen: continue
        
        avg_cpu = clock(dat[i][0],dat[i][1])
        time = hour(dat[i][0])

        win_cpu = None
        for k in range(i-1,0,-1):
            if hour( dat[i][0]-dat[k][0] ) >= winlen:
                break
        win_cpu = clock( dat[i][0]-dat[k][0] , dat[i][1]-dat[k][1] )
        
        print('%f %f %f'%( time, avg_cpu, win_cpu))
    return 0



if __name__ == "__main__":
    try:
        # sys.args[0] = 'recdecode'
        ret = main(len(sys.argv),sys.argv)
    except Exception as e:
        print('recdecode: %s' % str(e), file=sys.stderr)
        # raise e
        ret = 128
    sys.exit(ret)
