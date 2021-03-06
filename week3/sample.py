from oracle import *
import sys
from random import randrange

if len(sys.argv) < 2:
    print "Usage: python sample.py <filename>"
    sys.exit(-1)

f = open(sys.argv[1])
data = f.read()
f.close()

ctext = [(int(data[i:i+2],16)) for i in range(0, len(data), 2)]
C = [ctext[:16], ctext[16:32], ctext[-16:]]
#print C
P = [[0] * 16, [0] * 16, [0] * 16]
IV = [[0] * 16, [0] * 16, [0] * 16]

Oracle_Connect()
for bi in range(2):
    b = 2 - bi
    for k in range(16):
        if b == 2:
            C1 = [C[0][:], C[1][:], C[2][:]]
        if b == 1:
            C1 = [C[0][:], C[1][:]]
        pos = 15 - k
        for i in range(pos):
            C1[b-1][i] = randrange(256)
        for i in range(pos + 1, 16):
            C1[b-1][i] = (k + 1) ^ IV[b][i]
        ii = -1
        for i in range(256):
            C1[b-1][pos] = i
            if b == 2:
                rc = Oracle_Send(C1[0][:] + C1[1][:] + C1[2][:], 3)
            if b == 1:
                rc = Oracle_Send(C1[0][:] + C1[1][:], 2)
            #print rc
            if rc == 1:
                ii = i
                break
        IV[b][pos] = ii ^ (k + 1)
        P[b][pos] = C[b-1][pos] ^ IV[b][pos]
        #print ii
        #print P[b][pos]
Oracle_Disconnect()
print P[1]
print P[2]
print map(lambda x: str(unichr(x)), P[1])
print map(lambda x: str(unichr(x)), P[2])
#print map(lambda x: str(unichr(x)), P[2])
#Oracle_Connect()

#rc = Oracle_Send(ctext, 3)
#print "Oracle returned: %d" % rc

#Oracle_Disconnect()

