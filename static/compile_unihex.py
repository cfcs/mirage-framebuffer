#!/usr/bin/env python2
from sys import argv
print 'Usage: %s unihex-file single-chars.bitmap wide-chars.bitmap' % argv[0]

small = open(argv[2], 'ab')
wide = open(argv[3], 'ab')

for f in file(argv[1]):
  a , b = f.strip().split(":")
  if 64==len(b):
    o = wide
  elif 32 == len(b):
    o = small
  else:
    print '\x1b[31;1mSKIPPING',len(a),len(b)
    continue
  o.write(a.decode('hex'))
  o.write(b.decode('hex'))

small.close()
wide.close()

