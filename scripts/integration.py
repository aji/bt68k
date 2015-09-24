#!/usr/bin/env python

import sys, os
import subprocess
import tempfile

DELIM_TEST   = '-- test: '
DELIM_CHECK  = '--'

DELIM_OUT    = '-- tests go here --'

ARCH = 'm68k-unknown-elf-'

class Test(object):
    def __init__(self, lines):
        i = 0
        while i < len(lines):
            i += 1
            if lines[i-1].startswith(DELIM_TEST):
                self.name = lines[i-1][len(DELIM_TEST):]
                break
        asm = []
        while i < len(lines):
            if lines[i].startswith(DELIM_CHECK):
                break
            else:
                asm.append(lines[i])
                i += 1
        asm.append('1:\tjmp\t1b')

        self.name   = self.name if self.name else '(unnamed)'
        self.asm    = '\n'.join(asm) + '\n'
        self.check  = '\n'.join(lines[i+1:]) + '\n'
        self.code   = None
        self.rust   = None

    def build(self):
        #
        # do assembly

        out_object = '/tmp/bt68k-integration.o'
        out_binary = '/tmp/bt68k-integration.bin'

        print('Building {}'.format(self.name))

        p_as = subprocess.Popen([
                ARCH + 'as',
                '-mcpu=68000',
                '-o', out_object,
                ],
            stdin=subprocess.PIPE,
            )
        p_as.stdin.write(self.asm.encode())
        p_as.stdin.close()
        p_as.wait()

        p_ld = subprocess.Popen([
                ARCH + 'ld',
                '--entry', '0',
                '--oformat', 'binary',
                '--output', out_binary,
                '--format', 'elf32-m68k',
                out_object,
                ],
            )
        p_ld.wait()

        with open(out_binary, 'rb') as f:
            self.code = f.read()

        os.unlink(out_object)
        os.unlink(out_binary)

        #
        # do rust

        r = self.check
        for i in range(8):
            r = r.replace('%%d%i'%i, '(ee.cpu().data[%i])'%i)
            r = r.replace('%%a%i'%i, '(ee.cpu().addr[%i])'%i)
        r = r.replace('%sp',  '(ee.cpu().addr[7])')
        r = r.replace('%ssp', '(ee.cpu().ssp)')
        r = r.replace('%pc',  '(ee.cpu().pc)')
        r = r.replace('%sr',  '(ee.cpu().status)')
        self.rust = r

    def write(self, f):
        name = '"{}"'.format(self.name.replace('"', '\\"'))
        words = [((a<<8)|b) for a,b in zip(self.code[0::2], self.code[1::2])]
        code = '[{}]'.format(','.join('0x{:04x}'.format(w) for w in words))
        test = 'test!({},{},|ee|{{\n{}}});\n'.format(name, code, self.rust)
        f.write(test)

def scan_tests(lines):
    i = 0
    while i < len(lines):
        if lines[i].startswith(DELIM_TEST):
            tbeg = i
            tend = len(lines)
            while i < len(lines) - 1:
                i += 1
                if lines[i].startswith(DELIM_TEST):
                    tend = i
                    break
            yield Test(lines[tbeg:tend])
        else:
            i += 1

if __name__ == '__main__':
    with open(sys.argv[2], 'r') as f:
        before = ''
        after = ''
        for line in f:
            if line.startswith(DELIM_OUT):
                break
            before += line
        for line in f:
            after += line

    with open(sys.argv[1], 'r') as f:
        lines = [x.rstrip() for x in f]

    with open(sys.argv[3], 'w') as f:
        f.write(before)
        for t in scan_tests(lines):
            t.build()
            t.write(f)
        f.write(after)
