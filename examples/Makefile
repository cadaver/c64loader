all: examples.d64 examples.d81

examples.d64: test_unp.prg
	makedisk examples.d64 examples.seq "LOADER EXAMPLES   LE 2A" 10

examples.d81: examples.d64 examd81.seq
	c1541 < examd81.seq

test_unp.prg: test_unp.s ../loader.s
	dasm test_unp.s -otest_unp.prg -v3 -p3

datafile.exo3: datafile
	exomizer3 raw -odatafile.exo3 datafile
