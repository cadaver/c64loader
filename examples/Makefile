all: examples.d64 examples.d81

examples.d64: test_unp.prg test_exo3.prg test_spr.prg datafile datafile.exo3
	makedisk examples.d64 examples.seq "LOADER EXAMPLES   LE 2A" 10

examples.d81: examples.d64 examd81.seq
	c1541 < examd81.seq

test_unp.prg: test_unp.s ../config.s ../loader.s ../loaderinit.s ../loadunpacked.s
	dasm test_unp.s -otest_unp.prg -v3 -p3

test_exo3.prg: test_exo3.s ../config.s ../loader.s ../loaderinit.s ../loadexomizer3.s
	dasm test_exo3.s -otest_exo3.prg -v3 -p3

test_spr.prg: test_spr.s ../config.s ../loader.s ../loaderinit.s ../loadunpacked.s
	dasm test_spr.s -otest_spr.prg -v3 -p3

datafile.exo3: datafile
	prg2bin datafile datafile.bin
	exomizer3 raw -T4 -M256 -c -odatafile.exo3 datafile.bin

