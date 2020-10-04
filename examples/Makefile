all: examples.d64 examples.d81

examples.d64: test_unp.prg test_exom.prg test_exom3.prg test_pucr.prg test_1bit.prg datafile.exo datafile.exo3 datafile.puc examples.seq
	makedisk examples.d64 examples.seq "LOADER EXAMPLES   LE 2A" 12

examples.d81: examples.d64 examd81.seq
	c1541 < examd81.seq

test_exom.prg: test_exom.s ../loader.s ../cfg_exom.s
	dasm test_exom.s -otest_exom.prg -v3 -p3

test_exom3.prg: test_exom3.s ../loader.s ../cfg_exom3.s
	dasm test_exom3.s -otest_exom3.prg -v3 -p3

test_pucr.prg: test_pucr.s ../loader.s ../cfg_pucr.s
	dasm test_pucr.s -otest_pucr.prg -v3 -p3

test_unp.prg: test_unp.s ../loader.s ../cfg_unp.s
	dasm test_unp.s -otest_unp.prg -v3 -p3

test_1bit.prg: test_1bit.s ../loader.s ../cfg_1bit.s
	dasm test_1bit.s -otest_1bit.prg -v3 -p3

datafile.exo: datafile
	exomizer2 level -odatafile.exo datafile

datafile.exo3: datafile
	exomizer3 level -odatafile.exo3 datafile

datafile.puc: datafile
	pucrunch -c0 datafile datafile.puc
