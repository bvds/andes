# -static statically linked just so I don't have to figure out
# how to make ld find the directory  (or set DL_LIBRARY_PATH)
regress: regress.f
	gfortran -Wall -o regress regress.f -static -lacml -L/home/bvds/lib/gfortran64/lib
