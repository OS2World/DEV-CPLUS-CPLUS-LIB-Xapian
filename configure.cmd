set CFLAGS=-g -Zomf -O3 -march=pentium -mcpu=pentium4
set CXXFLAGS=-g -Zomf -O3 -march=pentium -mcpu=pentium4
set LDFLAGS=-s -Zsym -Zmap -Zbin-files -Zhigh-mem -Zomf -Zexe -Zargs-wild -Zargs-resp
set LN_CP_F=cp.exe
set RANLIb=echo
set AR=emxomfar -p256
set LIBCLAMAV_LIBS=
sh -c "./configure --prefix=/usr/local/xapian --disable-backend-quartz --disable-backend-remote"
