noinst_HEADERS +=\
	common/alltermslist.h\
	common/autoptr.h\
	common/database.h\
	common/document.h\
	common/documentterm.h\
	common/emptypostlist.h\
	common/expand.h\
	common/expandweight.h\
	common/gnu_getopt.h\
	common/inmemory_positionlist.h\
	common/leafpostlist.h\
	common/msvc_posix_wrapper.h\
	common/multialltermslist.h\
	common/multimatch.h\
	common/networkstats.h\
	common/noreturn.h\
	common/omassert.h\
	common/omdebug.h\
	common/omenquireinternal.h\
	common/omqueryinternal.h\
	common/omstringstream.h\
	common/omtime.h\
	common/ortermlist.h\
	common/output.h\
	common/positionlist.h\
	common/postlist.h\
	common/progclient.h\
	common/remoteconnection.h\
	common/remote-database.h\
	common/remoteprotocol.h\
	common/remoteserver.h\
	common/rset.h\
	common/safeerrno.h\
	common/safefcntl.h\
	common/safesysselect.h\
	common/safesysstat.h\
	common/safeunistd.h\
	common/safewindows.h\
	common/safewinsock2.h\
	common/serialise-double.h\
	common/serialise.h\
	common/stats.h\
	common/stringutils.h\
	common/submatch.h\
	common/tcpclient.h\
	common/tcpserver.h\
	common/termlist.h\
	common/utils.h\
	common/vectortermlist.h

EXTRA_DIST +=\
	common/dir_contents\
	common/Makefile

libxapian_la_SOURCES +=\
	common/msvc_posix_wrapper.cc\
	common/omdebug.cc\
	common/omstringstream.cc\
	common/safe.cc\
	common/serialise-double.cc\
	common/utils.cc

noinst_LTLIBRARIES += libgetopt.la

libgetopt_la_SOURCES =\
	common/getopt.cc
