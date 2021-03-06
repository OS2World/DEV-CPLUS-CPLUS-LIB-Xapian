/* api_nodb.cc: tests which don't use any of the backends
 *
 * Copyright 1999,2000,2001 BrightStation PLC
 * Copyright 2002 Ananova Ltd
 * Copyright 2002,2003,2004,2005,2006,2007 Olly Betts
 * Copyright 2006 Lemur Consulting Ltd
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <config.h>

#include <string>
#include <vector>
#include "autoptr.h"

#include <xapian.h>

#include "apitest.h"
#include "testsuite.h"
#include "testutils.h"

#include <list>

using namespace std;

// always succeeds
static bool test_trivial1()
{
    return true;
}

// tests that get_query_terms() returns the terms in the right order
static bool test_getqterms1()
{
    list<string> answers_list;
    answers_list.push_back("one");
    answers_list.push_back("two");
    answers_list.push_back("three");
    answers_list.push_back("four");

    Xapian::Query myquery(Xapian::Query::OP_OR,
	    Xapian::Query(Xapian::Query::OP_AND,
		    Xapian::Query("one", 1, 1),
		    Xapian::Query("three", 1, 3)),
	    Xapian::Query(Xapian::Query::OP_OR,
		    Xapian::Query("four", 1, 4),
		    Xapian::Query("two", 1, 2)));

    list<string> list1;
    {
	Xapian::TermIterator t;
	for (t = myquery.get_terms_begin(); t != myquery.get_terms_end(); ++t)
	    list1.push_back(*t);
    }
    TEST(list1 == answers_list);
    list<string> list2(myquery.get_terms_begin(), myquery.get_terms_end());
    TEST(list2 == answers_list);
    return true;
}

// tests that get_query_terms() doesn't SEGV on an empty query
// (regression test for bug in 0.9.0)
static bool test_getqterms2()
{
    Xapian::Query empty_query;
    TEST_EQUAL(empty_query.get_terms_begin(), empty_query.get_terms_end());
    return true;
}

// tests that empty queries work correctly
static bool test_emptyquery2()
{
    // test that Query::empty() is true for an empty query.
    TEST(Xapian::Query().empty());
    // test that an empty query has length 0
    TEST(Xapian::Query().get_length() == 0);
    vector<Xapian::Query> v;
    TEST(Xapian::Query(Xapian::Query::OP_OR, v.begin(), v.end()).empty());
    TEST(Xapian::Query(Xapian::Query::OP_OR, v.begin(), v.end()).get_length() == 0);
    return true;
}

/// Regression test for behaviour for an empty query with AND_NOT.
static bool test_emptyquery3()
{
    static const Xapian::Query::op ops[] = {
	Xapian::Query::OP_AND,
	Xapian::Query::OP_OR,
	Xapian::Query::OP_XOR,
	Xapian::Query::OP_AND_MAYBE,
	Xapian::Query::OP_AND_NOT
    };

    for (size_t i = 0; i < sizeof(ops) / sizeof(ops[0]); ++i) {
	tout << "Testing op #" << i << endl;
	Xapian::Query empty;
	Xapian::Query q("test");
	Xapian::Query qcombine(ops[i], empty, q);
	tout << qcombine.get_description() << endl;
	Xapian::Query qcombine2(ops[i], q, empty);
	tout << qcombine2.get_description() << endl;
    }

    return true;
}

// tests that query lengths are calculated correctly
static bool test_querylen1()
{
    // test that a simple query has the right length
    Xapian::Query myquery;
    myquery = Xapian::Query(Xapian::Query::OP_OR,
		      Xapian::Query("foo"),
		      Xapian::Query("bar"));
    myquery = Xapian::Query(Xapian::Query::OP_AND,
		      myquery,
		      Xapian::Query(Xapian::Query::OP_OR,
			      Xapian::Query("wibble"),
			      Xapian::Query("spoon")));

    TEST_EQUAL(myquery.get_length(), 4);
    TEST(!myquery.empty());
    return true;
}

// tests that query lengths are calculated correctly
static bool test_querylen2()
{
    // test with an even bigger and strange query
    string terms[3] = {
	"foo",
	"bar",
	"baz"
    };
    Xapian::Query queries[3] = {
	Xapian::Query("wibble"),
	Xapian::Query("wobble"),
	Xapian::Query(Xapian::Query::OP_OR, string("jelly"), string("belly"))
    };

    Xapian::Query myquery;
    vector<string> v1(terms, terms + 3);
    vector<Xapian::Query> v2(queries, queries + 3);
    vector<Xapian::Query *> v3;
    AutoPtr<Xapian::Query> dynquery1(new Xapian::Query(Xapian::Query::OP_AND,
					   string("ball"),
					   string("club")));
    AutoPtr<Xapian::Query> dynquery2(new Xapian::Query("ring"));
    v3.push_back(dynquery1.get());
    v3.push_back(dynquery2.get());

    Xapian::Query myq1 = Xapian::Query(Xapian::Query::OP_AND, v1.begin(), v1.end());
    tout << "myq1=" << myq1 << "\n";
    TEST_EQUAL(myq1.get_length(), 3);

    Xapian::Query myq2_1 = Xapian::Query(Xapian::Query::OP_OR, v2.begin(), v2.end());
    tout << "myq2_1=" << myq2_1 << "\n";
    TEST_EQUAL(myq2_1.get_length(), 4);

    Xapian::Query myq2_2 = Xapian::Query(Xapian::Query::OP_AND, v3.begin(), v3.end());
    tout << "myq2_2=" << myq2_2 << "\n";
    TEST_EQUAL(myq2_2.get_length(), 3);

    Xapian::Query myq2 = Xapian::Query(Xapian::Query::OP_OR, myq2_1, myq2_2);
    tout << "myq2=" << myq2 << "\n";
    TEST_EQUAL(myq2.get_length(), 7);

    myquery = Xapian::Query(Xapian::Query::OP_OR, myq1, myq2);
    tout << "myquery=" << myquery << "\n";
    TEST_EQUAL(myquery.get_length(), 10);

    return true;
}

// tests that queries validate correctly
static bool test_queryvalid1()
{
    vector<Xapian::Query> v1;
    // Need two arguments
    TEST_EXCEPTION(Xapian::InvalidArgumentError,
		   Xapian::Query(Xapian::Query::OP_AND_NOT, v1.begin(), v1.end()));
    tout << "ANDNOT () checked" << endl;
    v1.push_back(Xapian::Query("bad"));
    TEST_EXCEPTION(Xapian::InvalidArgumentError,
		   Xapian::Query(Xapian::Query::OP_AND_NOT, v1.begin(), v1.end()));
    tout << "ANDNOT (\"bad\") checked" << endl;
    v1.clear();
    v1.push_back(Xapian::Query());
    TEST_EXCEPTION(Xapian::InvalidArgumentError,
		   Xapian::Query(Xapian::Query::OP_AND_NOT, v1.begin(), v1.end()));
    tout << "ANDNOT (Xapian::Query()) checked" << endl;
    Xapian::Query q2(Xapian::Query::OP_XOR, Xapian::Query("foo"), Xapian::Query("bar"));
    tout << "XOR (\"foo\", \"bar\") checked" << endl;
    return true;
}

// tests that collapsing of queries includes subqueries
static bool test_subqcollapse1()
{
    Xapian::Query queries1[3] = {
	Xapian::Query("wibble"),
	Xapian::Query("wobble"),
	Xapian::Query(Xapian::Query::OP_OR, string("jelly"), string("belly"))
    };

    Xapian::Query queries2[3] = {
	Xapian::Query(Xapian::Query::OP_AND, string("jelly"), string("belly")),
	Xapian::Query("wibble"),
	Xapian::Query("wobble")
    };

    vector<Xapian::Query> vec1(queries1, queries1 + 3);
    Xapian::Query myquery1(Xapian::Query::OP_OR, vec1.begin(), vec1.end());
    TEST_EQUAL(myquery1.get_description(),
	       "Xapian::Query((wibble OR wobble OR jelly OR belly))");

    vector<Xapian::Query> vec2(queries2, queries2 + 3);
    Xapian::Query myquery2(Xapian::Query::OP_AND, vec2.begin(), vec2.end());
    TEST_EQUAL(myquery2.get_description(),
	       "Xapian::Query((jelly AND belly AND wibble AND wobble))");

    return true;
}

// test behaviour when creating a query from an empty vector
static bool test_emptyquerypart1()
{
    vector<string> emptyterms;
    Xapian::Query query(Xapian::Query::OP_OR, emptyterms.begin(), emptyterms.end());
    TEST(Xapian::Query(Xapian::Query::OP_AND, query, Xapian::Query("x")).empty());
    TEST(Xapian::Query(Xapian::Query::OP_AND, query, Xapian::Query("x")).get_length() == 0);
    TEST(!Xapian::Query(Xapian::Query::OP_OR, query, Xapian::Query("x")).empty());
    TEST(Xapian::Query(Xapian::Query::OP_OR, query, Xapian::Query("x")).get_length() == 1);
    return true;
}

static bool test_singlesubq1()
{
    vector<string> oneterm;
    oneterm.push_back("solo");
    Xapian::Query q_eliteset(Xapian::Query::OP_ELITE_SET, oneterm.begin(), oneterm.end(), 1);
    Xapian::Query q_near(Xapian::Query::OP_NEAR, oneterm.begin(), oneterm.end(), 1);
    Xapian::Query q_phrase(Xapian::Query::OP_PHRASE, oneterm.begin(), oneterm.end(), 1);
    return true;
}

static bool test_stemlangs1()
{
    unsigned lang_count = 0;
    string langs = Xapian::Stem::get_available_languages();
    tout << "available languages '" << langs << "'" << endl;
    TEST(!langs.empty());

    string::size_type i = 0;
    while (true) {
	string::size_type spc = langs.find(' ', i);
	// The only spaces in langs should be a single one between each pair
	// of language names.
	TEST_NOT_EQUAL(i, spc);

	++lang_count;

	// Try making a stemmer for this language.  We should be able to create
	// it without an exception being thrown.
	string language = langs.substr(i, spc - i);
	tout << "checking language code '" << language << "' works" << endl;
	Xapian::Stem stemmer(language);

	if (spc == string::npos) break;
	i = spc + 1;
    }

    // Check that we actually had some languages to test.
    TEST(lang_count > 0);

    // Check that we get an exception for a bogus language name.
    TEST_EXCEPTION(Xapian::InvalidArgumentError, Xapian::Stem stemmer("bogus"));

    return true;
}

// Some simple tests of the built in weighting schemes.
static bool test_weight1()
{
    Xapian::Weight * wt;

    Xapian::BoolWeight boolweight;
    TEST_EQUAL(boolweight.name(), "Bool");
    wt = Xapian::BoolWeight().unserialise(boolweight.serialise());
    TEST_EQUAL(boolweight.serialise(), wt->serialise());
    delete wt;

    Xapian::TradWeight tradweight_dflt;
    Xapian::TradWeight tradweight(1.0);
    TEST_EQUAL(tradweight.name(), "Trad");
    TEST_EQUAL(tradweight_dflt.serialise(), tradweight.serialise());
    wt = Xapian::TradWeight().unserialise(tradweight.serialise());
    TEST_EQUAL(tradweight.serialise(), wt->serialise());
    delete wt;

    Xapian::TradWeight tradweight2(2.0);
    TEST_NOT_EQUAL(tradweight.serialise(), tradweight2.serialise());

    Xapian::BM25Weight bm25weight_dflt;
    Xapian::BM25Weight bm25weight(1, 0, 1, 0.5, 0.5);
    TEST_EQUAL(bm25weight.name(), "BM25");
    TEST_EQUAL(bm25weight_dflt.serialise(), bm25weight.serialise());
    wt = Xapian::BM25Weight().unserialise(bm25weight.serialise());
    TEST_EQUAL(bm25weight.serialise(), wt->serialise());
    delete wt;

    Xapian::BM25Weight bm25weight2(1, 0.5, 1, 0.5, 0.5);
    TEST_NOT_EQUAL(bm25weight.serialise(), bm25weight2.serialise());

    return true;
}

// Regression test.
static bool test_nosuchdb1()
{
    // This is a "nodb" test because it doesn't test a particular backend.
    TEST_EXCEPTION(Xapian::DatabaseOpeningError,
		   Xapian::Database db("NOsuChdaTabASe"));
    return true;
}

// Regression test for add_value on an existing value (bug#82).
static bool test_addvalue1()
{
    Xapian::Document doc;
    doc.add_value(1, "original");
    doc.add_value(1, "replacement");
    TEST_EQUAL(doc.get_value(1), "replacement");

    return true;
}

// tests that the collapsing on termpos optimisation gives correct query length
static bool test_poscollapse2()
{
    Xapian::Query q(Xapian::Query::OP_OR, Xapian::Query("this", 1, 1), Xapian::Query("this", 1, 1));
    TEST_EQUAL(q.get_length(), 2);
    return true;
}

// regression test of querying an uninitialised database: should report an
// error; used to segfault with 1.0.0.
static bool test_uninitdb1()
{
    Xapian::Database db;
    TEST_EXCEPTION(Xapian::InvalidArgumentError,
		   Xapian::Enquire enq(db));
    return true;
}

// #######################################################################
// # End of test cases: now we list the tests to run.

test_desc nodb_tests[] = {
    {"trivial1",	   test_trivial1},
    {"getqterms1",	   test_getqterms1},
    {"getqterms2",	   test_getqterms2},
    {"emptyquery2",	   test_emptyquery2},
    {"emptyquery3",	   test_emptyquery3},
    {"querylen1",	   test_querylen1},
    {"querylen2",	   test_querylen2},
    {"queryvalid1",	   test_queryvalid1},
    {"subqcollapse1",	   test_subqcollapse1},
    {"emptyquerypart1",    test_emptyquerypart1},
    {"singlesubq1",	   test_singlesubq1},
    {"stemlangs1",	   test_stemlangs1},
    {"weight1",		   test_weight1},
    {"nosuchdb1",	   test_nosuchdb1},
    {"addvalue1",	   test_addvalue1},
    {"poscollapse2",	   test_poscollapse2},
    {"uninitdb1",	   test_uninitdb1},
    {0, 0}
};
