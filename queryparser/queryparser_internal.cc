/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is included which follows the "include" declaration
** in the input file. */
#line 1 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"

/* queryparser.lemony: build a Xapian::Query object from a user query string.
 *
 * Copyright (C) 2004,2005,2006,2007 Olly Betts
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

#include "omassert.h"
#include "queryparser_internal.h"
#include <xapian/unicode.h>
#include "utils.h"

// Include the list of token values lemon generates.
#include "queryparser_token.h"

#include <algorithm>
#include <list>
#include <string>

using namespace std;

using namespace Xapian;

inline bool
U_isupper(unsigned ch) {
    return (ch < 128 && C_isupper((unsigned char)ch));
}

inline bool
U_isdigit(unsigned ch) {
    return (ch < 128 && C_isdigit((unsigned char)ch));
}

inline bool
U_isalpha(unsigned ch) {
    return (ch < 128 && C_isalpha((unsigned char)ch));
}

using Xapian::Unicode::is_whitespace;

inline bool
is_not_whitespace(unsigned ch) {
    return !is_whitespace(ch);
}

using Xapian::Unicode::is_wordchar;

inline bool
is_not_wordchar(unsigned ch) {
    return !is_wordchar(ch);
}

inline bool
is_digit(unsigned ch) {
    return (Unicode::get_category(ch) == Unicode::DECIMAL_DIGIT_NUMBER);
}

// FIXME: we used to keep trailing "-" (e.g. Cl-) but it's of dubious utility
// and there's the risk of hyphens getting stuck onto the end of terms...
inline bool
is_suffix(unsigned ch) {
    return ch == '+' || ch == '#';
}

inline bool
prefix_needs_colon(const string & prefix, unsigned ch)
{
    if (!U_isupper(ch)) return false;
    string::size_type len = prefix.length();
    return (len > 1 && prefix[len - 1] != ':');
}

using Unicode::is_currency;

/// A structure identifying a group of filter terms.
struct filter_group_id {
    /** The prefix of the filter terms.
     *  This is used for boolean filter terms.
     */
    string prefix;

    /** The value number of the filter terms.
     *  This is used for value range terms.
     */
    Xapian::valueno valno;

    /// Make a new filter_group_id for boolean filter terms.
    explicit filter_group_id(const string & prefix_)
	: prefix(prefix_), valno(Xapian::BAD_VALUENO) {}

    /// Make a new filter_group_id for value range terms.
    explicit filter_group_id(Xapian::valueno valno_)
	: prefix(), valno(valno_) {}

    /// Compare to another filter_group_id.
    bool operator<(const filter_group_id & other) const {
	if (prefix != other.prefix) {
	    return prefix < other.prefix;
	}
	return valno < other.valno;
    }
};

/** Class used to pass information about a token from lexer to parser.
 *
 *  Generally an instance of this class carries term information, but it can be
 *  used for the start or end of a value range, with some operators (e.g. the
 *  distance in NEAR/3 or ADJ/3, etc).
 */
class Term {
    State * state;

  public:
    string name;
    string prefix;
    string unstemmed;
    bool stem;
    termpos pos;

    Term(const string &name_, termpos pos_) : name(name_), stem(false), pos(pos_) { }
    Term(const string &name_) : name(name_), stem(false), pos(0) { }
    Term(const string &name_, const string &prefix_)
	: name(name_), prefix(prefix_), stem(false), pos(0) { }
    Term(termpos pos_) : stem(false), pos(pos_) { }
    Term(State * state_, const string &name_, const string &prefix_,
	 const string &unstemmed_, bool stem_ = false, termpos pos_ = 0)
	: state(state_), name(name_), prefix(prefix_), unstemmed(unstemmed_),
	  stem(stem_), pos(pos_) { }

    std::string make_term() const;

    void dont_stem() { stem = false; }

    termpos get_termpos() const { return pos; }

    filter_group_id get_filter_group_id() const { return filter_group_id(prefix); }

    Query * as_query() const { return new Query(make_term(), 1, pos); }

    Query * as_wildcarded_query(State * state) const;

    Query * as_partial_query(State * state_) const;

    Query as_query_object() const { return Query(make_term(), 1, pos); }

    Query * as_query_with_synonyms() const;

    Query * as_query_with_auto_synonyms() const;
};

/// Parser State shared between the lexer and the parser.
class State {
    QueryParser::Internal * qpi;

  public:
    Query query;
    const char * error;
    unsigned flags;

    State(QueryParser::Internal * qpi_, unsigned flags_)
	: qpi(qpi_), error(NULL), flags(flags_) { }

    string stem_term(const string &term) {
	return qpi->stemmer(term);
    }

    void add_to_stoplist(const Term * term) {
	qpi->stoplist.push_back(term->name);
    }

    void add_to_unstem(const string & term, const string & unstemmed) {
	qpi->unstem.insert(make_pair(term, unstemmed));
    }

    valueno value_range(Query & q, Term *a, Term *b) {
	string start = a->name;
	string end = b->name;
	Xapian::valueno valno = Xapian::BAD_VALUENO;
	list<ValueRangeProcessor *>::const_iterator i;
	for (i = qpi->valrangeprocs.begin(); i != qpi->valrangeprocs.end(); ++i) {
	    valno = (**i)(start, end);
	    if (valno != Xapian::BAD_VALUENO) {
		delete a;
		delete b;
		q = Query(Query::OP_VALUE_RANGE, valno, start, end);
		return valno;
	    }
	}
	// FIXME: Do we want to report an error for this?  If not we need
	// to perform the above check in the tokeniser and if none of the
	// ValueRangeProcessor classes like the range, we rollback to
	// parsing the query without treating this as a range.  Needs
	// more thought and probably a look at queries users actually
	// enter.
	error = "Unknown range operation";
	return valno;
    }

    Query::op default_op() const { return qpi->default_op; }

    bool is_stopword(const Term *term) const {
	return qpi->stopper && (*qpi->stopper)(term->name);
    }

    Database get_database() const {
	return qpi->db;
    }
};

string
Term::make_term() const
{
    string term;
    if (stem) term += 'Z';
    if (!prefix.empty()) {
	term += prefix;
	if (prefix_needs_colon(prefix, name[0])) term += ':';
    }
    if (stem) {
	term += state->stem_term(name);
    } else {
	term += name;
    }

    if (!unstemmed.empty())
	state->add_to_unstem(term, unstemmed);
    return term;
}

Query *
Term::as_query_with_synonyms() const
{
    Query q = as_query_object();

    // Handle single-word synonyms.  First try the unstemmed term:
    string term;
    if (!prefix.empty()) {
	term += prefix;
	if (prefix_needs_colon(prefix, name[0])) term += ':';
    }
    term += name;

    Xapian::Database db = state->get_database();
    Xapian::TermIterator syn = db.synonyms_begin(term);
    Xapian::TermIterator end = db.synonyms_end(term);
    if (syn == end && stem) {
	// If that has no synonyms, try the stemmed form:
	term.resize(0);
	term += 'Z';
	if (!prefix.empty()) {
	    term += prefix;
	    if (prefix_needs_colon(prefix, name[0])) term += ':';
	}
	term += state->stem_term(name);
	syn = db.synonyms_begin(term);
	end = db.synonyms_end(term);
    }
    while (syn != end) {
	q = Query(Query::OP_OR, q, Query(*syn, 1, pos));
	++syn;
    }
    return new Query(q);
}

Query *
Term::as_query_with_auto_synonyms() const
{
    if ((state->flags & QueryParser::FLAG_AUTO_SYNONYMS) == 0)
	return as_query();
    return as_query_with_synonyms();
}

static void
add_to_query(Query *& q, Query::op op, Query * term)
{
    Assert(term);
    if (q) {
	*q = Query(op, *q, *term);
	delete term;
    } else {
	q = term;
    }
}

static void
add_to_query(Query *& q, Query::op op, const Query & term)
{
    if (q) {
	*q = Query(op, *q, term);
    } else {
	q = new Query(term);
    }
}

Query *
Term::as_wildcarded_query(State * state_) const
{
    Database db = state_->get_database();
    string root = prefix;
    root += name;
    Query * q = new Query;
    TermIterator t = db.allterms_begin(root);
    while (t != db.allterms_end(root)) {
	add_to_query(q, Query::OP_OR, Query(*t, 1, pos));
	++t;
    }
    delete this;
    return q;
}

Query *
Term::as_partial_query(State * state_) const
{
    Database db = state_->get_database();
    string root = prefix;
    root += name;
    Query * q = new Query;
    TermIterator t = db.allterms_begin(root);
    while (t != db.allterms_end(root)) {
	add_to_query(q, Query::OP_OR, Query(*t, 1, pos));
	++t;
    }
    // Add the term, as it would normally be handled, as an alternative.
    add_to_query(q, Query::OP_OR, as_query());
    delete this;
    return q;
}

inline bool
is_phrase_generator(unsigned ch)
{
    // These characters generate a phrase search.
    // Ordered mostly by frequency of calls to this function done when
    // running queryparsertest.
    return (ch && ch < 128 && strchr(".-/:\\@", ch) != NULL);
}

inline bool
is_stem_preventer(unsigned ch)
{
    return (ch && ch < 128 && strchr("(/\\@<>=*[{\"", ch) != NULL);
}

inline bool
should_stem(const std::string & term)
{
    const unsigned int SHOULD_STEM_MASK =
	(1 << Unicode::LOWERCASE_LETTER) |
	(1 << Unicode::TITLECASE_LETTER) |
	(1 << Unicode::MODIFIER_LETTER) |
	(1 << Unicode::OTHER_LETTER);
    Utf8Iterator u(term);
    return ((SHOULD_STEM_MASK >> Unicode::get_category(*u)) & 1);
}

inline unsigned check_infix(unsigned ch) {
    if (ch == '\'' || ch == '&' || ch == 0xb7 || ch == 0x5f4 || ch == 0x2027) {
	// Unicode includes all these except '&' in it's word boundary rules,
	// as well as 0x2019 (which we handle below) and ':' (for Swedish
	// apparently, but we ignore this for now as it's problematic in
	// real world cases).
	return ch;
    }
    // 0x2019 is Unicode apostrophe and single closing quote.
    // 0x201b is Unicode single opening quote with the tail rising.
    if (ch == 0x2019 || ch == 0x201b) return '\'';
    return 0;
}

inline unsigned check_infix_digit(unsigned ch) {
    // This list of characters comes from Unicode's word identifying algorithm.
    switch (ch) {
	case ',':
	case '.':
	case ';':
	case 0x037e: // GREEK QUESTION MARK
	case 0x0589: // ARMENIAN FULL STOP
	case 0x060D: // ARABIC DATE SEPARATOR
	case 0x07F8: // NKO COMMA
	case 0x2044: // FRACTION SLASH
	case 0xFE10: // PRESENTATION FORM FOR VERTICAL COMMA
	case 0xFE13: // PRESENTATION FORM FOR VERTICAL COLON
	case 0xFE14: // PRESENTATION FORM FOR VERTICAL SEMICOLON
	    return ch;
    }
    return 0;
}

struct yyParser;

// Prototype the functions lemon generates.
static yyParser *ParseAlloc();
static void ParseFree(yyParser *);
static void Parse(yyParser *, int, Term *, State *);

string
QueryParser::Internal::parse_term(Utf8Iterator &it, const Utf8Iterator &end,
				  bool &was_acronym)
{
    string term;
    // Look for initials separated by '.' (e.g. P.T.O., U.N.C.L.E).
    // Don't worry if there's a trailing '.' or not.
    if (U_isupper(*it)) {
	string t;
	Utf8Iterator p = it;
	do {
	    Unicode::append_utf8(t, *p++);
	} while (p != end && *p == '.' && ++p != end && U_isupper(*p));
	// One letter does not make an acronym!  If we handled a single
	// uppercase letter here, we wouldn't catch M&S below.
	if (t.length() > 1) {
	    // Check there's not a (lower case) letter or digit
	    // immediately after it.
	    // FIXME: should I.B.M..P.T.O be a range search?
	    if (p == end || !is_wordchar(*p)) {
		it = p;
		swap(term, t);
	    }
	}
    }
    was_acronym = !term.empty();

    if (term.empty()) {
	unsigned prevch = *it;
	Unicode::append_utf8(term, prevch);
	while (++it != end) {
	    unsigned ch = *it;
	    if (!is_wordchar(ch)) {
		// Treat a single embedded '&' or "'" or similar as a word
		// character (e.g. AT&T, Fred's).  Also, normalise
		// apostrophes to ASCII apostrophe.
		Utf8Iterator p = it;
		++p;
		if (p == end || !is_wordchar(*p)) break;
		unsigned nextch = *p;
		if (is_digit(prevch) &&
		    is_digit(nextch)) {
		    ch = check_infix_digit(ch);
		} else {
		    ch = check_infix(ch);
		}
		if (!ch) break;
	    }
	    Unicode::append_utf8(term, ch);
	    prevch = ch;
	}
	if (it != end && is_suffix(*it)) {
	    string suff_term = term;
	    Utf8Iterator p = it;
	    // Keep trailing + (e.g. C++, Na+) or # (e.g. C#).
	    do {
		if (suff_term.size() - term.size() == 3) {
		    suff_term.resize(0);
		    break;
		}
		suff_term += *p;
	    } while (is_suffix(*++p));
	    if (!suff_term.empty() && (p == end || !is_wordchar(*p))) {
		// If the suffixed term doesn't exist, check that the
		// non-suffixed term does.  This also takes care of
		// the case when QueryParser::set_database() hasn't
		// been called.
		bool use_suff_term = false;
		string lc = Unicode::tolower(suff_term);
		if (db.term_exists(lc)) {
		    use_suff_term = true;
		} else {
		    lc = Unicode::tolower(term);
		    if (!db.term_exists(lc)) use_suff_term = true;
		}
		if (use_suff_term) {
		    term = suff_term;
		    it = p;
		}
	    }
	}
    }
    return term;
}

Query
QueryParser::Internal::parse_query(const string &qs, unsigned flags,
				   const string &default_prefix)
{
    yyParser * pParser = ParseAlloc();

    // Set value_ranges if we may have to handle value ranges in the query.
    bool value_ranges;
    value_ranges = !valrangeprocs.empty() && (qs.find("..") != string::npos);

    termpos term_pos = 1;
    Utf8Iterator it(qs), end;

    State state(this, flags);

    // To successfully apply more than one spelling correction to a query
    // string, we must keep track of the offset due to previous corrections.
    int correction_offset = 0;
    corrected_query.resize(0);

    list<string> prefix_stack;
    // We always have the current prefix on the top of the stack.
    prefix_stack.push_back(default_prefix);

    vector<string> term_group;

    unsigned newprev = ' ';
main_lex_loop:
    enum {
	DEFAULT, IN_QUOTES, IN_PREFIXED_QUOTES, IN_PHRASED_TERM, IN_GROUP
    } mode = DEFAULT;
    while (it != end) {
	bool last_was_operator = false;
	if (false) {
just_had_operator:
	    if (it == end) break;
	    last_was_operator = true;
	    mode = DEFAULT;
	}
	if (mode == IN_PHRASED_TERM) mode = DEFAULT;
	if (is_whitespace(*it)) {
	    newprev = ' ';
	    ++it;
	    it = find_if(it, end, is_not_whitespace);
	    if (it == end) break;
	}

	if ((mode == DEFAULT || mode == IN_GROUP) && value_ranges) {
	    // Scan forward to see if this could be the "start of range"
	    // token.  Sadly this has O(n^2) tendencies, though at least
	    // "n" is the number of words in a query which is likely to
	    // remain fairly small.  FIXME: can we tokenise more elegantly?
	    Utf8Iterator p = it;
	    unsigned ch = 0;
	    while (p != end) {
		if (ch == '.' && *p == '.') {
		    ++p;
		    if (p == end || *p <= ' ' || *p == ')') break;

		    string r;
		    do {
			Unicode::append_utf8(r, *it++);
		    } while (it != p);
		    // Trim off the trailing "..".
		    r.resize(r.size() - 2);
		    Parse(pParser, RANGE_START, new Term(r), &state);
		    r.resize(0);
		    // Allow any character except whitespace and ')' in a
		    // RANGE_END.  Or should we be consistent with RANGE_START?
		    do {
			Unicode::append_utf8(r, *p++);
		    } while (p != end && *p > ' ' && *p != ')');
		    Parse(pParser, RANGE_END, new Term(r), &state);
		    it = p;
		    goto main_lex_loop;
		}
		ch = *p;
		if (!(is_wordchar(ch) || is_currency(ch) ||
		      (ch < 128 && strchr("%,-./:@", ch)))) break;
		++p;
	    }
	}

	if (!is_wordchar(*it)) {
	    unsigned prev = newprev;
	    unsigned ch = *it++;
	    newprev = ch;
	    // Drop out of IN_GROUP mode.
	    if (mode == IN_GROUP) mode = DEFAULT;
	    switch (ch) {
	      case '"': // Quoted phrase.
		// Skip whitespace.
		it = find_if(it, end, is_not_whitespace);
		if (mode == DEFAULT) {
		    if (it == end) {
			// Ignore an unmatched " at the end of the query to
			// avoid generating an empty pair of QUOTEs which will
			// cause a parse error.
			goto done;
		    }
		    if (*it == '"') {
			// Ignore empty "" (but only if we're not already
			// IN_QUOTES as we don't merge two adjacent quoted
			// phrases!)
			newprev = *it++;
			break;
		    }
		}
		if (flags & QueryParser::FLAG_PHRASE) {
		    Parse(pParser, QUOTE, NULL, &state);
		    if (mode == DEFAULT) {
			mode = IN_QUOTES;
		    } else {
			// Remove the prefix we pushed for this phrase.
			if (mode == IN_PREFIXED_QUOTES)
			    prefix_stack.pop_back();
			mode = DEFAULT;
		    }
		}
		break;

	      case '+': case '-': // Loved or hated term/phrase/subexpression.
		// Ignore + or - at the end of the query string.
		if (it == end) goto done;
		if (prev > ' ' && prev != '(') {
		    // Or if not after whitespace or an open bracket.
		    break;
		}
		if (is_whitespace(*it) || *it == '+' || *it == '-') {
		    // Ignore + or - followed by a space, or further + or -.
		    // Postfix + (such as in C++ and H+) is handled as part of
		    // the term lexing code in parse_term().
		    newprev = *it++;
		    break;
		}
		if (mode == DEFAULT && (flags & FLAG_LOVEHATE)) {
		    Parse(pParser, (ch == '+' ? LOVE : HATE), NULL, &state);
		    goto just_had_operator;
		}
		// Need to prevent the term after a LOVE or HATE starting a
		// term group...
		break;

	      case '(': // Bracketed subexpression.
		// Skip whitespace.
		it = find_if(it, end, is_not_whitespace);
		// Ignore ( at the end of the query string.
		if (it == end) goto done;
		if (prev > ' ' && strchr("()+-", prev) == NULL) {
		    // Or if not after whitespace or a bracket or '+' or '-'.
		    break;
		}
		if (*it == ')') {
		    // Ignore empty ().
		    newprev = *it++;
		    break;
		}
		if (mode == DEFAULT && (flags & FLAG_BOOLEAN)) {
		    prefix_stack.push_back(prefix_stack.back());
		    Parse(pParser, BRA, NULL, &state);
		}
		break;

	      case ')': // End of bracketed subexpression.
		if (term_pos == 1) {
		    // Ignore ) at start of query.
		    break;
		}
		if (mode == DEFAULT && (flags & FLAG_BOOLEAN)) {
		    // Remove the prefix we pushed for the corresponding BRA.
		    // If brackets are unmatched, it's a syntax error, but
		    // that's no excuse to SEGV!
		    if (prefix_stack.size() > 1) prefix_stack.pop_back();
		    Parse(pParser, KET, NULL, &state);
		}
		break;

	      case '~': // Synonym expansion.
		// Ignore at the end of the query string.
		if (it == end) goto done;
		if (prev > ' ' && prev != '+' && prev != '-' && prev != '(') {
		    // Or if not after whitespace, +, -, or an open bracket.
		    break;
		}
		if (!is_wordchar(*it)) {
		    // Ignore if not followed by a word character.
		    break;
		}
		if (mode == DEFAULT && (flags & FLAG_SYNONYM)) {
		    Parse(pParser, SYNONYM, NULL, &state);
		    goto just_had_operator;
		}
		break;
	    }
	    // Skip any other characters.
	    continue;
	}

	Assert(is_wordchar(*it));

	size_t term_start_index = it.raw() - qs.data();

	newprev = 'A'; // Any letter will do...

	// A term, a prefix, or a boolean operator.
	string prefix;
	if ((mode == DEFAULT || mode == IN_GROUP) && !prefixes.empty()) {
	    // Check for fieldname prefixes (e.g. title:historical).
	    Utf8Iterator p = find_if(it, end, is_not_wordchar);
	    if (p != end && *p == ':' && ++p != end && *p > ' ' && *p != ')') {
		string field;
		p = it;
		while (*p != ':')
		    Unicode::append_utf8(field, *p++);
		map<string, PrefixInfo>::const_iterator f;
		f = prefixes.find(field);
		if (f != prefixes.end()) {
		    // Special handling for prefixed fields, depending on the
		    // type of the prefix.
		    unsigned ch = *++p;
		    PrefixInfo::prefix_type type = f->second.type;
		    prefix = f->second.str;

		    if (type == PrefixInfo::BOOL_FILTER) {
			// Can't boolean prefix a subexpression or phrase;
			// just use anything following the boolean prefix
			// until the next space or ')' as part of the boolean
			// term.
			it = p;
			string name;
			while (it != end && *it > ' ' && *it != ')')
			    Unicode::append_utf8(name, *it++);
			// Build the unstemmed form in field.
			field += ':';
			field += name;
			Parse(pParser, BOOLEAN_FILTER,
			      new Term(&state, name, prefix, field), &state);
			continue;
		    }

		    Assert(type == PrefixInfo::FREE_TEXT);

		    if (ch == '"' && (flags & FLAG_PHRASE)) {
			// Prefixed phrase, e.g.: subject:"space flight"
			mode = IN_PREFIXED_QUOTES;
			Parse(pParser, QUOTE, NULL, &state);
			it = p;
			newprev = ch;
			++it;
			prefix_stack.push_back(prefix);
			continue;
		    }

		    if (ch == '(' && (flags & FLAG_BOOLEAN)) {
			// Prefixed subexpression, e.g.: title:(fast NEAR food)
			mode = DEFAULT;
			Parse(pParser, BRA, NULL, &state);
			it = p;
			newprev = ch;
			++it;
			prefix_stack.push_back(prefix);
			continue;
		    }

		    if (is_wordchar(ch)) {
			// Prefixed term.
			it = p;
		    } else {
			// It looks like a prefix but isn't, so parse it as
			// text instead.
			prefix.resize(0);
		    }
		}
	    }
	}

phrased_term:
	bool was_acronym;
	string term = parse_term(it, end, was_acronym);

	// Boolean operators.
	if ((mode == DEFAULT || mode == IN_GROUP) &&
	    (flags & FLAG_BOOLEAN) &&
	    // Don't want to interpret A.N.D. as an AND operator.
	    !was_acronym &&
	    prefix.empty() &&
	    term.size() >= 2 && term.size() <= 4 && U_isalpha(term[0])) {

	    string op = term;
	    if (flags & FLAG_BOOLEAN_ANY_CASE) {
		for (string::iterator i = op.begin(); i != op.end(); ++i) {
		    *i = C_toupper(*i);
		}
	    }
	    if (op.size() == 3) {
		if (op == "AND") {
		    Parse(pParser, AND, NULL, &state);
		    goto just_had_operator;
		}
		if (op == "NOT") {
		    Parse(pParser, NOT, NULL, &state);
		    goto just_had_operator;
		}
		if (op == "XOR") {
		    Parse(pParser, XOR, NULL, &state);
		    goto just_had_operator;
		}
		if (op == "ADJ") {
		    if (it != end && *it == '/') {
			size_t width = 0;
			Utf8Iterator p = it;
			while (++p != end && U_isdigit(*p)) {
			    width = (width * 10) + (*p - '0');
			}
			if (width && (p == end || is_whitespace(*p))) {
			    it = p;
			    Parse(pParser, ADJ, new Term(width), &state);
			    goto just_had_operator;
			}
		    }

		    Parse(pParser, ADJ, NULL, &state);
		    goto just_had_operator;
		}
	    } else if (op.size() == 2) {
		if (op == "OR") {
		    Parse(pParser, OR, NULL, &state);
		    goto just_had_operator;
		}
	    } else if (op.size() == 4) {
		if (op == "NEAR") {
		    if (it != end && *it == '/') {
			size_t width = 0;
			Utf8Iterator p = it;
			while (++p != end && U_isdigit(*p)) {
			    width = (width * 10) + (*p - '0');
			}
			if (width && (p == end || is_whitespace(*p))) {
			    it = p;
			    Parse(pParser, NEAR, new Term(width), &state);
			    goto just_had_operator;
			}
		    }

		    Parse(pParser, NEAR, NULL, &state);
		    goto just_had_operator;
		}
	    }
	}

	if (prefix.empty()) prefix = prefix_stack.back();

	{
	    string unstemmed_term(term);
	    term = Unicode::tolower(term);
	    // Don't stem if:
	    // * We're not stemming terms at all!
	    // * We're not stemming all terms and "should_stem()" returns false.
	    // * If the term is followed by a stem preventer character.
	    bool dont_stem =
		!stemmer.internal.get() ||
		stem_action == STEM_NONE ||
		(stem_action == STEM_SOME && !should_stem(unstemmed_term)) ||
		(it != end && is_stem_preventer(*it));

	    Term * term_obj = new Term(&state, term, prefix, unstemmed_term,
				       !dont_stem, term_pos++);
	    if (flags & FLAG_SPELLING_CORRECTION) {
		if (!was_acronym && prefix.empty()) {
		    if (!db.term_exists(term)) {
			string suggestion = db.get_spelling_suggestion(term);
			if (!suggestion.empty()) {
			    if (corrected_query.empty()) corrected_query = qs;
			    size_t term_end_index = it.raw() - qs.data();
			    size_t n = term_end_index - term_start_index;
			    size_t pos = term_start_index + correction_offset;
			    corrected_query.replace(pos, n, suggestion);
			    correction_offset += suggestion.size();
			    correction_offset -= n;
			}
		    }
		}
	    }
	    if (mode == IN_PHRASED_TERM) {
		Parse(pParser, PHR_TERM, term_obj, &state);
	    } else {
		if (mode == DEFAULT || mode == IN_GROUP) {
		    if (it != end) {
			if ((flags & FLAG_WILDCARD) && *it == '*') {
			    Utf8Iterator p(it);
			    ++p;
			    if (p == end || !is_wordchar(*p)) {
				it = p;
				// Wildcard at end of term (also known as
				// "right truncation").
				Parse(pParser, WILD_TERM, term_obj, &state);
				continue;
			    }
			}
		    } else {
			if (flags & FLAG_PARTIAL) {
			    // Final term of a partial match query, with no
			    // following characters - treat as a wildcard.
			    Parse(pParser, PARTIAL_TERM, term_obj, &state);
			    continue;
			}
		    }
		}

		// See if the next token will be PHR_TERM - if so, this one
		// needs to be TERM not GROUP_TERM.
		if (mode == IN_GROUP && is_phrase_generator(*it)) {
		    // FIXME: can we clean this up?
		    Utf8Iterator p = it;
		    do {
			++p;
		    } while (p != end && is_phrase_generator(*p));
		    // Don't generate a phrase unless the phrase generators are
		    // immediately followed by another term.
		    if (p != end && is_wordchar(*p)) {
			mode = DEFAULT;
		    }
		}

		Parse(pParser, (mode == IN_GROUP ? GROUP_TERM : TERM),
		      term_obj, &state);
		if (mode != DEFAULT && mode != IN_GROUP) continue;
	    }
	}

	if (it == end) break;

	if (is_phrase_generator(*it)) {
	    // Skip multiple phrase generators.
	    do {
		++it;
	    } while (it != end && is_phrase_generator(*it));
	    // Don't generate a phrase unless the phrase generators are
	    // immediately followed by another term.
	    if (it != end && is_wordchar(*it)) {
		mode = IN_PHRASED_TERM;
		goto phrased_term;
	    }
	} else if (mode == DEFAULT || mode == IN_GROUP) {
	    mode = DEFAULT;
	    if (!last_was_operator && is_whitespace(*it)) {
		newprev = ' ';
		// Skip multiple whitespace.
		do {
		    ++it;
		} while (it != end && is_whitespace(*it));
		// Don't generate a group unless the terms are only separated
		// by whitespace.
		if (it != end && is_wordchar(*it)) {
		    mode = IN_GROUP;
		}
	    }
	}
    }
done:
    // Implicitly close any unclosed quotes...
    if (mode == IN_QUOTES || mode == IN_PREFIXED_QUOTES)
	Parse(pParser, QUOTE, NULL, &state);
    Parse(pParser, 0, NULL, &state);
    ParseFree(pParser);

    errmsg = state.error;
    return state.query;
}

struct ProbQuery {
    Query * query;
    Query * love;
    Query * hate;
    // filter is a map from prefix to a query for that prefix.  Queries with
    // the same prefix are combined with OR, and the results of this are
    // combined with AND to get the full filter.
    map<filter_group_id, Query> filter;

    ProbQuery() : query(0), love(0), hate(0) { }
    ~ProbQuery() {
	delete query;
	delete love;
	delete hate;
    }

    Query merge_filters() const {
	map<filter_group_id, Query>::const_iterator i = filter.begin();
	Assert(i != filter.end());
	Query q = i->second;
	while (++i != filter.end()) {
	    q = Query(Query::OP_AND, q, i->second);
	}
	return q;
    }
};

class TermGroup {
    list<Term *> terms;

  public:
    TermGroup() { }

    /// Add a Term object to this TermGroup object.
    void add_term(Term * term) {
	terms.push_back(term);
    }

    /// Convert to a Xapian::Query * using default_op.
    Query * as_group(State *state) const;

    /** Provide a way to explicitly delete an object of this class.  The
     *  destructor is protected to prevent auto-variables of this type.
     */
    void destroy() { delete this; }

  protected:
    /** Protected destructor, so an auto-variable of this type is a
     *  compile-time error - you must allocate this object with new.
     */
    ~TermGroup() {
	list<Term*>::const_iterator i;
	for (i = terms.begin(); i != terms.end(); ++i) {
	    delete *i;
	}
    }
};

Query *
TermGroup::as_group(State *state) const
{
    Query * query = NULL;
    Query::op default_op = state->default_op();
    if (state->flags & QueryParser::FLAG_AUTO_MULTIWORD_SYNONYMS) {
	// Check for multi-word synonyms.
	Database db = state->get_database();

	string key;
	list<Term*>::const_iterator begin = terms.begin();
	list<Term*>::const_iterator i = begin;
	while (i != terms.end()) {
	    key.resize(0);
	    while (i != terms.end()) {
		if (!key.empty()) key += ' ';
		key += (*i)->name;
		++i;
	    }
	    // Greedily try to match as many consecutive words as possible.
	    TermIterator syn, end;
	    while (true) {
		syn = db.synonyms_begin(key);
		end = db.synonyms_end(key);
		if (syn != end) break;
		if (--i == begin) break;
		key.resize(key.size() - (*i)->name.size() - 1);
	    }
	    if (i == begin) {
		// No multi-synonym matches.
		if (state->is_stopword(*i)) {
		    state->add_to_stoplist(*i);
		} else {
		    add_to_query(query, default_op,
				 (*i)->as_query_with_auto_synonyms());
		}
		begin = ++i;
		continue;
	    }

	    Query * q = NULL;
	    list<Term*>::const_iterator j;
	    for (j = begin; j != i; ++j) {
		if (state->is_stopword(*j)) {
		    state->add_to_stoplist(*j);
		} else {
		    add_to_query(q, default_op, (*j)->as_query());
		}
	    }

	    // Use the position of the first term for the synonyms.
	    Xapian::termpos pos = (*begin)->pos;
	    begin = i;
	    while (syn != end) {
		add_to_query(q, Query::OP_OR, Query(*syn, 1, pos));
		++syn;
	    }
	    add_to_query(query, default_op, q);
	}
    } else {
	list<Term*>::const_iterator i;
	for (i = terms.begin(); i != terms.end(); ++i) {
	    if (state->is_stopword(*i)) {
		state->add_to_stoplist(*i);
	    } else {
		add_to_query(query, default_op,
			     (*i)->as_query_with_auto_synonyms());
	    }
	}
    }
    delete this;
    return query;
}

class TermList {
    list<Query> terms;
    size_t window;

  public:
    TermList() : window(0) { }

    /// Add a Term object to this TermList object.
    void add_term(Term * term) {
	terms.push_back(term->as_query_object());
	delete term;
    }

    /// Add an unstemmed Term object to this TermList object.
    void add_unstemmed_term(Term * term) {
	term->dont_stem();
	add_term(term);
    }

    void adjust_window(size_t alternative_window) {
	if (alternative_window > window) window = alternative_window;
    }

    /// Convert to a Xapian::Query * using adjacent OP_PHRASE.
    Query * as_phrase_query() const {
	Query * term;
	term = new Query(Query::OP_PHRASE, terms.begin(), terms.end(),
			 terms.size());
	delete this;
	return term;
    }

    /// Convert to a Xapian::Query * using OP_NEAR.
    Query * as_near_query() const {
	Query * term;
	// The common meaning of 'a NEAR b' is "a within 10 terms of b", which
	// means a window size of 11.  For more than 2 terms, we just add one
	// to the window size for each extra term.
	size_t w = window;
	if (w == 0) w = 10;
	term = new Query(Query::OP_NEAR, terms.begin(), terms.end(),
			 terms.size() + w - 1);
	delete this;
	return term;
    }

    /// Convert to a Xapian::Query * using OP_PHRASE to implement ADJ.
    Query * as_adj_query() const {
	Query * term;
	// The common meaning of 'a ADJ b' is "a at most 10 terms before b",
	// which means a window size of 11.  For more than 2 terms, we just add
	// one to the window size for each extra term.
	size_t w = window;
	if (w == 0) w = 10;
	term = new Query(Query::OP_PHRASE, terms.begin(), terms.end(),
			 terms.size() + w - 1);
	delete this;
	return term;
    }

    /** Provide a way to explicitly delete an object of this class.  The
     *  destructor is protected to prevent auto-variables of this type.
     */
    void destroy() { delete this; }

  protected:
    /** Protected destructor, so an auto-variable of this type is a
     *  compile-time error - you must allocate this object with new.
     */
    ~TermList() { }
};

// Helper macro for converting a boolean operation into a Xapian::Query.
#define BOOL_OP_TO_QUERY(E, A, OP, B, OP_TXT) \
    do {\
	if (!A || !B) {\
	    state->error = "Syntax: <expression> "OP_TXT" <expression>";\
	    yy_parse_failed(yypParser);\
	    return;\
	}\
	E = new Query(OP, *A, *B);\
	delete A;\
	delete B;\
    } while (0)

#line 1192 "queryparser/queryparser_internal.cc"
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
/* Make sure the INTERFACE macro is defined.
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    ParseTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is ParseTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.
**    ParseARG_SDECL     A static variable declaration for the %extra_argument
**    ParseARG_PDECL     A parameter declaration for the %extra_argument
**    ParseARG_STORE     Code to store %extra_argument into yypParser
**    ParseARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 38
#define YYACTIONTYPE unsigned char
#define ParseTOKENTYPE Term *
typedef union {
  ParseTOKENTYPE yy0;
  TermList * yy1;
  int yy8;
  ProbQuery * yy9;
  Query * yy13;
  TermGroup * yy60;
  int yy75;
} YYMINORTYPE;
#define YYSTACKDEPTH 100
#define ParseARG_SDECL State * state;
#define ParseARG_PDECL ,State * state
#define ParseARG_FETCH State * state = yypParser->state
#define ParseARG_STORE yypParser->state = state
#define YYNSTATE 73
#define YYNRULE 51
#define YYERRORSYMBOL 22
#define YYERRSYMDT yy75
#define YY_NO_ACTION      (YYNSTATE+YYNRULE+2)
#define YY_ACCEPT_ACTION  (YYNSTATE+YYNRULE+1)
#define YY_ERROR_ACTION   (YYNSTATE+YYNRULE)

/* Next are that tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
*/
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   125,    1,    2,    3,   13,   43,   46,   60,   70,   73,
 /*    10 */    30,   32,   34,   37,   12,    2,    5,   13,   43,   46,
 /*    20 */    60,   70,   19,   30,   32,   34,   37,   12,    2,    7,
 /*    30 */    13,   43,   46,   60,   70,   21,   30,   32,   34,   37,
 /*    40 */    12,    2,    9,   13,   43,   46,   60,   70,   29,   30,
 /*    50 */    32,   34,   37,   12,    2,   11,   13,   43,   46,   60,
 /*    60 */    70,   31,   30,   32,   34,   37,   41,    2,    3,   13,
 /*    70 */    43,   46,   60,   70,   33,   30,   32,   34,   37,   12,
 /*    80 */     2,   72,   13,   43,   46,   60,   70,   74,   30,   32,
 /*    90 */    34,   37,   10,    4,    6,   62,   65,   54,   69,    4,
 /*   100 */     6,   23,   24,   68,   44,   26,   25,   40,   71,   36,
 /*   110 */    35,   62,   65,   54,   69,   38,   28,   23,   24,   68,
 /*   120 */    44,   39,   25,   40,   27,   45,   62,   65,   54,   69,
 /*   130 */    42,   48,   23,   24,   68,   44,   55,   25,   40,   90,
 /*   140 */    90,   99,   99,   54,   15,   90,   90,   23,   24,   99,
 /*   150 */    99,   90,   25,   40,  103,   90,  103,  103,  103,  103,
 /*   160 */    18,   20,    8,   10,    4,    6,   17,   16,   54,   52,
 /*   170 */    90,   90,   23,   24,   51,  103,   90,   25,   40,   54,
 /*   180 */    52,   90,   90,   23,   24,   58,   54,   52,   25,   40,
 /*   190 */    23,   24,   64,   90,   90,   25,   40,   54,   52,   90,
 /*   200 */    90,   23,   24,   67,   90,   90,   25,   40,   14,   22,
 /*   210 */    90,   30,   32,   34,   37,   90,   50,   90,   90,   53,
 /*   220 */    90,   30,   32,   34,   37,   57,   90,   90,   53,   90,
 /*   230 */    30,   32,   34,   37,   54,   15,   90,   90,   23,   24,
 /*   240 */    90,   90,   90,   25,   40,   61,   22,   90,   30,   32,
 /*   250 */    34,   37,   90,   63,   90,   90,   53,   90,   30,   32,
 /*   260 */    34,   37,   66,   90,   90,   53,   90,   30,   32,   34,
 /*   270 */    37,  104,   90,  104,  104,  104,  104,   90,   18,   20,
 /*   280 */    90,   90,   49,   56,   17,   16,   90,   90,   90,   90,
 /*   290 */    59,   47,  104,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    23,   24,   25,   26,   27,   28,   29,   30,   31,    0,
 /*    10 */    33,   34,   35,   36,   24,   25,   26,   27,   28,   29,
 /*    20 */    30,   31,   11,   33,   34,   35,   36,   24,   25,   26,
 /*    30 */    27,   28,   29,   30,   31,   11,   33,   34,   35,   36,
 /*    40 */    24,   25,   26,   27,   28,   29,   30,   31,   11,   33,
 /*    50 */    34,   35,   36,   24,   25,   26,   27,   28,   29,   30,
 /*    60 */    31,   13,   33,   34,   35,   36,   24,   25,   26,   27,
 /*    70 */    28,   29,   30,   31,   12,   33,   34,   35,   36,   24,
 /*    80 */    25,   26,   27,   28,   29,   30,   31,    0,   33,   34,
 /*    90 */    35,   36,    3,    4,    5,    8,    9,   10,   11,    4,
 /*   100 */     5,   14,   15,   16,   17,   32,   19,   20,    5,   11,
 /*   110 */     6,    8,    9,   10,   11,    7,   11,   14,   15,   16,
 /*   120 */    17,   11,   19,   20,   19,   18,    8,    9,   10,   11,
 /*   130 */    21,   18,   14,   15,   16,   17,   11,   19,   20,   37,
 /*   140 */    37,    8,    9,   10,   11,   37,   37,   14,   15,   16,
 /*   150 */    17,   37,   19,   20,    0,   37,    2,    3,    4,    5,
 /*   160 */     6,    7,    2,    3,    4,    5,   12,   13,   10,   11,
 /*   170 */    37,   37,   14,   15,   16,   21,   37,   19,   20,   10,
 /*   180 */    11,   37,   37,   14,   15,   16,   10,   11,   19,   20,
 /*   190 */    14,   15,   16,   37,   37,   19,   20,   10,   11,   37,
 /*   200 */    37,   14,   15,   16,   37,   37,   19,   20,   30,   31,
 /*   210 */    37,   33,   34,   35,   36,   37,   28,   37,   37,   31,
 /*   220 */    37,   33,   34,   35,   36,   28,   37,   37,   31,   37,
 /*   230 */    33,   34,   35,   36,   10,   11,   37,   37,   14,   15,
 /*   240 */    37,   37,   37,   19,   20,   30,   31,   37,   33,   34,
 /*   250 */    35,   36,   37,   28,   37,   37,   31,   37,   33,   34,
 /*   260 */    35,   36,   28,   37,   37,   31,   37,   33,   34,   35,
 /*   270 */    36,    0,   37,    2,    3,    4,    5,   37,    6,    7,
 /*   280 */    37,   37,    8,    9,   12,   13,   37,   37,   37,   37,
 /*   290 */    16,   17,   21,
};
#define YY_SHIFT_USE_DFLT (-1)
static const short yy_shift_ofst[] = {
 /*     0 */    87,    9,   -1,  160,  103,   -1,  118,   -1,  118,   89,
 /*    10 */   118,   95,   -1,  133,   -1,  272,   -1,   -1,   11,   -1,
 /*    20 */    24,   -1,   -1,   -1,   -1,   37,  105,   -1,   -1,   -1,
 /*    30 */    48,   -1,   62,   -1,  104,   98,   -1,  108,  110,   -1,
 /*    40 */   118,  109,   -1,   -1,  107,   -1,  274,  113,   -1,  158,
 /*    50 */    -1,   -1,  272,   -1,  125,   -1,  169,   -1,   -1,   -1,
 /*    60 */   224,   -1,  176,   -1,   -1,  187,   -1,   -1,   -1,  154,
 /*    70 */   271,  118,   -1,
};
#define YY_REDUCE_USE_DFLT (-24)
static const short yy_reduce_ofst[] = {
 /*     0 */   -23,  -24,  -24,  -24,  -10,  -24,    3,  -24,   16,  -24,
 /*    10 */    29,  -24,  -24,  178,  -24,  -24,  -24,  -24,  -24,  -24,
 /*    20 */   -24,  -24,  -24,  -24,  -24,   73,  -24,  -24,  -24,  -24,
 /*    30 */   -24,  -24,  -24,  -24,  -24,  -24,  -24,  -24,  -24,  -24,
 /*    40 */    42,  -24,  -24,  -24,  -24,  -24,  -24,  -24,  -24,  188,
 /*    50 */   -24,  -24,  -24,  -24,  -24,  -24,  197,  -24,  -24,  -24,
 /*    60 */   215,  -24,  225,  -24,  -24,  234,  -24,  -24,  -24,  -24,
 /*    70 */   -24,   55,  -24,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */    82,   81,   75,  124,   82,   76,   82,   77,   82,   79,
 /*    10 */    82,   80,   81,   83,   88,  101,  116,  118,  124,  120,
 /*    20 */   124,  122,  102,  105,  106,  124,  124,  107,  115,  114,
 /*    30 */   108,  117,  109,  119,  110,  124,  121,  111,  124,  123,
 /*    40 */    82,   81,  112,   84,  124,   85,  124,  124,   86,  124,
 /*    50 */    90,   98,  103,  104,  124,  113,  124,   92,   94,   96,
 /*    60 */   100,   87,  124,   89,   97,  124,   91,   93,   95,  101,
 /*    70 */   102,   82,   78,
};
#define YY_SZ_ACTTAB (int)(sizeof(yy_action)/sizeof(yy_action[0]))

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammer, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
#ifdef YYFALLBACK
static const YYCODETYPE yyFallback[] = {
};
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
struct yyStackEntry {
  int stateno;       /* The state-number */
  int major;         /* The major token value.  This is the code
                     ** number for the token at this stack level */
  YYMINORTYPE minor; /* The user-supplied minor token value.  This
                     ** is the value of the token  */
};
typedef struct yyStackEntry yyStackEntry;

/* The state of the parser is completely contained in an instance of
** the following structure */
struct yyParser {
  int yyidx;                    /* Index of top element in stack */
  int yyerrcnt;                 /* Shifts left before out of the error */
  ParseARG_SDECL                /* A place to hold %extra_argument */
  yyStackEntry yystack[YYSTACKDEPTH];  /* The parser's stack */
};
typedef struct yyParser yyParser;

/* Prototype this here so we can call it from a rule action (ick). */
static void yy_parse_failed(yyParser *);

#include "omdebug.h"

#ifdef XAPIAN_DEBUG_VERBOSE
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static const char *const yyTokenName[] = {
  "$",             "ERROR",         "OR",            "XOR",         
  "AND",           "NOT",           "NEAR",          "ADJ",         
  "LOVE",          "HATE",          "SYNONYM",       "TERM",        
  "GROUP_TERM",    "PHR_TERM",      "WILD_TERM",     "PARTIAL_TERM",
  "BOOLEAN_FILTER",  "RANGE_START",   "RANGE_END",     "QUOTE",       
  "BRA",           "KET",           "error",         "query",       
  "expr",          "prob_expr",     "bool_arg",      "prob",        
  "term",          "stop_prob",     "stop_term",     "compound_term",
  "phrase",        "phrased_term",  "group",         "near_expr",   
  "adj_expr",    
};

/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "query ::= expr",
 /*   1 */ "query ::=",
 /*   2 */ "expr ::= prob_expr",
 /*   3 */ "expr ::= bool_arg AND bool_arg",
 /*   4 */ "expr ::= bool_arg NOT bool_arg",
 /*   5 */ "expr ::= bool_arg AND NOT bool_arg",
 /*   6 */ "expr ::= bool_arg OR bool_arg",
 /*   7 */ "expr ::= bool_arg XOR bool_arg",
 /*   8 */ "bool_arg ::= expr",
 /*   9 */ "bool_arg ::=",
 /*  10 */ "prob_expr ::= prob",
 /*  11 */ "prob_expr ::= term",
 /*  12 */ "prob ::= RANGE_START RANGE_END",
 /*  13 */ "prob ::= stop_prob RANGE_START RANGE_END",
 /*  14 */ "prob ::= stop_term stop_term",
 /*  15 */ "prob ::= prob stop_term",
 /*  16 */ "prob ::= LOVE term",
 /*  17 */ "prob ::= stop_prob LOVE term",
 /*  18 */ "prob ::= HATE term",
 /*  19 */ "prob ::= stop_prob HATE term",
 /*  20 */ "prob ::= HATE BOOLEAN_FILTER",
 /*  21 */ "prob ::= stop_prob HATE BOOLEAN_FILTER",
 /*  22 */ "prob ::= BOOLEAN_FILTER",
 /*  23 */ "prob ::= stop_prob BOOLEAN_FILTER",
 /*  24 */ "prob ::= LOVE BOOLEAN_FILTER",
 /*  25 */ "prob ::= stop_prob LOVE BOOLEAN_FILTER",
 /*  26 */ "stop_prob ::= prob",
 /*  27 */ "stop_prob ::= stop_term",
 /*  28 */ "stop_term ::= TERM",
 /*  29 */ "stop_term ::= compound_term",
 /*  30 */ "term ::= TERM",
 /*  31 */ "term ::= compound_term",
 /*  32 */ "compound_term ::= WILD_TERM",
 /*  33 */ "compound_term ::= PARTIAL_TERM",
 /*  34 */ "compound_term ::= QUOTE phrase QUOTE",
 /*  35 */ "compound_term ::= phrased_term",
 /*  36 */ "compound_term ::= group",
 /*  37 */ "compound_term ::= near_expr",
 /*  38 */ "compound_term ::= adj_expr",
 /*  39 */ "compound_term ::= BRA expr KET",
 /*  40 */ "compound_term ::= SYNONYM TERM",
 /*  41 */ "phrase ::= TERM",
 /*  42 */ "phrase ::= phrase TERM",
 /*  43 */ "phrased_term ::= TERM PHR_TERM",
 /*  44 */ "phrased_term ::= phrased_term PHR_TERM",
 /*  45 */ "group ::= TERM GROUP_TERM",
 /*  46 */ "group ::= group GROUP_TERM",
 /*  47 */ "near_expr ::= TERM NEAR TERM",
 /*  48 */ "near_expr ::= near_expr NEAR TERM",
 /*  49 */ "adj_expr ::= TERM ADJ TERM",
 /*  50 */ "adj_expr ::= adj_expr ADJ TERM",
};

/*
** This function returns the symbolic name associated with a token
** value.
*/
static const char *ParseTokenName(int tokenType){
  if( tokenType>0 && size_t(tokenType)<(sizeof(yyTokenName)/sizeof(yyTokenName[0])) ){
    return yyTokenName[tokenType];
  }
  return "Unknown";
}

/*
** This function returns the symbolic name associated with a rule
** value.
*/
static const char *ParseRuleName(int ruleNum){
  if( ruleNum>0 && size_t(ruleNum)<(sizeof(yyRuleName)/sizeof(yyRuleName[0])) ){
    return yyRuleName[ruleNum];
  }
  return "Unknown";
}
#endif /* XAPIAN_DEBUG_VERBOSE */

/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** None.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to Parse and ParseFree.
*/
static yyParser *ParseAlloc(){
  yyParser *pParser;
  pParser = new yyParser;
  pParser->yyidx = -1;
  return pParser;
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
static void yy_destructor(YYCODETYPE yymajor, YYMINORTYPE *yypminor){
  switch( yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are not used
    ** inside the C code.
    */
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
    case 18:
    case 19:
    case 20:
    case 21:
#line 1187 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{delete (yypminor->yy0);}
#line 1617 "queryparser/queryparser_internal.cc"
      break;
    case 24:
    case 25:
    case 26:
    case 28:
    case 30:
    case 31:
#line 1258 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{delete (yypminor->yy13);}
#line 1627 "queryparser/queryparser_internal.cc"
      break;
    case 27:
    case 29:
#line 1347 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{delete (yypminor->yy9);}
#line 1633 "queryparser/queryparser_internal.cc"
      break;
    case 32:
    case 33:
    case 35:
    case 36:
#line 1548 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{(yypminor->yy1)->destroy();}
#line 1641 "queryparser/queryparser_internal.cc"
      break;
    case 34:
#line 1582 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{(yypminor->yy60)->destroy();}
#line 1646 "queryparser/queryparser_internal.cc"
      break;
    default:  break;   /* If no destructor action specified: do nothing */
  }
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
static int yy_pop_parser_stack(yyParser *pParser){
  YYCODETYPE yymajor;
  yyStackEntry *yytos = &pParser->yystack[pParser->yyidx];

  if( pParser->yyidx<0 ) return 0;
  DEBUGLINE(QUERYPARSER, "Popping " << ParseTokenName(yytos->major));
  yymajor = (YYCODETYPE)yytos->major;
  yy_destructor( yymajor, &yytos->minor);
  pParser->yyidx--;
  return yymajor;
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** A pointer to the parser.  This should be a pointer
** obtained from ParseAlloc.
*/
static void ParseFree(
  yyParser *pParser           /* The parser to be deleted */
){
  if( pParser==0 ) return;
  while( pParser->yyidx>=0 ) yy_pop_parser_stack(pParser);
  delete pParser;
}

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_shift_action(
  yyParser *pParser,        /* The parser */
  int iLookAhead            /* The look-ahead token */
){
  int i;
  int stateno = pParser->yystack[pParser->yyidx].stateno;
 
  /* if( pParser->yyidx<0 ) return YY_NO_ACTION;  */
  i = yy_shift_ofst[stateno];
  if( i==YY_SHIFT_USE_DFLT ){
    return yy_default[stateno];
  }
  if( iLookAhead==YYNOCODE ){
    return YY_NO_ACTION;
  }
  i += iLookAhead;
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
#ifdef YYFALLBACK
    int iFallback;            /* Fallback token */
    if( iLookAhead<sizeof(yyFallback)/sizeof(yyFallback[0])
           && (iFallback = yyFallback[iLookAhead])!=0 ){
      DEBUGLINE(QUERYPARSER,
		"FALLBACK " << ParseTokenName(iLookAhead) << " => " <<
		ParseTokenName(iFallback));
      return yy_find_shift_action(pParser, iFallback);
    }
#endif
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_reduce_action(
  yyParser *pParser,        /* The parser */
  int iLookAhead            /* The look-ahead token */
){
  int i;
  int stateno = pParser->yystack[pParser->yyidx].stateno;
 
  i = yy_reduce_ofst[stateno];
  if( i==YY_REDUCE_USE_DFLT ){
    return yy_default[stateno];
  }
  if( iLookAhead==YYNOCODE ){
    return YY_NO_ACTION;
  }
  i += iLookAhead;
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Perform a shift action.
*/
static void yy_shift(
  yyParser *yypParser,          /* The parser to be shifted */
  int yyNewState,               /* The new state to shift in */
  int yyMajor,                  /* The major token to shift in */
  YYMINORTYPE *yypMinor         /* Pointer ot the minor token to shift in */
){
  yyStackEntry *yytos;
  yypParser->yyidx++;
  if( yypParser->yyidx>=YYSTACKDEPTH ){
     ParseARG_FETCH;
     yypParser->yyidx--;
     DEBUGLINE(QUERYPARSER, "Stack Overflow!");
     while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
     /* Here code is inserted which will execute if the parser
     ** stack every overflows */
     ParseARG_STORE; /* Suppress warning about unused %extra_argument var */
     return;
  }
  yytos = &yypParser->yystack[yypParser->yyidx];
  yytos->stateno = yyNewState;
  yytos->major = yyMajor;
  yytos->minor = *yypMinor;
#ifdef XAPIAN_DEBUG_VERBOSE
  if(yypParser->yyidx>0 ){
    int i;
    DEBUGLINE(QUERYPARSER, "Shift " << yyNewState);
    string stack("Stack:");
    for(i=1; i<=yypParser->yyidx; i++) {
      stack += ' ';
      stack += ParseTokenName(yypParser->yystack[i].major);
    }
    DEBUGLINE(QUERYPARSER, stack);
  }
#endif
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
static const struct {
  YYCODETYPE lhs;         /* Symbol on the left-hand side of the rule */
  unsigned char nrhs;     /* Number of right-hand side symbols in the rule */
} yyRuleInfo[] = {
  { 23, 1 },
  { 23, 0 },
  { 24, 1 },
  { 24, 3 },
  { 24, 3 },
  { 24, 4 },
  { 24, 3 },
  { 24, 3 },
  { 26, 1 },
  { 26, 0 },
  { 25, 1 },
  { 25, 1 },
  { 27, 2 },
  { 27, 3 },
  { 27, 2 },
  { 27, 2 },
  { 27, 2 },
  { 27, 3 },
  { 27, 2 },
  { 27, 3 },
  { 27, 2 },
  { 27, 3 },
  { 27, 1 },
  { 27, 2 },
  { 27, 2 },
  { 27, 3 },
  { 29, 1 },
  { 29, 1 },
  { 30, 1 },
  { 30, 1 },
  { 28, 1 },
  { 28, 1 },
  { 31, 1 },
  { 31, 1 },
  { 31, 3 },
  { 31, 1 },
  { 31, 1 },
  { 31, 1 },
  { 31, 1 },
  { 31, 3 },
  { 31, 2 },
  { 32, 1 },
  { 32, 2 },
  { 33, 2 },
  { 33, 2 },
  { 34, 2 },
  { 34, 2 },
  { 35, 3 },
  { 35, 3 },
  { 36, 3 },
  { 36, 3 },
};

static void yy_accept(yyParser*);  /* Forward Declaration */

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
static void yy_reduce(
  yyParser *yypParser,         /* The parser */
  int yyruleno                 /* Number of the rule by which to reduce */
){
  int yygoto;                     /* The next state */
  int yyact;                      /* The next action */
  YYMINORTYPE yygotominor;        /* The LHS of the rule reduced */
  yyStackEntry *yymsp;            /* The top of the parser's stack */
  int yysize;                     /* Amount to pop the stack */
  ParseARG_FETCH;
  yymsp = &yypParser->yystack[yypParser->yyidx];
#ifdef XAPIAN_DEBUG_VERBOSE
  DEBUGLINE(QUERYPARSER, "Reduce [" << ParseRuleName(yyruleno) << "].");
#endif

  switch( yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
      case 0:
#line 1240 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    // Save the parsed query in the State structure so we can return it.
    if (yymsp[0].minor.yy13) {
	state->query = *yymsp[0].minor.yy13;
	delete yymsp[0].minor.yy13;
    } else {
	state->query = Query();
    }
}
#line 1899 "queryparser/queryparser_internal.cc"
        break;
      case 1:
#line 1250 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    // Handle a query string with no terms in.
    state->query = Query();
}
#line 1907 "queryparser/queryparser_internal.cc"
        break;
      case 2:
      case 8:
#line 1261 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ yygotominor.yy13 = yymsp[0].minor.yy13; }
#line 1913 "queryparser/queryparser_internal.cc"
        break;
      case 3:
#line 1264 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ BOOL_OP_TO_QUERY(yygotominor.yy13, yymsp[-2].minor.yy13, Query::OP_AND, yymsp[0].minor.yy13, "AND");   yy_destructor(4,&yymsp[-1].minor);
}
#line 1919 "queryparser/queryparser_internal.cc"
        break;
      case 4:
#line 1266 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    // 'NOT foo' -> '<alldocuments> NOT foo'
    if (!yymsp[-2].minor.yy13 && (state->flags & QueryParser::FLAG_PURE_NOT)) {
	yymsp[-2].minor.yy13 = new Query("", 1, 0);
    }
    BOOL_OP_TO_QUERY(yygotominor.yy13, yymsp[-2].minor.yy13, Query::OP_AND_NOT, yymsp[0].minor.yy13, "NOT");
  yy_destructor(5,&yymsp[-1].minor);
}
#line 1931 "queryparser/queryparser_internal.cc"
        break;
      case 5:
#line 1275 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ BOOL_OP_TO_QUERY(yygotominor.yy13, yymsp[-3].minor.yy13, Query::OP_AND_NOT, yymsp[0].minor.yy13, "AND NOT");   yy_destructor(4,&yymsp[-2].minor);
  yy_destructor(5,&yymsp[-1].minor);
}
#line 1938 "queryparser/queryparser_internal.cc"
        break;
      case 6:
#line 1278 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ BOOL_OP_TO_QUERY(yygotominor.yy13, yymsp[-2].minor.yy13, Query::OP_OR, yymsp[0].minor.yy13, "OR");   yy_destructor(2,&yymsp[-1].minor);
}
#line 1944 "queryparser/queryparser_internal.cc"
        break;
      case 7:
#line 1281 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ BOOL_OP_TO_QUERY(yygotominor.yy13, yymsp[-2].minor.yy13, Query::OP_XOR, yymsp[0].minor.yy13, "XOR");   yy_destructor(3,&yymsp[-1].minor);
}
#line 1950 "queryparser/queryparser_internal.cc"
        break;
      case 9:
#line 1290 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    // Set the argument to NULL, which enables the bool_arg-using rules in
    // expr above to report uses of AND, OR, etc which don't have two
    // arguments.
    yygotominor.yy13 = NULL;
}
#line 1960 "queryparser/queryparser_internal.cc"
        break;
      case 10:
#line 1302 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy13 = yymsp[0].minor.yy9->query;
    yymsp[0].minor.yy9->query = NULL;
    // Handle any "+ terms".
    if (yymsp[0].minor.yy9->love) {
	if (yymsp[0].minor.yy9->love->empty()) {
	    // +<nothing>.
	    delete yygotominor.yy13;
	    yygotominor.yy13 = yymsp[0].minor.yy9->love;
	} else if (yygotominor.yy13) {
	    swap(yygotominor.yy13, yymsp[0].minor.yy9->love);
	    add_to_query(yygotominor.yy13, Query::OP_AND_MAYBE, yymsp[0].minor.yy9->love);
	} else {
	    yygotominor.yy13 = yymsp[0].minor.yy9->love;
	}
	yymsp[0].minor.yy9->love = NULL;
    }
    // Handle any "- terms".
    if (yymsp[0].minor.yy9->hate && !yymsp[0].minor.yy9->hate->empty()) {
	if (!yygotominor.yy13) {
	    // Can't just hate!
	    yy_parse_failed(yypParser);
	    return;
	}
	*yygotominor.yy13 = Query(Query::OP_AND_NOT, *yygotominor.yy13, *yymsp[0].minor.yy9->hate);
    }
    // Handle any boolean filters.
    if (!yymsp[0].minor.yy9->filter.empty()) {
	// FIXME: if (!yygotominor.yy13) make_the_query_boolean_somehow();
	add_to_query(yygotominor.yy13, Query::OP_FILTER, yymsp[0].minor.yy9->merge_filters());
    }
    // FIXME what if yygotominor.yy13 && yygotominor.yy13->empty() (all terms are stopwords)?
    delete yymsp[0].minor.yy9;
}
#line 1998 "queryparser/queryparser_internal.cc"
        break;
      case 11:
      case 29:
      case 31:
#line 1337 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy13 = yymsp[0].minor.yy13;
}
#line 2007 "queryparser/queryparser_internal.cc"
        break;
      case 12:
#line 1349 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    Query range;
    Xapian::valueno valno = state->value_range(range, yymsp[-1].minor.yy0, yymsp[0].minor.yy0);
    if (valno == BAD_VALUENO) {
	yy_parse_failed(yypParser);
	return;
    }
    yygotominor.yy9 = new ProbQuery;
    yygotominor.yy9->filter[filter_group_id(valno)] = range;
}
#line 2021 "queryparser/queryparser_internal.cc"
        break;
      case 13:
#line 1360 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    Query range;
    Xapian::valueno valno = state->value_range(range, yymsp[-1].minor.yy0, yymsp[0].minor.yy0);
    if (valno == BAD_VALUENO) {
	yy_parse_failed(yypParser);
	return;
    }
    yygotominor.yy9 = yymsp[-2].minor.yy9;
    Query & q = yygotominor.yy9->filter[filter_group_id(valno)];
    q = Query(Query::OP_OR, q, range);
}
#line 2036 "queryparser/queryparser_internal.cc"
        break;
      case 14:
#line 1372 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = new ProbQuery;
    yygotominor.yy9->query = yymsp[-1].minor.yy13;
    if (yymsp[0].minor.yy13) add_to_query(yygotominor.yy9->query, state->default_op(), yymsp[0].minor.yy13);
}
#line 2045 "queryparser/queryparser_internal.cc"
        break;
      case 15:
#line 1378 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = yymsp[-1].minor.yy9;
    // If yymsp[0].minor.yy13 is a stopword, there's nothing to do here.
    if (yymsp[0].minor.yy13) add_to_query(yygotominor.yy9->query, state->default_op(), yymsp[0].minor.yy13);
}
#line 2054 "queryparser/queryparser_internal.cc"
        break;
      case 16:
#line 1384 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = new ProbQuery;
    if (state->default_op() == Query::OP_AND) {
	yygotominor.yy9->query = yymsp[0].minor.yy13;
    } else {
	yygotominor.yy9->love = yymsp[0].minor.yy13;
    }
  yy_destructor(8,&yymsp[-1].minor);
}
#line 2067 "queryparser/queryparser_internal.cc"
        break;
      case 17:
#line 1393 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = yymsp[-2].minor.yy9;
    if (state->default_op() == Query::OP_AND) {
	/* The default op is AND, so we just put loved terms into the query
	 * (in this case the only effect of love is to ignore the stopword
	 * list). */
	add_to_query(yygotominor.yy9->query, Query::OP_AND, yymsp[0].minor.yy13);
    } else {
	add_to_query(yygotominor.yy9->love, Query::OP_AND, yymsp[0].minor.yy13);
    }
  yy_destructor(8,&yymsp[-1].minor);
}
#line 2083 "queryparser/queryparser_internal.cc"
        break;
      case 18:
#line 1405 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = new ProbQuery;
    yygotominor.yy9->hate = yymsp[0].minor.yy13;
  yy_destructor(9,&yymsp[-1].minor);
}
#line 2092 "queryparser/queryparser_internal.cc"
        break;
      case 19:
#line 1410 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = yymsp[-2].minor.yy9;
    add_to_query(yygotominor.yy9->hate, Query::OP_OR, yymsp[0].minor.yy13);
  yy_destructor(9,&yymsp[-1].minor);
}
#line 2101 "queryparser/queryparser_internal.cc"
        break;
      case 20:
#line 1415 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = new ProbQuery;
    yygotominor.yy9->hate = yymsp[0].minor.yy0->as_query();
    delete yymsp[0].minor.yy0;
  yy_destructor(9,&yymsp[-1].minor);
}
#line 2111 "queryparser/queryparser_internal.cc"
        break;
      case 21:
#line 1421 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = yymsp[-2].minor.yy9;
    add_to_query(yygotominor.yy9->hate, Query::OP_OR, yymsp[0].minor.yy0->as_query_object());
    delete yymsp[0].minor.yy0;
  yy_destructor(9,&yymsp[-1].minor);
}
#line 2121 "queryparser/queryparser_internal.cc"
        break;
      case 22:
#line 1427 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = new ProbQuery;
    yygotominor.yy9->filter[yymsp[0].minor.yy0->get_filter_group_id()] = yymsp[0].minor.yy0->as_query_object();
    delete yymsp[0].minor.yy0;
}
#line 2130 "queryparser/queryparser_internal.cc"
        break;
      case 23:
#line 1433 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = yymsp[-1].minor.yy9;
    // We OR filters with the same prefix...
    Query & q = yygotominor.yy9->filter[yymsp[0].minor.yy0->get_filter_group_id()];
    q = Query(Query::OP_OR, q, yymsp[0].minor.yy0->as_query_object());
    delete yymsp[0].minor.yy0;
}
#line 2141 "queryparser/queryparser_internal.cc"
        break;
      case 24:
#line 1441 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    // LOVE BOOLEAN_FILTER(yymsp[0].minor.yy0) is just the same as BOOLEAN_FILTER
    yygotominor.yy9 = new ProbQuery;
    yygotominor.yy9->filter[yymsp[0].minor.yy0->get_filter_group_id()] = yymsp[0].minor.yy0->as_query_object();
    delete yymsp[0].minor.yy0;
  yy_destructor(8,&yymsp[-1].minor);
}
#line 2152 "queryparser/queryparser_internal.cc"
        break;
      case 25:
#line 1448 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    // LOVE BOOLEAN_FILTER(yymsp[0].minor.yy0) is just the same as BOOLEAN_FILTER
    yygotominor.yy9 = yymsp[-2].minor.yy9;
    // We OR filters with the same prefix...
    Query & q = yygotominor.yy9->filter[yymsp[0].minor.yy0->get_filter_group_id()];
    q = Query(Query::OP_OR, q, yymsp[0].minor.yy0->as_query_object());
    delete yymsp[0].minor.yy0;
  yy_destructor(8,&yymsp[-1].minor);
}
#line 2165 "queryparser/queryparser_internal.cc"
        break;
      case 26:
#line 1463 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ yygotominor.yy9 = yymsp[0].minor.yy9; }
#line 2170 "queryparser/queryparser_internal.cc"
        break;
      case 27:
#line 1465 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy9 = new ProbQuery;
    yygotominor.yy9->query = yymsp[0].minor.yy13;
}
#line 2178 "queryparser/queryparser_internal.cc"
        break;
      case 28:
#line 1479 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    if (state->is_stopword(yymsp[0].minor.yy0)) {
	yygotominor.yy13 = NULL;
	state->add_to_stoplist(yymsp[0].minor.yy0);
    } else {
	yygotominor.yy13 = yymsp[0].minor.yy0->as_query_with_auto_synonyms();
    }
    delete yymsp[0].minor.yy0;
}
#line 2191 "queryparser/queryparser_internal.cc"
        break;
      case 30:
#line 1498 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy13 = yymsp[0].minor.yy0->as_query_with_auto_synonyms();
    delete yymsp[0].minor.yy0;
}
#line 2199 "queryparser/queryparser_internal.cc"
        break;
      case 32:
#line 1515 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ yygotominor.yy13 = yymsp[0].minor.yy0->as_wildcarded_query(state); }
#line 2204 "queryparser/queryparser_internal.cc"
        break;
      case 33:
#line 1518 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ yygotominor.yy13 = yymsp[0].minor.yy0->as_partial_query(state); }
#line 2209 "queryparser/queryparser_internal.cc"
        break;
      case 34:
#line 1521 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ yygotominor.yy13 = yymsp[-1].minor.yy1->as_phrase_query();   yy_destructor(19,&yymsp[-2].minor);
  yy_destructor(19,&yymsp[0].minor);
}
#line 2216 "queryparser/queryparser_internal.cc"
        break;
      case 35:
#line 1524 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ yygotominor.yy13 = yymsp[0].minor.yy1->as_phrase_query(); }
#line 2221 "queryparser/queryparser_internal.cc"
        break;
      case 36:
#line 1526 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy13 = yymsp[0].minor.yy60->as_group(state);
}
#line 2228 "queryparser/queryparser_internal.cc"
        break;
      case 37:
#line 1531 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ yygotominor.yy13 = yymsp[0].minor.yy1->as_near_query(); }
#line 2233 "queryparser/queryparser_internal.cc"
        break;
      case 38:
#line 1534 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ yygotominor.yy13 = yymsp[0].minor.yy1->as_adj_query(); }
#line 2238 "queryparser/queryparser_internal.cc"
        break;
      case 39:
#line 1537 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{ yygotominor.yy13 = yymsp[-1].minor.yy13;   yy_destructor(20,&yymsp[-2].minor);
  yy_destructor(21,&yymsp[0].minor);
}
#line 2245 "queryparser/queryparser_internal.cc"
        break;
      case 40:
#line 1539 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy13 = yymsp[0].minor.yy0->as_query_with_synonyms();
    delete yymsp[0].minor.yy0;
  yy_destructor(10,&yymsp[-1].minor);
}
#line 2254 "queryparser/queryparser_internal.cc"
        break;
      case 41:
#line 1550 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy1 = new TermList;
    yygotominor.yy1->add_unstemmed_term(yymsp[0].minor.yy0);
}
#line 2262 "queryparser/queryparser_internal.cc"
        break;
      case 42:
      case 44:
#line 1555 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy1 = yymsp[-1].minor.yy1;
    yygotominor.yy1->add_unstemmed_term(yymsp[0].minor.yy0);
}
#line 2271 "queryparser/queryparser_internal.cc"
        break;
      case 43:
#line 1567 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy1 = new TermList;
    yygotominor.yy1->add_unstemmed_term(yymsp[-1].minor.yy0);
    yygotominor.yy1->add_unstemmed_term(yymsp[0].minor.yy0);
}
#line 2280 "queryparser/queryparser_internal.cc"
        break;
      case 45:
#line 1584 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy60 = new TermGroup;
    yygotominor.yy60->add_term(yymsp[-1].minor.yy0);
    yygotominor.yy60->add_term(yymsp[0].minor.yy0);
}
#line 2289 "queryparser/queryparser_internal.cc"
        break;
      case 46:
#line 1590 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy60 = yymsp[-1].minor.yy60;
    yygotominor.yy60->add_term(yymsp[0].minor.yy0);
}
#line 2297 "queryparser/queryparser_internal.cc"
        break;
      case 47:
      case 49:
#line 1601 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy1 = new TermList;
    yygotominor.yy1->add_unstemmed_term(yymsp[-2].minor.yy0);
    yygotominor.yy1->add_unstemmed_term(yymsp[0].minor.yy0);
    if (yymsp[-1].minor.yy0) {
	yygotominor.yy1->adjust_window(yymsp[-1].minor.yy0->get_termpos());
	delete yymsp[-1].minor.yy0;
    }
}
#line 2311 "queryparser/queryparser_internal.cc"
        break;
      case 48:
      case 50:
#line 1611 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"
{
    yygotominor.yy1 = yymsp[-2].minor.yy1;
    yygotominor.yy1->add_unstemmed_term(yymsp[0].minor.yy0);
    if (yymsp[-1].minor.yy0) {
	yygotominor.yy1->adjust_window(yymsp[-1].minor.yy0->get_termpos());
	delete yymsp[-1].minor.yy0;
    }
}
#line 2324 "queryparser/queryparser_internal.cc"
        break;
  }
  yygoto = yyRuleInfo[yyruleno].lhs;
  yysize = yyRuleInfo[yyruleno].nrhs;
  yypParser->yyidx -= yysize;
  yyact = yy_find_reduce_action(yypParser,yygoto);
  if( yyact < YYNSTATE ){
    yy_shift(yypParser,yyact,yygoto,&yygotominor);
  }else if( yyact == YYNSTATE + YYNRULE + 1 ){
    yy_accept(yypParser);
  }
}

/*
** The following code executes when the parse fails
*/
static void yy_parse_failed(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH;
  DEBUGLINE(QUERYPARSER, "Fail!");
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
#line 1191 "/data/home/olly/tmp/xapian-svn-snapshot/tags/1.0.2/xapian/xapian-core/queryparser/queryparser.lemony"

    // If we've not already set an error message, set a default one.
    if (!state->error) state->error = "parse error";
#line 2354 "queryparser/queryparser_internal.cc"
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  YYMINORTYPE yyminor            /* The minor type of the error token */
){
  ParseARG_FETCH;
  (void)yymajor;
  (void)yyminor;
#define TOKEN (yyminor.yy0)
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH;
  DEBUGLINE(QUERYPARSER, "Accept!");
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "ParseAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
static void Parse(
  yyParser *yypParser,         /* The parser */
  int yymajor,                 /* The major token code number */
  ParseTOKENTYPE yyminor       /* The value for the token */
  ParseARG_PDECL               /* Optional %extra_argument parameter */
){
  YYMINORTYPE yyminorunion;
  int yyact;            /* The parser action. */
  int yyendofinput;     /* True if we are at the end of input */
  int yyerrorhit = 0;   /* True if yymajor has invoked an error */

  /* (re)initialize the parser, if necessary */
  if( yypParser->yyidx<0 ){
    if( yymajor==0 ) return;
    yypParser->yyidx = 0;
    yypParser->yyerrcnt = -1;
    yypParser->yystack[0].stateno = 0;
    yypParser->yystack[0].major = 0;
  }
  yyminorunion.yy0 = yyminor;
  yyendofinput = (yymajor==0);
  ParseARG_STORE;

  DEBUGLINE(QUERYPARSER, "Input " << ParseTokenName(yymajor) << " " <<
	    (yyminor ? yyminor->name : "<<null>>"));

  do{
    yyact = yy_find_shift_action(yypParser,yymajor);
    if( yyact<YYNSTATE ){
      yy_shift(yypParser,yyact,yymajor,&yyminorunion);
      yypParser->yyerrcnt--;
      if( yyendofinput && yypParser->yyidx>=0 ){
        yymajor = 0;
      }else{
        yymajor = YYNOCODE;
      }
    }else if( yyact < YYNSTATE + YYNRULE ){
      yy_reduce(yypParser,yyact-YYNSTATE);
    }else if( yyact == YY_ERROR_ACTION ){
      int yymx;
      DEBUGLINE(QUERYPARSER, "Syntax Error!");
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".  
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
      if( yypParser->yyerrcnt<0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yymx = yypParser->yystack[yypParser->yyidx].major;
      if( yymx==YYERRORSYMBOL || yyerrorhit ){
	DEBUGLINE(QUERYPARSER, "Discard input token " << ParseTokenName(yymajor));
        yy_destructor((YYCODETYPE)yymajor,&yyminorunion);
        yymajor = YYNOCODE;
      }else{
         while(
          yypParser->yyidx >= 0 &&
          yymx != YYERRORSYMBOL &&
          (yyact = yy_find_shift_action(yypParser,YYERRORSYMBOL)) >= YYNSTATE
        ){
          yy_pop_parser_stack(yypParser);
        }
        if( yypParser->yyidx < 0 || yymajor==0 ){
          yy_destructor((YYCODETYPE)yymajor,&yyminorunion);
          yy_parse_failed(yypParser);
          yymajor = YYNOCODE;
        }else if( yymx!=YYERRORSYMBOL ){
          YYMINORTYPE u2;
          u2.YYERRSYMDT = 0;
          yy_shift(yypParser,yyact,YYERRORSYMBOL,&u2);
        }
      }
      yypParser->yyerrcnt = 3;
      yyerrorhit = 1;
#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( yypParser->yyerrcnt<=0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yypParser->yyerrcnt = 3;
      yy_destructor((YYCODETYPE)yymajor,&yyminorunion);
      if( yyendofinput ){
        yy_parse_failed(yypParser);
      }
      yymajor = YYNOCODE;
#endif
    }else{
      yy_accept(yypParser);
      yymajor = YYNOCODE;
    }
  }while( yymajor!=YYNOCODE && yypParser->yyidx>=0 );
  return;
}

// Select C++ syntax highlighting in vim editor: vim: syntax=cpp
