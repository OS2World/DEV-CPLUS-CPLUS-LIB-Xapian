/* flint_database.cc: flint database
 *
 * Copyright 1999,2000,2001 BrightStation PLC
 * Copyright 2001 Hein Ragas
 * Copyright 2002 Ananova Ltd
 * Copyright 2002,2003,2004,2005,2006,2007 Olly Betts
 * Copyright 2006 Richard Boulton
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

#include "safeerrno.h"

#include "flint_database.h"
#include "utils.h"
#include "omdebug.h"
#include "autoptr.h"
#include <xapian/error.h>
#include <xapian/valueiterator.h>

#include "contiguousalldocspostlist.h"
#include "flint_modifiedpostlist.h"
#include "flint_postlist.h"
#include "flint_alldocspostlist.h"
#include "flint_termlist.h"
#include "flint_positionlist.h"
#include "flint_utils.h"
#include "flint_record.h"
#include "flint_values.h"
#include "flint_document.h"
#include "flint_alltermslist.h"
#include "flint_lock.h"
#include "flint_spellingwordslist.h"

#include <sys/types.h>
#include "safesysstat.h"

#include <list>
#include <string>

using namespace std;
using namespace Xapian;

/* This finds the tables, opens them at consistent revisions, manages
 * determining the current and next revision numbers, and stores handles
 * to the tables.
 */
FlintDatabase::FlintDatabase(const string &flint_dir, int action,
			       unsigned int block_size)
	: db_dir(flint_dir),
	  readonly(action == XAPIAN_DB_READONLY),
	  version_file(db_dir),
	  postlist_table(db_dir, readonly),
	  positionlist_table(db_dir, readonly),
	  termlist_table(db_dir, readonly),
	  value_table(db_dir, readonly),
	  synonym_table(db_dir, readonly),
	  spelling_table(db_dir, readonly),
	  record_table(db_dir, readonly),
	  lock(db_dir + "/flintlock")
{
    DEBUGCALL(DB, void, "FlintDatabase", flint_dir << ", " << action <<
	      ", " << block_size);

    bool dbexists = database_exists();
    // open tables
    if (action == XAPIAN_DB_READONLY) {
	if (!dbexists) {
	    throw Xapian::DatabaseOpeningError("Cannot open database at `" + db_dir + "' - it does not exist");
	}
	// Can still allow searches even if recovery is needed
	open_tables_consistent();
    } else {
	if (!dbexists) {
	    // FIXME: if we allow Xapian::DB_OVERWRITE, check it here
	    if (action == Xapian::DB_OPEN) {
		throw Xapian::DatabaseOpeningError("Cannot open database at `" + db_dir + "' - it does not exist");
	    }

	    // Create the directory for the database, if it doesn't exist
	    // already.
	    bool fail = false;
	    struct stat statbuf;
	    if (stat(db_dir, &statbuf) == 0) {
		if (!S_ISDIR(statbuf.st_mode)) fail = true;
	    } else if (errno != ENOENT || mkdir(db_dir, 0755) == -1) {
		fail = true;
	    }
	    if (fail) {
		throw Xapian::DatabaseOpeningError("Cannot create directory `"
						   + db_dir + "'", errno);
	    }
	    get_database_write_lock();

	    create_and_open_tables(block_size);
	    return;
	}

	if (action == Xapian::DB_CREATE) {
	    throw Xapian::DatabaseCreateError("Can't create new database at `" +
					      db_dir + "': a database already exists and I was told "
					      "not to overwrite it");
	}

	get_database_write_lock();
	// if we're overwriting, pretend the db doesn't exist
	// FIXME: if we allow Xapian::DB_OVERWRITE, check it here
	if (action == Xapian::DB_CREATE_OR_OVERWRITE) {
	    create_and_open_tables(block_size);
	    return;
	}

	// Get latest consistent version
	open_tables_consistent();

	// Check that there are no more recent versions of tables.  If there
	// are, perform recovery by writing a new revision number to all
	// tables.
	if (record_table.get_open_revision_number() !=
	    postlist_table.get_latest_revision_number()) {
	    flint_revision_number_t new_revision = get_next_revision_number();

	    set_revision_number(new_revision);
	}
	if (record_table.get_doccount() == 0) {
	    postlist_table.set_total_length_and_lastdocid(0, postlist_table.get_lastdocid());
	}
    }
}

FlintDatabase::~FlintDatabase()
{
    DEBUGCALL(DB, void, "~FlintDatabase", "");
}

bool
FlintDatabase::database_exists() {
    DEBUGCALL(DB, bool, "FlintDatabase::database_exists", "");
    return record_table.exists() &&
	   postlist_table.exists() &&
	   termlist_table.exists();
}

void
FlintDatabase::create_and_open_tables(unsigned int block_size)
{
    DEBUGCALL(DB, void, "FlintDatabase::create_and_open_tables", "");
    //FIXME - check that database directory exists.

    // Create postlist_table first, and record_table last.  Existence of
    // record_table is considered to imply existence of the database.
    version_file.create();
    postlist_table.create_and_open(block_size);
    // The positionlist table is created lazily, but erase it in case we're
    // overwriting an existing database and it already exists.
    positionlist_table.erase();
    positionlist_table.set_block_size(block_size);

    termlist_table.create_and_open(block_size);
    // The value table is created lazily, but erase it in case we're
    // overwriting an existing database and it already exists.
    value_table.erase();
    value_table.set_block_size(block_size);

    synonym_table.create_and_open(block_size);
    spelling_table.create_and_open(block_size);
    record_table.create_and_open(block_size);

    Assert(database_exists());

    // Check consistency
    flint_revision_number_t revision = record_table.get_open_revision_number();
    if (revision != termlist_table.get_open_revision_number() ||
	revision != postlist_table.get_open_revision_number()) {
	throw Xapian::DatabaseCreateError("Newly created tables are not in consistent state");
    }
    postlist_table.set_total_length_and_lastdocid(0, 0);
}

void
FlintDatabase::open_tables_consistent()
{
    DEBUGCALL(DB, void, "FlintDatabase::open_tables_consistent", "");
    // Open record_table first, since it's the last to be written to,
    // and hence if a revision is available in it, it should be available
    // in all the other tables (unless they've moved on already).
    //
    // If we find that a table can't open the desired revision, we
    // go back and open record_table again, until record_table has
    // the same revision as the last time we opened it.

    version_file.read_and_check();
    record_table.open();
    flint_revision_number_t revision = record_table.get_open_revision_number();

    // In case the positionlist, value, synonym, and/or spelling tables don't
    // exist yet.
    unsigned int block_size = record_table.get_block_size();
    positionlist_table.set_block_size(block_size);
    value_table.set_block_size(block_size);
    synonym_table.set_block_size(block_size);
    spelling_table.set_block_size(block_size);

    bool fully_opened = false;
    int tries = 100;
    int tries_left = tries;
    while (!fully_opened && (tries_left--) > 0) {
	if (spelling_table.open(revision) &&
	    synonym_table.open(revision) &&
	    value_table.open(revision) &&
	    termlist_table.open(revision) &&
	    positionlist_table.open(revision) &&
	    postlist_table.open(revision)) {
	    // Everything now open at the same revision.
	    fully_opened = true;
	} else {
	    // Couldn't open consistent revision: two cases possible:
	    // i)   An update has completed and a second one has begun since
	    //      record was opened.  This leaves a consistent revision
	    //      available, but not the one we were trying to open.
	    // ii)  Tables have become corrupt / have no consistent revision
	    //      available.  In this case, updates must have ceased.
	    //
	    // So, we reopen the record table, and check its revision number,
	    // if it's changed we try the opening again, otherwise we give up.
	    //
	    record_table.open();
	    flint_revision_number_t newrevision =
		    record_table.get_open_revision_number();
	    if (revision == newrevision) {
		// Revision number hasn't changed - therefore a second index
		// sweep hasn't begun and the system must have failed.  Database
		// is inconsistent.
		throw Xapian::DatabaseCorruptError("Cannot open tables at consistent revisions");
	    }
	    revision = newrevision;
	}
    }

    if (!fully_opened) {
	throw Xapian::DatabaseModifiedError("Cannot open tables at stable revision - changing too fast");
    }
}

void
FlintDatabase::open_tables(flint_revision_number_t revision)
{
    DEBUGCALL(DB, void, "FlintDatabase::open_tables", revision);
    version_file.read_and_check();
    record_table.open(revision);

    // In case the positionlist, value, synonym, and/or spelling tables don't
    // exist yet.
    unsigned int block_size = record_table.get_block_size();
    positionlist_table.set_block_size(block_size);
    value_table.set_block_size(block_size);
    synonym_table.set_block_size(block_size);
    spelling_table.set_block_size(block_size);

    spelling_table.open(revision);
    synonym_table.open(revision);
    value_table.open(revision);
    termlist_table.open(revision);
    positionlist_table.open(revision);
    postlist_table.open(revision);
}

flint_revision_number_t
FlintDatabase::get_revision_number() const
{
    DEBUGCALL(DB, flint_revision_number_t, "FlintDatabase::get_revision_number", "");
    // We could use any table here, theoretically.
    RETURN(postlist_table.get_open_revision_number());
}

flint_revision_number_t
FlintDatabase::get_next_revision_number() const
{
    DEBUGCALL(DB, flint_revision_number_t, "FlintDatabase::get_next_revision_number", "");
    /* We _must_ use postlist_table here, since it is always the first
     * to be written, and hence will have the greatest available revision
     * number.
     */
    flint_revision_number_t new_revision =
	    postlist_table.get_latest_revision_number();
    new_revision += 1;
    RETURN(new_revision);
}

void
FlintDatabase::set_revision_number(flint_revision_number_t new_revision)
{
    DEBUGCALL(DB, void, "FlintDatabase::set_revision_number", new_revision);
    postlist_table.commit(new_revision);
    positionlist_table.commit(new_revision);
    termlist_table.commit(new_revision);
    value_table.commit(new_revision);
    synonym_table.commit(new_revision);
    spelling_table.commit(new_revision);
    record_table.commit(new_revision);
}

void
FlintDatabase::reopen()
{
    DEBUGCALL(DB, void, "FlintDatabase::reopen", "");
    if (readonly) {
	open_tables_consistent();
    }
}

void
FlintDatabase::get_database_write_lock()
{
    DEBUGCALL(DB, void, "FlintDatabase::get_database_write_lock", "");
    FlintLock::reason why = lock.lock(true);
    if (why != FlintLock::SUCCESS) {
	string msg("Unable to acquire database write lock on ");
	msg += db_dir;
	if (why == FlintLock::INUSE) {
	    msg += ": already locked";
	} else if (why == FlintLock::UNSUPPORTED) {
	    msg += ": locking probably not supported by this FS";
	}
	throw Xapian::DatabaseLockError(msg);
    }
}

void
FlintDatabase::apply()
{
    DEBUGCALL(DB, void, "FlintDatabase::apply", "");
    if (!postlist_table.is_modified() &&
	!positionlist_table.is_modified() &&
	!termlist_table.is_modified() &&
	!value_table.is_modified() &&
	!synonym_table.is_modified() &&
	!spelling_table.is_modified() &&
	!record_table.is_modified()) {
	return;
    }

    flint_revision_number_t old_revision = get_revision_number();
    flint_revision_number_t new_revision = get_next_revision_number();

    try {
	set_revision_number(new_revision);
    } catch (...) {
	// Modifications failed.  Wipe all the modifications from memory.
	try {
	    // Discard any buffered changes and reinitialised cached values
	    // from the table.
	    cancel();

	    // Reopen tables with old revision number.
	    open_tables(old_revision);

	    // Increase revision numbers to new revision number plus one,
	    // writing increased numbers to all tables.
	    ++new_revision;
	    set_revision_number(new_revision);
	} catch (const Xapian::Error &e) {
	    throw Xapian::DatabaseError("Modifications failed, and cannot set consistent table revision numbers: " + e.get_msg());
	}
	throw;
    }
}

void
FlintDatabase::cancel()
{
    DEBUGCALL(DB, void, "FlintDatabase::cancel", "");
    postlist_table.cancel();
    positionlist_table.cancel();
    termlist_table.cancel();
    value_table.cancel();
    synonym_table.cancel();
    spelling_table.cancel();
    record_table.cancel();
}

Xapian::doccount
FlintDatabase::get_doccount() const
{
    DEBUGCALL(DB, Xapian::doccount, "FlintDatabase::get_doccount", "");
    RETURN(record_table.get_doccount());
}

Xapian::docid
FlintDatabase::get_lastdocid() const
{
    DEBUGCALL(DB, Xapian::docid, "FlintDatabase::get_lastdocid", "");
    RETURN(postlist_table.get_lastdocid());
}

Xapian::doclength
FlintDatabase::get_avlength() const
{
    DEBUGCALL(DB, Xapian::doclength, "FlintDatabase::get_avlength", "");
    Xapian::doccount docs = record_table.get_doccount();
    if (docs == 0) RETURN(0);
    RETURN(double(postlist_table.get_total_length()) / docs);
}

Xapian::doclength
FlintDatabase::get_doclength(Xapian::docid did) const
{
    DEBUGCALL(DB, Xapian::doclength, "FlintDatabase::get_doclength", did);
    Assert(did != 0);

    FlintTermList termlist(0, &termlist_table, did, 0);
    RETURN(termlist.get_doclength());
}

Xapian::doccount
FlintDatabase::get_termfreq(const string & tname) const
{
    DEBUGCALL(DB, Xapian::doccount, "FlintDatabase::get_termfreq", tname);
    Assert(!tname.empty());

    RETURN(postlist_table.get_termfreq(tname));
}

Xapian::termcount
FlintDatabase::get_collection_freq(const string & tname) const
{
    DEBUGCALL(DB, Xapian::termcount, "FlintDatabase::get_collection_freq", tname);
    Assert(!tname.empty());

    RETURN(postlist_table.get_collection_freq(tname));
}

bool
FlintDatabase::term_exists(const string & tname) const
{
    DEBUGCALL(DB, bool, "FlintDatabase::term_exists", tname);
    Assert(!tname.empty());
    // FIXME: nasty C&P from backends/flint/flint_postlist.cc
    string key = pack_string_preserving_sort(tname);
    return postlist_table.key_exists(key);
}

bool
FlintDatabase::has_positions() const
{
    return positionlist_table.get_entry_count() > 0;
}


LeafPostList *
FlintDatabase::open_post_list(const string& tname) const
{
    DEBUGCALL(DB, LeafPostList *, "FlintDatabase::open_post_list", tname);
    Xapian::Internal::RefCntPtr<const FlintDatabase> ptrtothis(this);

    if (tname.empty()) {
	Xapian::doccount doccount = get_doccount();
    	if (postlist_table.get_lastdocid() == doccount) {
	    RETURN(new ContiguousAllDocsPostList(ptrtothis, doccount));
	}
	RETURN(new FlintAllDocsPostList(ptrtothis, &termlist_table, doccount));
    }

    RETURN(new FlintPostList(ptrtothis,
			     &postlist_table,
			     &positionlist_table,
			     tname));
}

TermList *
FlintDatabase::open_term_list(Xapian::docid did) const
{
    DEBUGCALL(DB, TermList *, "FlintDatabase::open_term_list", did);
    Assert(did != 0);

    Xapian::Internal::RefCntPtr<const FlintDatabase> ptrtothis(this);
    RETURN(new FlintTermList(ptrtothis, &termlist_table, did, get_doccount()));
}

Xapian::Document::Internal *
FlintDatabase::open_document(Xapian::docid did, bool lazy) const
{
    DEBUGCALL(DB, Xapian::Document::Internal *, "FlintDatabase::open_document",
	      did << ", " << lazy);
    Assert(did != 0);

    Xapian::Internal::RefCntPtr<const FlintDatabase> ptrtothis(this);
    RETURN(new FlintDocument(ptrtothis,
			      &value_table,
			      &record_table,
			      did, lazy));
}

PositionList *
FlintDatabase::open_position_list(Xapian::docid did,
				   const string & tname) const
{
    Assert(did != 0);

    AutoPtr<FlintPositionList> poslist(new FlintPositionList());
    poslist->read_data(&positionlist_table, did, tname);
    if (poslist->get_size() == 0) {
	// Check that term / document combination exists.
	// If the doc doesn't exist, this will throw Xapian::DocNotFoundError:
	AutoPtr<TermList> tl(open_term_list(did));
	tl->skip_to(tname);
	if (tl->at_end() || tl->get_termname() != tname)
	    throw Xapian::RangeError("Can't open position list: requested term is not present in document.");
    }

    return poslist.release();
}

TermList *
FlintDatabase::open_allterms(const string & prefix) const
{
    DEBUGCALL(DB, TermList *, "FlintDatabase::open_allterms", "");
    RETURN(new FlintAllTermsList(Xapian::Internal::RefCntPtr<const FlintDatabase>(this),
				 &postlist_table, prefix));
}

TermList *
FlintDatabase::open_spelling_termlist(const string & word) const
{
    return spelling_table.open_termlist(word);
}

TermList *
FlintDatabase::open_spelling_wordlist() const
{
    FlintCursor * cursor = spelling_table.cursor_get();
    if (!cursor) return NULL;
    return new FlintSpellingWordsList(Xapian::Internal::RefCntPtr<const FlintDatabase>(this),
				      cursor);
}

Xapian::doccount
FlintDatabase::get_spelling_frequency(const string & word) const
{
    return spelling_table.get_word_frequency(word);
}

TermList *
FlintDatabase::open_synonym_termlist(const string & term) const
{
    return synonym_table.open_termlist(term);
}

TermList *
FlintDatabase::open_synonym_keylist(const string & prefix) const
{
    FlintCursor * cursor = synonym_table.cursor_get();
    if (!cursor) return NULL;
    return new FlintSynonymTermList(Xapian::Internal::RefCntPtr<const FlintDatabase>(this),
				    cursor, synonym_table.get_entry_count(),
				    prefix);
}

///////////////////////////////////////////////////////////////////////////

size_t FlintWritableDatabase::flush_threshold = 0;

FlintWritableDatabase::FlintWritableDatabase(const string &dir, int action,
					       int block_size)
	: freq_deltas(),
	  doclens(),
	  mod_plists(),
	  database_ro(dir, action, block_size),
	  total_length(database_ro.postlist_table.get_total_length()),
	  lastdocid(database_ro.get_lastdocid()),
	  changes_made(0)
{
    DEBUGCALL(DB, void, "FlintWritableDatabase", dir << ", " << action << ", "
	      << block_size);
    if (flush_threshold == 0) {
	const char *p = getenv("XAPIAN_FLUSH_THRESHOLD");
	if (p) flush_threshold = atoi(p);
    }
    if (flush_threshold == 0) flush_threshold = 10000;
}

FlintWritableDatabase::~FlintWritableDatabase()
{
    DEBUGCALL(DB, void, "~FlintWritableDatabase", "");
    dtor_called();
}

void
FlintWritableDatabase::flush()
{
    if (transaction_active())
	throw Xapian::InvalidOperationError("Can't flush during a transaction");
    if (changes_made ||
	database_ro.spelling_table.is_modified() ||
	database_ro.synonym_table.is_modified()) {
	do_flush_const();
    }
}

void
FlintWritableDatabase::do_flush_const() const
{
    DEBUGCALL(DB, void, "FlintWritableDatabase::do_flush_const", "");

    if (changes_made) {
	database_ro.postlist_table.merge_changes(mod_plists, doclens,
						 freq_deltas);

	// Update the total document length and last used docid.
	database_ro.postlist_table.set_total_length_and_lastdocid(total_length,
								  lastdocid);
    }
    database_ro.apply();
    freq_deltas.clear();
    doclens.clear();
    mod_plists.clear();
    changes_made = 0;
}

Xapian::docid
FlintWritableDatabase::add_document(const Xapian::Document & document)
{
    DEBUGCALL(DB, Xapian::docid,
	      "FlintWritableDatabase::add_document", document);
    // Make sure the docid counter doesn't overflow.
    if (lastdocid == Xapian::docid(-1))
	throw Xapian::DatabaseError("Run out of docids - you'll have to use copydatabase to eliminate any gaps before you can add more documents");
    // Use the next unused document ID.
    RETURN(add_document_(++lastdocid, document));
}

Xapian::docid
FlintWritableDatabase::add_document_(Xapian::docid did,
				     const Xapian::Document & document)
{
    DEBUGCALL(DB, Xapian::docid,
	      "FlintWritableDatabase::add_document_", did << ", " << document);
    Assert(did != 0);
    try {
	// Add the record using that document ID.
	database_ro.record_table.replace_record(document.get_data(), did);

	// Set the values.
	{
	    Xapian::ValueIterator value = document.values_begin();
	    Xapian::ValueIterator value_end = document.values_end();
	    for ( ; value != value_end; ++value) {
		database_ro.value_table.add_value(*value, did,
						  value.get_valueno());
	    }
	}

	flint_doclen_t new_doclen = 0;
	{
	    Xapian::TermIterator term = document.termlist_begin();
	    Xapian::TermIterator term_end = document.termlist_end();
	    for ( ; term != term_end; ++term) {
		termcount wdf = term.get_wdf();
		// Calculate the new document length
		new_doclen += wdf;

		string tname = *term;
		map<string, pair<termcount_diff, termcount_diff> >::iterator i;
		i = freq_deltas.find(tname);
		if (i == freq_deltas.end()) {
		    freq_deltas.insert(make_pair(tname, make_pair(1, termcount_diff(wdf))));
		} else {
		    ++i->second.first;
		    i->second.second += wdf;
		}

		// Add did to tname's postlist
		map<string, map<docid, pair<char, termcount> > >::iterator j;
		j = mod_plists.find(tname);
		if (j == mod_plists.end()) {
		    map<docid, pair<char, termcount> > m;
		    j = mod_plists.insert(make_pair(tname, m)).first;
		}
		Assert(j->second.find(did) == j->second.end());
		j->second.insert(make_pair(did, make_pair('A', wdf)));

		if (term.positionlist_begin() != term.positionlist_end()) {
		    database_ro.positionlist_table.set_positionlist(
			did, tname,
			term.positionlist_begin(), term.positionlist_end());
		}
	    }
	}
	DEBUGLINE(DB, "Calculated doclen for new document " << did << " as " << new_doclen);

	// Set the termlist
	database_ro.termlist_table.set_entries(did,
		document.termlist_begin(), document.termlist_end(),
		new_doclen, false);

	// Set the new document length
	Assert(doclens.find(did) == doclens.end());
	doclens[did] = new_doclen;
	total_length += new_doclen;
    } catch (...) {
	// If an error occurs while adding a document, or doing any other
	// transaction, the modifications so far must be cleared before
	// returning control to the user - otherwise partial modifications will
	// persist in memory, and eventually get written to disk.
	cancel();
	throw;
    }

    // FIXME: this should be done by checking memory usage, not the number of
    // changes.
    // We could also look at:
    // * mod_plists.size()
    // * doclens.size()
    // * freq_deltas.size()
    //
    // cout << "+++ mod_plists.size() " << mod_plists.size() <<
    //     ", doclens.size() " << doclens.size() <<
    //	   ", freq_deltas.size() " << freq_deltas.size() << endl;
    if (++changes_made >= flush_threshold && !transaction_active())
	do_flush_const();

    RETURN(did);
}

void
FlintWritableDatabase::delete_document(Xapian::docid did)
{
    DEBUGCALL(DB, void, "FlintWritableDatabase::delete_document", did);
    Assert(did != 0);

    // Remove the record.  If this fails, just propagate the exception since
    // the state should still be consistent (most likely it's
    // DocNotFoundError).
    database_ro.record_table.delete_record(did);

    try {
	// Remove the values
	database_ro.value_table.delete_all_values(did);

	// OK, now add entries to remove the postings in the underlying record.
	Xapian::Internal::RefCntPtr<const FlintWritableDatabase> ptrtothis(this);
	FlintTermList termlist(ptrtothis,
				&database_ro.termlist_table,
				did, get_doccount());

	total_length -= termlist.get_doclength();

	termlist.next();
	while (!termlist.at_end()) {
	    string tname = termlist.get_termname();
	    database_ro.positionlist_table.delete_positionlist(did, tname);
	    termcount wdf = termlist.get_wdf();

	    map<string, pair<termcount_diff, termcount_diff> >::iterator i;
	    i = freq_deltas.find(tname);
	    if (i == freq_deltas.end()) {
		freq_deltas.insert(make_pair(tname, make_pair(-1, -termcount_diff(wdf))));
	    } else {
		--i->second.first;
		i->second.second -= wdf;
	    }

	    // Remove did from tname's postlist
	    map<string, map<docid, pair<char, termcount> > >::iterator j;
	    j = mod_plists.find(tname);
	    if (j == mod_plists.end()) {
		map<docid, pair<char, termcount> > m;
		j = mod_plists.insert(make_pair(tname, m)).first;
	    }

	    map<docid, pair<char, termcount> >::iterator k;
	    k = j->second.find(did);
	    if (k == j->second.end()) {
		j->second.insert(make_pair(did, make_pair('D', 0u)));
	    } else {
		// Deleting a document we added/modified since the last flush.
		k->second = make_pair('D', 0u);
	    }

	    termlist.next();
	}

	// Remove the termlist.
	database_ro.termlist_table.delete_termlist(did);

	// Remove the new doclength.
	doclens.erase(did);
    } catch (...) {
	// If an error occurs while deleting a document, or doing any other
	// transaction, the modifications so far must be cleared before
	// returning control to the user - otherwise partial modifications will
	// persist in memory, and eventually get written to disk.
	cancel();
	throw;
    }

    if (++changes_made >= flush_threshold && !transaction_active())
	do_flush_const();
}

void
FlintWritableDatabase::replace_document(Xapian::docid did,
					const Xapian::Document & document)
{
    DEBUGCALL(DB, void, "FlintWritableDatabase::replace_document", did << ", " << document);
    Assert(did != 0);

    try {
	if (did > lastdocid) {
	    lastdocid = did;
	    // If this docid is above the highwatermark, then we can't be
	    // replacing an existing document.
	    (void)add_document_(did, document);
	    return;
	}

	// OK, now add entries to remove the postings in the underlying record.
	Xapian::Internal::RefCntPtr<const FlintWritableDatabase> ptrtothis(this);
	FlintTermList termlist(ptrtothis,
				&database_ro.termlist_table,
				did, get_doccount());

	termlist.next();
	while (!termlist.at_end()) {
	    string tname = termlist.get_termname();
	    termcount wdf = termlist.get_wdf();

	    map<string, pair<termcount_diff, termcount_diff> >::iterator i;
	    i = freq_deltas.find(tname);
	    if (i == freq_deltas.end()) {
		freq_deltas.insert(make_pair(tname, make_pair(-1, -termcount_diff(wdf))));
	    } else {
		--i->second.first;
		i->second.second -= wdf;
	    }

	    // Remove did from tname's postlist
	    map<string, map<docid, pair<char, termcount> > >::iterator j;
	    j = mod_plists.find(tname);
	    if (j == mod_plists.end()) {
		map<docid, pair<char, termcount> > m;
		j = mod_plists.insert(make_pair(tname, m)).first;
	    }

	    map<docid, pair<char, termcount> >::iterator k;
	    k = j->second.find(did);
	    if (k == j->second.end()) {
		j->second.insert(make_pair(did, make_pair('D', 0u)));
	    } else {
		// Modifying a document we added/modified since the last flush.
		k->second = make_pair('D', 0u);
	    }

	    termlist.next();
	}

	total_length -= termlist.get_doclength();

	// Replace the record
	database_ro.record_table.replace_record(document.get_data(), did);

	// FIXME: we read the values delete them and then replace in case
	// they come from where they're going!  Better to ask Document
	// nicely and shortcut in this case!
	{
	    list<pair<string, Xapian::valueno> > tmp;
	    Xapian::ValueIterator value = document.values_begin();
	    Xapian::ValueIterator value_end = document.values_end();
	    for ( ; value != value_end; ++value) {
		tmp.push_back(make_pair(*value, value.get_valueno()));
	    }
//	    database_ro.value_table.add_value(*value, did, value.get_valueno());

	    // Replace the values.
	    database_ro.value_table.delete_all_values(did);

	    // Set the values.
	    list<pair<string, Xapian::valueno> >::const_iterator i;
	    for (i = tmp.begin(); i != tmp.end(); ++i) {
		database_ro.value_table.add_value(i->first, did, i->second);
	    }
	}

	flint_doclen_t new_doclen = 0;
	{
	    Xapian::TermIterator term = document.termlist_begin();
	    Xapian::TermIterator term_end = document.termlist_end();
	    for ( ; term != term_end; ++term) {
		// Calculate the new document length
		termcount wdf = term.get_wdf();
		new_doclen += wdf;

		string tname = *term;
		map<string, pair<termcount_diff, termcount_diff> >::iterator i;
		i = freq_deltas.find(tname);
		if (i == freq_deltas.end()) {
		    freq_deltas.insert(make_pair(tname, make_pair(1, termcount_diff(wdf))));
		} else {
		    ++i->second.first;
		    i->second.second += wdf;
		}

		// Add did to tname's postlist
		map<string, map<docid, pair<char, termcount> > >::iterator j;
		j = mod_plists.find(tname);
		if (j == mod_plists.end()) {
		    map<docid, pair<char, termcount> > m;
		    j = mod_plists.insert(make_pair(tname, m)).first;
		}
		map<docid, pair<char, termcount> >::iterator k;
		k = j->second.find(did);
		if (k != j->second.end()) {
		    Assert(k->second.first == 'D');
		    k->second.first = 'M';
		    k->second.second = wdf;
		} else {
		    j->second.insert(make_pair(did, make_pair('A', wdf)));
		}

		PositionIterator it = term.positionlist_begin();
		PositionIterator it_end = term.positionlist_end();
		if (it != it_end) {
		    database_ro.positionlist_table.set_positionlist(
			did, tname, it, it_end);
		} else {
		    database_ro.positionlist_table.delete_positionlist(did, tname);
		}
	    }
	}
	DEBUGLINE(DB, "Calculated doclen for replacement document " << did << " as " << new_doclen);

	// Set the termlist
	database_ro.termlist_table.set_entries(did,
		document.termlist_begin(), document.termlist_end(),
		new_doclen, false);

	// Set the new document length
	doclens[did] = new_doclen;
	total_length += new_doclen;
    } catch (const Xapian::DocNotFoundError &) {
	(void)add_document_(did, document);
	return;
    } catch (...) {
	// If an error occurs while replacing a document, or doing any other
	// transaction, the modifications so far must be cleared before
	// returning control to the user - otherwise partial modifications will
	// persist in memory, and eventually get written to disk.
	cancel();
	throw;
    }

    if (++changes_made >= flush_threshold && !transaction_active())
	do_flush_const();
}

Xapian::doccount
FlintWritableDatabase::get_doccount() const
{
    DEBUGCALL(DB, Xapian::doccount, "FlintWritableDatabase::get_doccount", "");
    RETURN(database_ro.get_doccount());
}

Xapian::docid
FlintWritableDatabase::get_lastdocid() const
{
    DEBUGCALL(DB, Xapian::docid, "FlintWritableDatabase::get_lastdocid", "");
    RETURN(lastdocid);
}

Xapian::doclength
FlintWritableDatabase::get_avlength() const
{
    DEBUGCALL(DB, Xapian::doclength, "FlintWritableDatabase::get_avlength", "");
    Xapian::doccount docs = database_ro.get_doccount();
    if (docs == 0) RETURN(0);
    RETURN(double(total_length) / docs);
}

Xapian::doclength
FlintWritableDatabase::get_doclength(Xapian::docid did) const
{
    DEBUGCALL(DB, Xapian::doclength, "FlintWritableDatabase::get_doclength", did);
    map<docid, termcount>::const_iterator i = doclens.find(did);
    if (i != doclens.end()) RETURN(i->second);

    RETURN(database_ro.get_doclength(did));
}

Xapian::doccount
FlintWritableDatabase::get_termfreq(const string & tname) const
{
    DEBUGCALL(DB, Xapian::doccount, "FlintWritableDatabase::get_termfreq", tname);
    Xapian::doccount termfreq = database_ro.get_termfreq(tname);
    map<string, pair<termcount_diff, termcount_diff> >::const_iterator i;
    i = freq_deltas.find(tname);
    if (i != freq_deltas.end()) termfreq += i->second.first;
    RETURN(termfreq);
}

Xapian::termcount
FlintWritableDatabase::get_collection_freq(const string & tname) const
{
    DEBUGCALL(DB, Xapian::termcount, "FlintWritableDatabase::get_collection_freq", tname);
    Xapian::termcount collfreq = database_ro.get_collection_freq(tname);

    map<string, pair<termcount_diff, termcount_diff> >::const_iterator i;
    i = freq_deltas.find(tname);
    if (i != freq_deltas.end()) collfreq += i->second.second;

    RETURN(collfreq);
}

bool
FlintWritableDatabase::term_exists(const string & tname) const
{
    DEBUGCALL(DB, bool, "FlintWritableDatabase::term_exists", tname);
    RETURN(get_termfreq(tname) != 0);
}

bool
FlintWritableDatabase::has_positions() const
{
    return database_ro.has_positions();
}


LeafPostList *
FlintWritableDatabase::open_post_list(const string& tname) const
{
    DEBUGCALL(DB, LeafPostList *, "FlintWritableDatabase::open_post_list", tname);
    Xapian::Internal::RefCntPtr<const FlintWritableDatabase> ptrtothis(this);

    if (tname.empty()) {
	Xapian::doccount doccount = get_doccount();
    	if (lastdocid == doccount) {
	    RETURN(new ContiguousAllDocsPostList(ptrtothis, doccount));
	}
	RETURN(new FlintAllDocsPostList(ptrtothis,
					&database_ro.termlist_table,
					doccount));
    }

    map<string, map<docid, pair<char, termcount> > >::const_iterator j;
    j = mod_plists.find(tname);
    if (j != mod_plists.end()) {
	// We've got buffered changes to this term's postlist, so we need to
	// use a FlintModifiedPostList.
	RETURN(new FlintModifiedPostList(ptrtothis,
				 &database_ro.postlist_table,
				 &database_ro.positionlist_table,
				 tname,
				 j->second));
    }

    RETURN(new FlintPostList(ptrtothis,
			     &database_ro.postlist_table,
			     &database_ro.positionlist_table,
			     tname));
}

TermList *
FlintWritableDatabase::open_term_list(Xapian::docid did) const
{
    DEBUGCALL(DB, TermList *, "FlintWritableDatabase::open_term_list", did);
    Assert(did != 0);

    Xapian::Internal::RefCntPtr<const FlintWritableDatabase> ptrtothis(this);
    RETURN(new FlintTermList(ptrtothis, &database_ro.termlist_table, did,
			     get_doccount()));
}

Xapian::Document::Internal *
FlintWritableDatabase::open_document(Xapian::docid did, bool lazy) const
{
    DEBUGCALL(DB, Xapian::Document::Internal *, "FlintWritableDatabase::open_document",
	      did << ", " << lazy);
    Assert(did != 0);

    Xapian::Internal::RefCntPtr<const FlintWritableDatabase> ptrtothis(this);
    RETURN(new FlintDocument(ptrtothis,
			     &database_ro.value_table,
			     &database_ro.record_table,
			     did, lazy));
}

PositionList *
FlintWritableDatabase::open_position_list(Xapian::docid did,
				   const string & tname) const
{
    Assert(did != 0);

    AutoPtr<FlintPositionList> poslist(new FlintPositionList());
    poslist->read_data(&database_ro.positionlist_table, did, tname);
    if (poslist->get_size() == 0) {
	// Check that term / document combination exists.
	// If the doc doesn't exist, this will throw Xapian::DocNotFoundError:
	AutoPtr<TermList> tl(open_term_list(did));
	tl->skip_to(tname);
	if (tl->at_end() || tl->get_termname() != tname)
	    throw Xapian::RangeError("Can't open position list: requested term is not present in document.");
    }

    return poslist.release();
}

TermList *
FlintWritableDatabase::open_allterms(const string & prefix) const
{
    DEBUGCALL(DB, TermList *, "FlintWritableDatabase::open_allterms", "");
    if (transaction_active())
	throw Xapian::UnimplementedError("Can't open allterms iterator during a transaction");
    // If there are changes, terms may have been added or removed, and so we
    // need to flush.
    if (changes_made) do_flush_const();
    RETURN(new FlintAllTermsList(Xapian::Internal::RefCntPtr<const FlintWritableDatabase>(this),
				 &database_ro.postlist_table, prefix));
}

void
FlintWritableDatabase::cancel()
{
    database_ro.cancel();
    total_length = database_ro.postlist_table.get_total_length();
    lastdocid = database_ro.get_lastdocid();
    freq_deltas.clear();
    doclens.clear();
    mod_plists.clear();
    changes_made = 0;
}

void
FlintWritableDatabase::add_spelling(const string & word,
				    Xapian::termcount freqinc) const
{
    database_ro.spelling_table.add_word(word, freqinc);
}

void
FlintWritableDatabase::remove_spelling(const string & word,
				       Xapian::termcount freqdec) const
{
    database_ro.spelling_table.remove_word(word, freqdec);
}

TermList *
FlintWritableDatabase::open_spelling_termlist(const string & word) const
{
    return database_ro.spelling_table.open_termlist(word);
}

TermList *
FlintWritableDatabase::open_spelling_wordlist() const
{
    database_ro.spelling_table.merge_changes();
    FlintCursor * cursor = database_ro.spelling_table.cursor_get();
    if (!cursor) return NULL;
    return new FlintSpellingWordsList(Xapian::Internal::RefCntPtr<const FlintWritableDatabase>(this),
				      cursor);
}

Xapian::doccount
FlintWritableDatabase::get_spelling_frequency(const string & word) const
{
    return database_ro.spelling_table.get_word_frequency(word);
}

TermList *
FlintWritableDatabase::open_synonym_termlist(const string & term) const
{
    return database_ro.synonym_table.open_termlist(term);
}

TermList *
FlintWritableDatabase::open_synonym_keylist(const string & prefix) const
{
    database_ro.synonym_table.merge_changes();
    FlintCursor * cursor = database_ro.synonym_table.cursor_get();
    if (!cursor) return NULL;
    return new FlintSynonymTermList(Xapian::Internal::RefCntPtr<const FlintWritableDatabase>(this),
				    cursor,
				    database_ro.synonym_table.get_entry_count(),
				    prefix);
}

void
FlintWritableDatabase::add_synonym(const string & term,
				   const string & synonym) const
{
    database_ro.synonym_table.add_synonym(term, synonym);
}

void
FlintWritableDatabase::remove_synonym(const string & term,
				      const string & synonym) const
{
    database_ro.synonym_table.remove_synonym(term, synonym);
}

void
FlintWritableDatabase::clear_synonyms(const string & term) const
{
    database_ro.synonym_table.clear_synonyms(term);
}
