/** @file ortermlist.cc
 * @brief Merge two TermList objects using an OR operation.
 */
/* Copyright (C) 2007 Olly Betts
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 */

#include <config.h>

#include "omassert.h"
#include "omdebug.h"

#include "ortermlist.h"

#include <xapian/positioniterator.h>

void
OrTermList::check_started() const
{
    Assert(!left_current.empty());
    Assert(!right_current.empty());
}

OrTermList::~OrTermList()
{
    delete left;
    delete right;
}

Xapian::termcount
OrTermList::get_approx_size() const
{
    DEBUGCALL(EXPAND, Xapian::termcount, "OrTermList::get_approx_size", "");
    // This is actually the upper bound, but we only use this to order the
    // binary tree of OrTermList objects so it's probably not worth trying
    // to be more sophisticated.
    RETURN(left->get_approx_size() + right->get_approx_size());
}

void
OrTermList::accumulate_stats(Xapian::Internal::ExpandStats & stats) const
{
    DEBUGCALL(EXPAND, void, "OrTermList::accumulate_stats", "[stats&]");
    check_started();
    if (left_current <= right_current) left->accumulate_stats(stats);
    if (left_current >= right_current) right->accumulate_stats(stats);
}

string
OrTermList::get_termname() const
{
    DEBUGCALL(EXPAND, string, "OrTermList::get_termname", "");
    check_started();
    if (left_current < right_current) RETURN(left_current);
    RETURN(right_current);
}

Xapian::termcount
OrTermList::get_wdf() const
{
    DEBUGCALL(EXPAND, Xapian::termcount, "OrTermList::get_wdf", "");
    check_started();
    if (left_current < right_current) RETURN(left->get_wdf());
    if (left_current > right_current) RETURN(right->get_wdf());
    RETURN(left->get_wdf() + right->get_wdf());
}

Xapian::doccount
OrTermList::get_termfreq() const
{
    DEBUGCALL(EXPAND, Xapian::doccount, "OrTermList::get_termfreq", "");
    check_started();
    if (left_current < right_current) RETURN(left->get_termfreq());
    Assert(left_current > right_current || left->get_termfreq() == right->get_termfreq());
    RETURN(right->get_termfreq());
}

#if 0 // This method isn't actually used anywhere currently.
Xapian::termcount
OrTermList::get_collection_freq() const
{
    DEBUGCALL(EXPAND, Xapian::termcount, "OrTermList::get_collection_freq", "");
    check_started();
    if (left_current < right_current) RETURN(left->get_collection_freq());
    Assert(left_current > right_current || left->get_collection_freq() == right->get_collection_freq());
    RETURN(right->get_collection_freq());
}
#endif

// Helper function.
inline void
handle_prune(TermList *& old, TermList * result)
{
    if (result) {
	delete old;
	old = result;
    }
}

TermList *
OrTermList::next()
{
    DEBUGCALL(EXPAND, TermList *, "OrTermList::next", "");
    // If we've not started yet, both left_current and right_current will be
    // empty, so we'll take the third case below which is what we want to do to
    // get started.
    if (left_current < right_current) {
	handle_prune(left, left->next());
	if (left->at_end()) {
	    TermList *ret = right;
	    right = NULL;
	    RETURN(ret);
	}
	left_current = left->get_termname();
    } else if (left_current > right_current) {
	handle_prune(right, right->next());
	if (right->at_end()) {
	    TermList *ret = left;
	    left = NULL;
	    RETURN(ret);
	}
	right_current = right->get_termname();
    } else {
	AssertEq(left_current, right_current);
	handle_prune(left, left->next());
	handle_prune(right, right->next());
	if (left->at_end()) {
	    // right->at_end() may also be true, but our parent will deal with
	    // that.
	    TermList *ret = right;
	    right = NULL;
	    RETURN(ret);
	}
	if (right->at_end()) {
	    TermList *ret = left;
	    left = NULL;
	    RETURN(ret);
	}
	left_current = left->get_termname();
	right_current = right->get_termname();
    }
    RETURN(NULL);
}

bool
OrTermList::at_end() const
{
    DEBUGCALL(EXPAND, bool, "OrTermList::at_end", "");
    check_started();
    // next() should have pruned if either child is at_end().
    Assert(!left->at_end());
    Assert(!right->at_end());
    RETURN(false);
}

Xapian::termcount
OrTermList::positionlist_count() const
{
    Assert(false);
    return 0;
}

Xapian::PositionIterator
OrTermList::positionlist_begin() const
{
    Assert(false);
    return Xapian::PositionIterator(NULL);
}


FreqAdderOrTermList::~FreqAdderOrTermList()
{
}

Xapian::doccount
FreqAdderOrTermList::get_termfreq() const
{
    DEBUGCALL(EXPAND, Xapian::doccount, "FreqAdderOrTermList::get_termfreq", "");
    check_started();
    if (left_current < right_current) RETURN(left->get_termfreq());
    if (left_current > right_current) RETURN(right->get_termfreq());
    RETURN(left->get_termfreq() + right->get_termfreq());
}
