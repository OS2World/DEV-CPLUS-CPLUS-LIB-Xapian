/* stats.cc
 *
 * Copyright 1999,2000,2001 BrightStation PLC
 * Copyright 2002 Ananova Ltd
 * Copyright 2002,2003,2004,2006 Olly Betts
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

#include "stats.h"
#include "omassert.h"
#include "omdebug.h"

#include <xapian/version.h> // For XAPIAN_HAS_REMOTE_BACKEND

void
StatsGatherer::add_child(Xapian::Weight::Internal *source) {
    DEBUGCALL(MATCH, void, "StatsGatherer::add_child", source);
    Assert(have_gathered == false);
    sources.insert(source);
}

void
StatsGatherer::remove_child(Xapian::Weight::Internal *source) {
    DEBUGCALL(MATCH, void, "StatsGatherer::remove_child", source);
    // If have_gathered is true, the stats will be wrong, but we just
    // continue as best we can.
    sources.erase(source);
}

void
StatsGatherer::contrib_stats(const Stats & extra_stats)
{
    DEBUGCALL(MATCH, void, "StatsGatherer::contrib_stats", "[extra_stats]");
    Assert(have_gathered == false);
    total_stats += extra_stats;
}

const Stats *
LocalStatsGatherer::get_stats() const
{
    DEBUGCALL(MATCH, Stats *, "LocalStatsGatherer::get_stats", "");
    if(!have_gathered) {
	for (std::set<Xapian::Weight::Internal *>::iterator i = sources.begin();
	     i != sources.end();
	     ++i) {
	    (*i)->contrib_my_stats();
	}
	have_gathered = true;
    }

    RETURN((&total_stats));
}

#ifdef XAPIAN_HAS_REMOTE_BACKEND

#include "networkstats.h"
#include "remote-database.h"
#include "remoteserver.h"

NetworkStatsGatherer::NetworkStatsGatherer(RemoteServer *nserv_)
	: have_global_stats(false), nserv(nserv_)
{
    DEBUGCALL(MATCH, void, "NetworkStatsGatherer", nserv_);
}

const Stats *
NetworkStatsGatherer::get_stats() const
{
    DEBUGCALL(MATCH, Stats *, "NetworkStatsGatherer::get_stats", "");
    fetch_local_stats();
    fetch_global_stats();

    RETURN(&total_stats);
}

void
NetworkStatsGatherer::fetch_local_stats() const
{
    DEBUGCALL(MATCH, void, "NetworkStatsGatherer::fetch_local_stats", "");
    if (!have_gathered) {
	for (std::set<Xapian::Weight::Internal *>::iterator i = sources.begin();
	     i != sources.end();
	     ++i) {
	    (*i)->contrib_my_stats();
	}
	have_gathered = true;
    }
}

Stats
NetworkStatsGatherer::get_local_stats() const
{
    DEBUGCALL(MATCH, Stats, "NetworkStatsGatherer::get_local_stats", "");
    fetch_local_stats();
    return total_stats;
}

void
NetworkStatsGatherer::fetch_global_stats() const
{
    DEBUGCALL(MATCH, void, "NetworkStatsGatherer::fetch_global_stats", "");
    Assert(have_gathered);

    total_stats = nserv->get_global_stats();

    have_global_stats = true;
}

void
NetworkStatsSource::contrib_my_stats()
{
    DEBUGCALL(MATCH, void, "NetworkStatsSource::contrib_my_stats", "");
    Assert(have_remote_stats);
    gatherer->contrib_stats(my_stats);
}

void
NetworkStatsSource::take_remote_stats(Stats stats)
{
    DEBUGCALL(MATCH, void, "NetworkStatsSource::take_remote_stats", "[stats]");
    my_stats = stats;
    have_remote_stats = true;
}

#endif /* XAPIAN_HAS_REMOTE_BACKEND */

LocalStatsSource::LocalStatsSource(StatsGatherer * gatherer_)
	: Xapian::Weight::Internal(gatherer_)
{
    DEBUGCALL(MATCH, void, "LocalStatsSource", gatherer_);
}

LocalStatsSource::~LocalStatsSource()
{
    DEBUGCALL(MATCH, void, "~LocalStatsSource", "");
}

void
Xapian::Weight::Internal::perform_request() const
{
    DEBUGCALL(MATCH, void, "Xapian::Weight::Internal::perform_request", "");
    Assert(total_stats == 0);
    total_stats = gatherer->get_stats();
    Assert(total_stats != 0);

#ifdef XAPIAN_DEBUG_VERBOSE
    DEBUGLINE(WTCALC, "Xapian::Weight::Internal::perform_request(): stats are:");
    DEBUGLINE(WTCALC, "  collection_size = " << total_stats->collection_size);
    DEBUGLINE(WTCALC, "  rset_size = "       << total_stats->rset_size);
    DEBUGLINE(WTCALC, "  average_length = "  << total_stats->average_length);

    map<string, Xapian::doccount>::const_iterator i;
    for (i = total_stats->termfreq.begin();
	 i != total_stats->termfreq.end(); i++) {
	DEBUGLINE(WTCALC, "  termfreq of `" << i->first <<
		  "'\tis " << i->second);
    }
    for (i = total_stats->reltermfreq.begin();
	 i != total_stats->reltermfreq.end(); i++) {
	DEBUGLINE(WTCALC, "  reltermfreq of `" << i->first <<
		 "'\tis " << i->second);
    }
#endif /* XAPIAN_DEBUG_VERBOSE */
}

