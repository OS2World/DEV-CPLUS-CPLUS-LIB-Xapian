/* index_utils.h - utility functions for indexing testcase data
 *
 * Copyright (C) 2005 Olly Betts
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef XAPIAN_HGUARD_INDEX_UTILS_H
#define XAPIAN_HGUARD_INDEX_UTILS_H

#include <iosfwd>
#include <xapian.h>

Xapian::Document document_from_stream(std::istream &from);
std::string munge_term(const std::string &term);

#endif /* XAPIAN_HGUARD_INDEX_UTILS_H */
