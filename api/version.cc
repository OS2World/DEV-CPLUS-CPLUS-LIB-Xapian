// version.cc: Return library version information
//
// Copyright (C) 2005,2006 Olly Betts
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

#include <config.h>

#include "xapian.h"

namespace Xapian {

const char * version_string() { return XAPIAN_VERSION; }

int major_version() { return XAPIAN_MAJOR_VERSION; }

int minor_version() { return XAPIAN_MINOR_VERSION; }

int revision() { return XAPIAN_REVISION; }

// Deprecated aliases.

const char * xapian_version_string() { return XAPIAN_VERSION; }

int xapian_major_version() { return XAPIAN_MAJOR_VERSION; }

int xapian_minor_version() { return XAPIAN_MINOR_VERSION; }

int xapian_revision() { return XAPIAN_REVISION; }

}
