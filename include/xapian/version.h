// version.h: Define preprocesor symbols for the library version.
// If using GCC, also check the C++ ABI version is compatible with that used
// to build the library.
//
// Copyright (C) 2002,2004,2005,2006,2007 Olly Betts
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

#ifndef XAPIAN_INCLUDED_VERSION_H
#define XAPIAN_INCLUDED_VERSION_H

#ifdef __GNUC__
#if !defined(__GXX_ABI_VERSION) || __GXX_ABI_VERSION != 102
#error The C++ ABI version of compiler you are using does not match
#error that of the compiler used to build the library. The versions
#error must match or your program will not work correctly.
#error The Xapian library was built with g++ 3.3.5
#endif

#endif

#define XAPIAN_VERSION "1.0.2"
#define XAPIAN_MAJOR_VERSION 1
#define XAPIAN_MINOR_VERSION 0
#define XAPIAN_REVISION 2

#define XAPIAN_HAS_FLINT_BACKEND 1
/* #undef XAPIAN_HAS_QUARTZ_BACKEND */
#define XAPIAN_HAS_INMEMORY_BACKEND 1
/* #undef XAPIAN_HAS_REMOTE_BACKEND */

#endif /* XAPIAN_INCLUDED_VERSION_H */
