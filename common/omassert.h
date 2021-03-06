/* omassert.h: Provide Assert() and similar functions
 *
 * Copyright 1999,2000,2001 BrightStation PLC
 * Copyright 2002 Ananova Ltd
 * Copyright 2002,2003,2004,2007 Olly Betts
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

// Note: we use macros to define our assertions, since with a template
// approach the location strings typically don't get thrown away by the
// compiler.

#ifndef OM_HGUARD_OMASSERT_H
#define OM_HGUARD_OMASSERT_H

// Include the definitions of the exceptions we're going to throw
#include <xapian/error.h>

// Include utility functions
#include "utils.h"

// 2nd level of stringize definition not needed for the use we put this
// to in this file (since we always use it within a macro here) but
// is required in general  (#N doesn't work outside a macro definition)
#ifndef STRINGIZE
# define STRINGIZE(N) _STRINGIZE(N)
# define _STRINGIZE(N) #N
#endif

// pull this out to improve readability and so we can change it for
// all cases easily
#define ASSERT_LOCN(a) __FILE__":"STRINGIZE(__LINE__)": "#a

#ifdef XAPIAN_DEBUG_PARANOID
// Paranoid checks, typically too expensive to include in debug versions
// for use by developers but useful to turn on when debugging OM itself

// If we want the paranoid checks, want other checks too
#ifndef XAPIAN_DEBUG
#define XAPIAN_DEBUG
#endif /* !XAPIAN_DEBUG */

// NB use an else clause to avoid dangling else damage
#define AssertParanoid(a) if (a) { } else throw Xapian::AssertionError(ASSERT_LOCN(a))
#define AssertEqParanoid(a,b) if ((a) == (b)) { } else throw Xapian::AssertionError(ASSERT_LOCN(a)" - expected equal values: had " + om_tostring(a) + " and " + om_tostring(b))
#define AssertNeParanoid(a,b) if ((a) != (b)) { } else throw Xapian::AssertionError(ASSERT_LOCN(a)" - expected different values: had " + om_tostring(a))
#else /* XAPIAN_DEBUG_PARANOID */
#define AssertParanoid(a)
#define AssertEqParanoid(a,b)
#define AssertNeParanoid(a,b)
#endif /* XAPIAN_DEBUG_PARANOID */

#ifdef XAPIAN_DEBUG
#include <math.h> // for fabs() for AssertEqDouble
#include <float.h> // for DBL_EPSILON for AssertEqDouble

// Assertions to put in debug builds
// NB put in a do{}while(0) to avoid dangling else damage
#define Assert(a) do { if (!(a)) throw Xapian::AssertionError(ASSERT_LOCN(a)); } while (0)
// Keep AssertEqDouble separate so we can use an epsilon test
#define AssertEqDouble(a,b) do { if (fabs((a) - (b)) >= DBL_EPSILON) throw Xapian::AssertionError(ASSERT_LOCN(a)" - expected equal values: had " + om_tostring(a) + " and " + om_tostring(b)); } while (0)
#define AssertEq(a,b) do { if ((a) != (b)) throw Xapian::AssertionError(ASSERT_LOCN(a)" - expected equal values: had " + om_tostring(a) + " and " + om_tostring(b)); } while (0)
#define AssertNe(a,b) do { if ((a) == (b)) throw Xapian::AssertionError(ASSERT_LOCN(a)" - expected different values: had " + om_tostring(a)); } while (0)
#else
#define Assert(a) (void)0
#define AssertEqDouble(a,b) (void)0
#define AssertEq(a,b) (void)0
#define AssertNe(a,b) (void)0
#endif

// CompileTimeAssert(expr); takes a constant expression, expr, and causes
// a compile-time error if false.  Must be used within a function, not at
// the top level (this is because we can't encode the filename, only
// the linenumber, so we can't avoid the risk of collisions between
// uses at the same line in different header files at the top level)

#define CompileTimeAssert(EXPR)\
 do{int CompileTimeCheck[(EXPR)?1:-1];(void)CompileTimeCheck;}while(0)

#endif /* OM_HGUARD_OMASSERT_H */
