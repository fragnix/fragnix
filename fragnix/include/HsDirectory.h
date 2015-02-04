/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2001-2004
 *
 * Definitions for package `directory' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef __HSDIRECTORY_H__
#define __HSDIRECTORY_H__

// On Solaris we have to make sure _FILE_OFFSET_BITS is defined 
// before including <sys/stat.h> below, because that header
// will try and define it if it isn't already.
#include "HsFFI.h"

#include "HsDirectoryConfig.h"

// Otherwise these clash with similar definitions from other packages:
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "HsFFI.h"

/* -----------------------------------------------------------------------------
   INLINE functions.

   These functions are given as inlines here for when compiling via C,
   but we also generate static versions into the cbits library for
   when compiling to native code.
   -------------------------------------------------------------------------- */

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# else
#  define INLINE static inline
# endif
#endif

/* A size that will contain many path names, but not necessarily all
 * (PATH_MAX is not defined on systems with unlimited path length,
 * e.g. the Hurd).
 */
INLINE HsInt __hscore_long_path_size(void) {
#ifdef PATH_MAX
    return PATH_MAX;
#else
    return 4096;
#endif
}

INLINE mode_t __hscore_S_IRUSR(void) { return S_IRUSR; }
INLINE mode_t __hscore_S_IWUSR(void) { return S_IWUSR; }
INLINE mode_t __hscore_S_IXUSR(void) { return S_IXUSR; }
INLINE mode_t __hscore_S_IFDIR(void) { return S_IFDIR; }

#endif /* __HSDIRECTORY_H__ */

