#include "config.h"

#if IS_LINUX
/* Linux cheats AC_CHECK_FUNCS(strptime_l), sigh. */
#define THREAD_SAFE 0
#define _XOPEN_SOURCE
#define _BSD_SOURCE
#elif HAVE_STRPTIME_L
#define THREAD_SAFE 1
#define _GNU_SOURCE
#else
#define THREAD_SAFE 0
#endif

#include <string.h>
#include <time.h>
#include <locale.h>

#if THREAD_SAFE
#if HAVE_XLOCALE_H
#include <xlocale.h>
#endif

locale_t c_locale = NULL;

void init_locale() {
    if (c_locale == NULL) c_locale = newlocale(LC_TIME_MASK, "C", NULL);
}
#else
void init_locale() {
    static int initialized = 0;
    if (initialized == 0) {
        setlocale(LC_TIME, "C");
	initialized == 1;
    }
}
#endif

time_t c_parse_unix_time(char *fmt, char *src) {
    struct tm dst;
    init_locale();
    memset(&dst, 0, sizeof(struct tm));
#if THREAD_SAFE
    strptime_l(src, fmt, &dst, c_locale);
#else
    strptime(src, fmt, &dst);
#endif
    return mktime(&dst);
}

#if !defined(HAVE_TIMEGM)
/* This part is for Solaris that doesn't have timegm.
 * The copyright notice of timegm and is_leap is as below:
 *
 * Copyright (c) 1997 Kungliga Tekniska H.gskolan
 * (Royal Institute of Technology, Stockholm, Sweden).
 * All rights reserved. 
 */

static int
is_leap(unsigned y)
{
  y += 1900;
  return (y % 4) == 0 && ((y % 100) != 0 || (y % 400) == 0);
}

static time_t
timegm (struct tm *tm)
{
  static const unsigned ndays[2][12] ={
    {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
    {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}};
  time_t res = 0;
  unsigned i;

  for (i = 70; i < tm->tm_year; ++i)
    res += is_leap(i) ? 366 : 365;

  for (i = 0; i < tm->tm_mon; ++i)
    res += ndays[is_leap(tm->tm_year)][i];
  res += tm->tm_mday - 1;
  res *= 24;
  res += tm->tm_hour;
  res *= 60;
  res += tm->tm_min;
  res *= 60;
  res += tm->tm_sec;
  return res;
}
#endif /* HAVE_TIMEGM */

time_t c_parse_unix_time_gmt(char *fmt, char *src) {
    struct tm dst;
    init_locale();
    memset(&dst, 0, sizeof(struct tm));
#if THREAD_SAFE
    strptime_l(src, fmt, &dst, c_locale);
#else
    strptime(src, fmt, &dst);
#endif
    return timegm(&dst);
}

size_t c_format_unix_time(char *fmt, time_t src, char* dst, int siz) {
    struct tm tim;
    init_locale();
    localtime_r(&src, &tim);
#if THREAD_SAFE
    return strftime_l(dst, siz, fmt, &tim, c_locale);
#else
    return strftime(dst, siz, fmt, &tim);
#endif
}

size_t c_format_unix_time_gmt(char *fmt, time_t src, char* dst, int siz) {
    struct tm tim;
    init_locale();
    gmtime_r(&src, &tim);
#if THREAD_SAFE
    return strftime_l(dst, siz, fmt, &tim, c_locale);
#else
    return strftime(dst, siz, fmt, &tim);
#endif
}
