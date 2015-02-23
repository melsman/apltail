// C Runtime system for the APL compiler C backend
//
// Copyright (c) 2015, Martin Elsman
// MIT License

// ------------------
// Some abbreviations
// ------------------

#include <unistd.h>
#include <math.h>
#include <sys/time.h>

#define ori(x,y) (int)(((unsigned int)(x))|((unsigned int)(y)))
#define andi(x,y) (int)(((unsigned int)(x))&((unsigned int)(y)))
#define xori(x,y) (int)(((unsigned int)(x))^((unsigned int)(y)))
#define shli(x,y) (int)(((unsigned int)(x))<<((unsigned int)(y)))
#define shri(x,y) (int)(((unsigned int)(x))>>((unsigned int)(y)))
#define shari(x,y) (int)(((int)(x))>>((unsigned int)(y)))
#define max(x,y) (((x) > (y)) ? (x) : (y))
#define min(x,y) (((x) < (y)) ? (x) : (y))
#define i2d(x) ((double)x)
#define d2i(x) ((int)x)
#define b2i(x) ((x)?1:0)
#define ln(x)  (log(x))
#define true 1
#define false 0
#define bool int

// ------------------------------
// Printing of scalars and arrays
// ------------------------------

static void prInt(int i) { printf("%d", i); }

static void prBool(int b) { prInt(b); }

// [countChar(c,s)] returns the number of occurences of c in s.
static ssize_t countChar(ssize_t c, char *s) {
  char *p; 
  ssize_t count;

  count = 0;
  for( p=s; *p != '\0'; p++ ) 
    {
      if( *p == c ) count++;
    }
  return count;
}

static void formatD(char* buf, double arg)
{
  sprintf(buf, "%.12g", arg);
  if( countChar('.', buf) == 0 && countChar('E', buf) == 0 ) 
    {
      strcat(buf, ".0");
    }
}

static void prDouble(double arg)
{
  char buf[64];
  formatD(buf, arg);
  printf("%s", buf);
}

// Print result of program evaluation

static void prScalarDouble(double arg)
{
  printf("[]("); prDouble(arg); printf(")");
}

// ---------------------------
// Some mathematical functions
// ---------------------------

static int resi(int y, int x)    // notice the swapped arguments
{
  int tmp;
  if ( y == 0 ) { return x; }
  tmp = x % y;
  if (tmp == 0 || (x>0 && y>0) || (x<0 && y<0)) {
    return tmp;
  } else {
    return tmp+y;
  }
}

static int floori(double x) {
  return (int)floor(x);
}

static int ceili(double x) {
  return (int)ceil(x);
}

// ------------------------------
// Roll function
// ------------------------------

static double roll (int x) {
  int i = rand();
  double r = ((double)i)/((double)RAND_MAX);
  if (x == 0) {
    return r;
  }
  int y = (int)(x * r);
  return (double)y;
}


// -----------------
// Now function
// -----------------

struct timeval tv_init;

static void initialize() {
  gettimeofday(&tv_init, NULL);
  return;
}

// return time since process start in milliseconds
static int now (int x) {
  struct timeval tv_check, tv_diff;
  gettimeofday(&tv_check, NULL);
  timersub(&tv_check, &tv_init, &tv_diff);
  long int usec = tv_diff.tv_usec;
  long int sec = tv_diff.tv_sec;
  long int msec = usec / 1000;
  return (int)(sec*1000+msec);
}

// -----------------
// Halting execution
// -----------------

void halt(char *s) {
  printf("Execution halted: %s\n",s);
  exit(1);
}
