// C Runtime system for the APL compiler C backend
//
// Copyright (c) 2015, Martin Elsman
// MIT License

// ------------------
// Some abbreviations
// ------------------

#define max(x,y) (((x) > (y)) ? (x) : (y))
#define min(x,y) (((x) < (y)) ? (x) : (y))
#define i2d(x) (x)
#define d2i(x) (x)
#define b2i(x) ((x)?1:0)
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

static void prDouble(double arg)
{
  char buf[64];
  sprintf(buf, "%.12g", arg);
  if( countChar('.', buf) == 0 && countChar('E', buf) == 0 ) 
    {
      strcat(buf, ".0");
    }
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

// -----------------
// Halting execution
// -----------------

void halt(char *s) {
  printf("Execution halted: %s\n",s);
  exit(1);
}
