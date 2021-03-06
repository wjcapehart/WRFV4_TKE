

////////////////////////////////////////////////////////////////////////


// *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
// ** Copyright UCAR (c) 1992 - 2018
// ** University Corporation for Atmospheric Research (UCAR)
// ** National Center for Atmospheric Research (NCAR)
// ** Research Applications Lab (RAL)
// ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA
// *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*


////////////////////////////////////////////////////////////////////////


using namespace std;

#include <iostream>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <cmath>

#include "vx_util.h"
#include "vx_log.h"

#include "shx_file.h"


////////////////////////////////////////////////////////////////////////


static ConcatString program_name;

static const int buf_size = 65536;

static unsigned char buf[buf_size];


////////////////////////////////////////////////////////////////////////


static void usage();


////////////////////////////////////////////////////////////////////////


int main(int argc, char * argv [])

{

program_name = get_short_name(argv[0]);

if ( argc != 2 )  usage();

int fd = -1;
int n_read, bytes;
int count;
ConcatString input_filename = argv[1];
ShxFileHeader h;
ShxRecord r;


if ( (fd = open(input_filename.contents(), O_RDONLY)) < 0 )  {

   mlog << Error
        << "\n\n  " << program_name << ": unable to open input file \""
        << input_filename << "\"\n\n";

   exit ( 1 );

}

cout << get_short_name(input_filename) << '\n';

   //
   //  main header
   //

bytes = 100;

if ( (n_read = read(fd, buf, bytes)) != bytes )  {

   mlog << Error
        << "\n\n  " << program_name << ": trouble reading main file header from input file \""
        << input_filename << "\"\n\n";

   exit ( 1 );

}

h.set(buf);

cout << "ShxFileHeader ...\n";

h.dump(cout, 1);

cout << "\n";

   //
   //  records
   //

count = 0;

while ( (n_read = read(fd, buf, 8)) == 8 )  {

   r.set(buf);

   cout << "Record " << (count++) << " ... \n";

   r.dump(cout, 1);

   cout << "\n";

}   //  while

cout << "\n\n   Read " << count << " records\n\n";

   //
   //  done
   //

close(fd);  fd = -1;

return ( 0 );

}


////////////////////////////////////////////////////////////////////////


void usage()

{

mlog << Error
     << "\n\n  usage:  " << program_name << " shp_filename\n\n";

exit ( 1 );

return;

}


////////////////////////////////////////////////////////////////////////


