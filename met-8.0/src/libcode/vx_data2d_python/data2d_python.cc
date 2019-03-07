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
#include <stdlib.h>
#include <cmath>

#include "data2d_python.h"
#include "vx_python_utils.h"
#include "data2d_utils.h"
#include "grdfiletype_to_string.h"

#include "vx_math.h"
#include "vx_log.h"


////////////////////////////////////////////////////////////////////////


   //
   //  Code for class MetPythonDataFile
   //


////////////////////////////////////////////////////////////////////////


MetPythonDataFile::MetPythonDataFile()

{

python_init_from_scratch();

}


////////////////////////////////////////////////////////////////////////


MetPythonDataFile::~MetPythonDataFile()

{

close();

}


////////////////////////////////////////////////////////////////////////


MetPythonDataFile::MetPythonDataFile(const MetPythonDataFile &)

{

mlog << Error << "\nMetPythonDataFile::MetPythonDataFile(const MetPythonDataFile &) -> "
     << "should never be called!\n\n";

exit ( 1 );

// python_init_from_scratch();
//
// assign(f);

}


////////////////////////////////////////////////////////////////////////


MetPythonDataFile & MetPythonDataFile::operator=(const MetPythonDataFile &)

{

mlog << Error << "\nMetPythonDataFile::operator=(const MetPythonDataFile &) -> "
     << "should never be called!\n\n";

exit ( 1 );

// if ( this == &f )  return ( * this );
//
// assign(f);

return ( * this );

}


////////////////////////////////////////////////////////////////////////


void MetPythonDataFile::python_init_from_scratch()

{

PythonCommand.clear();
Plane.clear();
VInfo.clear();

close();

return;

}


////////////////////////////////////////////////////////////////////////


void MetPythonDataFile::close()

{

Plane.clear();
VInfo.clear();

mtddf_clear();   //   base class

   //
   //  Don't reset the Type field
   //  Don't reset the PythonCommand
   //

return;

}


////////////////////////////////////////////////////////////////////////


void MetPythonDataFile::set_type(const GrdFileType t)

{

Type = t;

return;

}


////////////////////////////////////////////////////////////////////////


bool MetPythonDataFile::open(const char * cur_command)

{

close();

ConcatString full_path, path_name, file_name;
int i, file_argc;
char **file_argv = (char **) 0; // allocated
StringArray sa;


   //
   //  Store the PythonCommand that is being run
   //

PythonCommand = cur_command;

   //
   //  parse and store argc and argv
   //

sa = PythonCommand.split(" ");

file_argc = sa.n_elements();

if ( file_argc > 0 )  {
   file_argv = new char * [ file_argc ];
   for ( i=0; i<sa.n_elements(); i++ )  {
      file_argv[i] = new char [ strlen(sa[i]) + 1 ];
      strcpy(file_argv[i], sa[i]);
   }
}

   //
   //  Build the path and store the file name
   //

full_path = sa[0];

sa = full_path.split("/");

if ( sa.n_elements() <= 1 )  {
   path_name = ".";
}
else {
   for ( i=0; i<sa.n_elements()-1; i++ )  path_name << "/" << sa[i];
}

file_name = sa[sa.n_elements() - 1];

   //
   //  Set the PYTHONPATH
   //

setenv("PYTHONPATH", path_name, 1);

file_name.chomp(".py");   //  remove possible ".py" suffix from script filename

bool use_xarray = false;

switch ( Type )  {   //  assumes Type is already set

   case FileType_Python_Xarray:
      use_xarray = true;
      break;

   case FileType_Python_Numpy:
      use_xarray = false;
      break;

   default:
      mlog << Error
           << "MetPythonDataFile::open(const char * script_filename) -> bad file type: "
           << grdfiletype_to_string(Type) << "\n\n";
      exit ( 1 );
      break;

}   //  switch

Filename = file_name;

Raw_Grid = new Grid;

python_dataplane(file_name, file_argc, file_argv, use_xarray, Plane, *Raw_Grid, VInfo);

Dest_Grid = new Grid;

(*Dest_Grid) = (*Raw_Grid);

if ( ShiftRight != 0 )  Plane.shift_right(ShiftRight);

   //
   //  cleanup
   //

if ( file_argv )  {
   for ( i=1; i<file_argc; i++ )  {
      if ( file_argv[i] )  { delete [] file_argv[i]; file_argv[i] = (char *) 0; }
   }
   delete [] file_argv; file_argv = (char **) 0;
}

   //
   //  done
   //

return ( true );

}


////////////////////////////////////////////////////////////////////////


void MetPythonDataFile::dump(ostream & out, int depth) const

{

Indent prefix(depth);

out << prefix << "File = ";

if ( Filename.empty() )  out << "(nul)\n";
else                     out << '\"' << Filename << "\"\n";

if ( Raw_Grid )  {

   out << prefix << "Grid:\n";

   Raw_Grid->dump(out, depth + 1);

} else {

   out << prefix << "No Grid!\n";

}

   //
   //  done
   //

out.flush();

return;

}


////////////////////////////////////////////////////////////////////////


double MetPythonDataFile::get(int x, int y) const

{

double value = Plane.get(x, y);

return ( value );

}


////////////////////////////////////////////////////////////////////////


bool MetPythonDataFile::data_ok(int x, int y) const

{

const double value = get(x, y);

return ( !is_bad_data(value) );

}


////////////////////////////////////////////////////////////////////////


void MetPythonDataFile::data_minmax(double & data_min, double & data_max) const

{

Plane.data_range(data_min, data_max);

return;

}


////////////////////////////////////////////////////////////////////////


bool MetPythonDataFile::data_plane(VarInfo &vinfo, DataPlane &plane)

{

   //
   //  the python command is specified by VarInfo::Name
   //  only open if the python command is empty or has changed
   //

if ( PythonCommand.empty() || PythonCommand != vinfo.req_name() ) {

   close();

   open(vinfo.req_name());

}

   //
   //  ok
   //

plane = Plane;

   //
   //  store the VarInfo metadata without completely overwritting it
   //

vinfo.set_name(VInfo.name());
vinfo.set_long_name(VInfo.long_name());
vinfo.set_level_name(VInfo.level_name());
vinfo.set_units(VInfo.units());
vinfo.set_magic(VInfo.name(), VInfo.level_name());

return ( true );

}


////////////////////////////////////////////////////////////////////////


int MetPythonDataFile::data_plane_array(VarInfo &vinfo, DataPlaneArray &plane_array)

{

   //
   //  the python command is specified by VarInfo::Name
   //  only open if the python command is empty or has changed
   //

if ( PythonCommand.empty() || PythonCommand != vinfo.req_name() ) {

   close();

   open(vinfo.req_name());

}

   //
   //  ok
   //

plane_array.clear();

plane_array.add(Plane, 0.0, 0.0);

   //
   //  store the VarInfo metadata without completely overwritting it
   //

vinfo.set_name(VInfo.name());
vinfo.set_long_name(VInfo.long_name());
vinfo.set_level_name(VInfo.level_name());
vinfo.set_units(VInfo.units());
vinfo.set_magic(VInfo.name(), VInfo.level_name());

return ( true );

}


////////////////////////////////////////////////////////////////////////


bool MetPythonDataFile::data_plane(DataPlane &plane)

{

   //
   //  is the file even open?
   //

if ( ! Raw_Grid )  return ( false );

   //
   //  ok
   //

plane = Plane;

return ( true );

}


////////////////////////////////////////////////////////////////////////


int MetPythonDataFile::index(VarInfo &vinfo)

{

   //
   //  is the file even open?
   //

if ( ! Raw_Grid )  return ( -1 );

   //
   //  ok
   //

return ( 0 );

}


////////////////////////////////////////////////////////////////////////
