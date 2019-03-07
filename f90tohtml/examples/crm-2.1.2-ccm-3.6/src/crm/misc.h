c     $Header: /fs/cgd/csm/models/CVS.REPOS/atm/ccm_crm_src/crm/Attic/misc.h,v 1.1.2.1 1998/08/11 19:20:25 zender Exp $ -*-fortran-*-

#ifndef MISC_SET
#define MISC_SET
#define NCPREC NF_FLOAT
#define PVP 
#ifdef CRAY
#define REALTYPE MPI_REAL
#undef  SHELL_MSS
#undef  FORTFFT
#else /* not CRAY */
#define REALTYPE MPI_DOUBLE_PRECISION
#define SHELL_MSS
#define FORTFFT
#endif /* not CRAY */
#undef  COUP_SOM
#undef  COUP_CSM
#undef  SPMD
#if ( ! defined CRAY ) && ( ! defined SUN )  && ( ! defined SGI )
You must define one of CRAY, SUN or SGI 
#endif /* not CCM_ARCH */ 
#endif /* not MISC_SET */


