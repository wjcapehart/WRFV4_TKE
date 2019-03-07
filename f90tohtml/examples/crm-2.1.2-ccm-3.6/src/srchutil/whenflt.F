#include <misc.h>
#include <params.h>
c---------
      subroutine whenflt(n,array,inc,target,index,nval)
c
c $Id: whenflt.F,v 1.1 1998/04/01 07:26:06 ccm Exp $
c $Author: ccm $
c
	dimension index(*), array(*)
	ina=1
	nval=0
	if(inc .lt. 0) ina=(-inc)*(n-1)+1
	do 100 i=1,n
	    if(array(ina) .lt. target) then
	    nval=nval+1
	    index(nval)=i
	    end if
	    ina=ina+inc
 100    continue
      return
      end           
 
