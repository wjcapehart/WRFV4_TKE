#include <misc.h>
#include <params.h>
      subroutine wheneq(n,array,inc,target,index,nval)
c
c $Id: wheneq.F,v 1.1 1998/04/01 07:26:01 ccm Exp $
c $Author: ccm $
c
      integer index(*),
     $        array(*),
     $        target
      ina=1
      nval=0
      if(inc .lt. 0) ina=(-inc)*(n-1)+1
      do i=1,n
         if(array(ina) .eq. target) then
           nval=nval+1
           index(nval)=i
         end if
         ina=ina+inc
      enddo
      return
      end
 
