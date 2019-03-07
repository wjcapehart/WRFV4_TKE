#include <misc.h>
#include <params.h>
      integer function isrchfgt(n, array, inc, target)
c
c $Id: isrchfgt.F,v 1.1 1998/04/01 07:25:54 ccm Exp $
c $Author: ccm $
c
      integer n,inc
      real array(*),target
      integer i
      if (n.le.0) then
         isrchfgt = 0
         return
      end if
      ind = 1
      do i=1,n
         if (array(ind).gt.target) then
            isrchfgt = i
            return
         else
            ind = ind + inc
         end if
      end do
      isrchfgt = n + 1
      return
      end

 
