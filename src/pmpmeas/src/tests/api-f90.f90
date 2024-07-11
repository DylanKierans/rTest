! -------
! PMPMEAS
! -------
! 
! Copyright 2022 Dirk Pleiter (pleiter@kth.se)
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions
! are met:
!
! 1. Redistributions of source code must retain the above copyright
!    notice, this list of conditions and the following disclaimer.
!
! 2. The origin of this software must not be misrepresented; you must 
!    not claim that you wrote the original software.  If you use this 
!    software in a product, an acknowledgment in the product 
!    documentation would be appreciated but is not required.
!
! 3. Altered source versions must be plainly marked as such, and must
!    not be misrepresented as being the original software.
!
! 4. The name of the author may not be used to endorse or promote 
!    products derived from this software without specific prior written 
!    permission.
!
! THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
! OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
! DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
! GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
! WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
! NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

program API_F90

use M_PMPMEAS

use ISO_C_BINDING, only : C_NULL_CHAR

real(8) x

write(*,*) "This is the Fortran main program"

call pmpmeas_init()

do i = 1, 10
  call pmpmeas_start("main::f90"//C_NULL_CHAR)

  x = 1e-16
  do j = 1, 200000000
    x = x * 1.00000001
  end do

  call pmpmeas_stop(1./100000)
end do

write(*,*) x

do i = 1, 10
  call pmpmeas_start("empty"//C_NULL_CHAR)
  call pmpmeas_stop(1.0)
end do

call pmpmeas_finish()

end program
