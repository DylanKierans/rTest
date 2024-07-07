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

module M_PMPMEAS
  use ISO_C_BINDING, only : C_CHAR, C_FLOAT
  interface

    subroutine pmpmeas_init() bind(C,name='pmpmeas_init')
    end subroutine

    subroutine pmpmeas_start(tag) bind(C,name='pmpmeas_start')
      import C_CHAR
      character(kind=C_CHAR),dimension(*) :: tag
    end subroutine

    subroutine pmpmeas_stop(weight) bind(C,name='pmpmeas_stop_fortran')
      import C_FLOAT
      real(kind=C_FLOAT) :: weight
    end subroutine

    subroutine pmpmeas_finish() bind(C,name='pmpmeas_finish')
    end subroutine

  end interface
end module
