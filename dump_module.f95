module dump_module
   ! overloading interface
   ! dump data arrays of various types
   interface dump
      module procedure dump_integers
      module procedure dump_reals
      module procedure dump_strings
   end interface dump
contains
   subroutine dump_integers(n, x)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      integer, intent(in), dimension(n) :: x
      ! local data
      integer :: i
      ! processing
      do i = 1, n
         write (*,*) x(i)
      end do
   end subroutine dump_integers

   subroutine dump_reals(n, x)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, intent(in), dimension(n) :: x
      ! local data
      integer :: i
      ! processing
      do i = 1, n
         write (*,*) x(i)
      end do
   end subroutine dump_reals

   subroutine dump_strings(n, str)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      character (len = *), intent(in), dimension(n) :: str
      ! local data
      integer :: i
      ! processing
      do i = 1, n
         write (*,*) TRIM(str(i))
      end do
   end subroutine dump_strings
end module dump_module
