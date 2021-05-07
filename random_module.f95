module random_module
contains
   function random_integers(n) result(z)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      ! function return location
      integer, dimension(n) :: z
      ! local data
      integer :: i
      real :: r
      ! processing
      do i=1,n
         call random_number(r)
         z(i) = int(2147483448.*r)
      end do
   end function random_integers

   function random_reals(n) result(r)
      implicit none
      ! dummy argumentss
      integer, intent(in) :: n
      ! function return location
      double precision, dimension(n) :: r
      ! local data
      integer :: i
      integer, allocatable :: seed(:)
      ! processing
      call random_number(r)
   end function random_reals

   function random_strings(n, RECSIZ, NORECS) result(records)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n, RECSIZ, NORECS
      ! function return location
      character (len = RECSIZ), dimension(n) :: records
      ! local data
      integer :: i, recno
      real :: rnd
      character (len = 50), parameter :: INFILE = '/home/slacker/dat/data.dat'
      ! processing
      open (8,file=INFILE,access='direct',form='formatted',recl=RECSIZ)
      do i = 1, n
         call random_number(rnd)
         recno = int(NORECS * rnd)+1
         read (8,1000,rec=recno) records(i)
      end do
      close (8)
      1000 format (a)
   end function random_strings   

   subroutine init_random_seed()
      implicit none
      ! local data
      integer :: i, n, clock
      integer, dimension(:), allocatable :: seed
      ! processing
      call random_seed(size = n)
      allocate(seed(n))
      call system_clock(count = clock)
      seed = clock+37*(/(i-1,i=1,n)/)
      call random_seed(put = seed)
      deallocate(seed)
   end subroutine init_random_seed
end module random_module
