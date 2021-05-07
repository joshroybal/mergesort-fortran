module mergesort_module
   ! overloading interfaces
   ! main bottom up merge sort subroutines
   interface sort
      module procedure sort_integers
      module procedure sort_reals
      module procedure sort_strings
   end interface sort
   ! merge sort auxliary merge subroutine
   interface merge_arrays
      module procedure merge_integers
      module procedure merge_reals
      module procedure merge_strings
   end interface merge_arrays
   ! unit test functions
   interface sorted
      module procedure sorted_integers
      module procedure sorted_reals
      module procedure sorted_strings
   end interface sorted
contains
   subroutine sort_integers(n, x, y, order)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      integer, intent(inout), dimension(n) :: x, y
      character (len = *), intent(in) :: order
      ! local data
      integer i, j, rnsz, m, l, r

      rnsz = 1
      do
         if (2*rnsz > n) exit
         l = 1
         r = l+rnsz
         do
            if (r+rnsz > n+1) exit
            call merge_arrays(rnsz, rnsz, x(l:), x(r:), y(l:), order)
            l = l+2*rnsz
            r = l+rnsz
         end do
         m = mod(n, 2*rnsz)
         if (m > rnsz) then
            l = (n-m)+1
            r = l+rnsz
            call merge_arrays(rnsz, m-rnsz, x(l:), x(r:), y(l:), order)
         else if (m > 0) then
            l = (n-m)+1
            y(l:) = x(l:)
         end if
         x = y
         rnsz = 2*rnsz
      end do
      call merge_arrays(rnsz, n-rnsz, x(1:rnsz), x(rnsz+1:n), y, order)
   end subroutine sort_integers

   subroutine sort_reals(n, x, y, order)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, intent(inout), dimension(n) :: x, y
      character (len = *), intent(in) :: order
      ! local data
      integer i, j, rnsz, m, l, r

      rnsz = 1
      do
         if (2*rnsz > n) exit
         l = 1
         r = l+rnsz
         do
            if (r+rnsz > n+1) exit
            call merge_arrays(rnsz, rnsz, x(l:), x(r:), y(l:), order)
            l = l+2*rnsz
            r = l+rnsz
         end do
         m = mod(n, 2*rnsz)
         if (m > rnsz) then
            l = (n-m)+1
            r = l+rnsz
            call merge_arrays(rnsz, m-rnsz, x(l:), x(r:), y(l:), order)
         else if (m > 0) then
            l = (n-m)+1
            y(l:) = x(l:)
         end if
         x = y
         rnsz = 2*rnsz
      end do
      call merge_arrays(rnsz, n-rnsz, x(1:rnsz), x(rnsz+1:n), y, order)
   end subroutine sort_reals

   subroutine sort_strings(n, x, y, order)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      character (len = *), intent(inout), dimension(n) :: x, y
      character (len = *), intent(in) :: order
      ! local data
      integer i, j, rnsz, m, l, r

      rnsz = 1
      do
         if (2*rnsz > n) exit
         l = 1
         r = l+rnsz
         do
            if (r+rnsz > n+1) exit
            call merge_arrays(rnsz, rnsz, x(l:), x(r:), y(l:), order)
            l = l+2*rnsz
            r = l+rnsz
         end do
         m = mod(n, 2*rnsz)
         if (m > rnsz) then
            l = (n-m)+1
            r = l+rnsz
            call merge_arrays(rnsz, m-rnsz, x(l:), x(r:), y(l:), order)
         else if (m > 0) then
            l = (n-m)+1
            y(l:) = x(l:)
         end if
         x = y
         rnsz = 2*rnsz
      end do
      call merge_arrays(rnsz, n-rnsz, x(1:rnsz), x(rnsz+1:n), y, order)
   end subroutine sort_strings   

   subroutine merge_integers(m, n, x, y, z, order)
      implicit none
      ! dummy arguments
      integer, intent(in) :: m, n
      integer, dimension(m), intent(in) :: x
      integer, dimension(n), intent(in) :: y
      integer, dimension(m+n), intent(out) :: z
      character (len = *), intent(in) :: order
      ! local data
      integer :: i, j, k
      ! processing
      i = 1
      j = 1
      k = 1
      do
         if (k > m+n) exit
         if (i > m) then
            z(k:m+n) = y(j:n)
            return
         else if (j > n) then
            z(k:m+n) = x(i:m)
            return
         else if (order == 'ASC' .and. x(i) <= y(j)) then
            z(k) = x(i)
            i = i+1
         else if (order == 'DESC' .and. x(i) >= y(j)) then
            z(k) = x(i)
            i = i + 1
         else
            z(k) = y(j)
            j = j+1
         end if
         k = k+1
      end do
   end subroutine merge_integers

   subroutine merge_reals(m, n, x, y, z, order)
      implicit none
      ! dummy arguments
      integer, intent(in) :: m, n
      double precision, dimension(m), intent(in) :: x
      double precision, dimension(n), intent(in) :: y
      double precision, dimension(m+n), intent(out) :: z
      character (len = *), intent(in) :: order
      ! local data
      integer :: i, j, k
      ! processing
      i = 1
      j = 1
      k = 1
      do
         if (k > m+n) exit
         if (i > m) then
            z(k:m+n) = y(j:n)
            return
         else if (j > n) then
            z(k:m+n) = x(i:m)
            return
         else if (order == 'ASC' .and. x(i) <= y(j)) then
            z(k) = x(i)
            i = i+1
         else if (order == 'DESC' .and. x(i) >= y(j)) then
            z(k) = x(i)
            i = i+1
         else
            z(k) = y(j)
            j = j+1
         end if
         k = k+1
      end do
   end subroutine merge_reals

   subroutine merge_strings(m, n, x, y, z, order)
      implicit none
      ! dummy arguments
      integer, intent(in) :: m, n
      character (len = *), dimension(m), intent(in) :: x
      character (len = *), dimension(n), intent(in) :: y
      character (len = *), dimension(m+n), intent(out) :: z
      character (len = *), intent(in) :: order
      ! local data
      integer :: i, j, k
      ! processing
      i = 1
      j = 1
      k = 1
      do
         if (k > m+n) exit
         if (i > m) then
            z(k:m+n) = y(j:n)
            return
         else if (j > n) then
            z(k:m+n) = x(i:m)
            return
         else if (order == 'ASC' .and. x(i) <= y(j)) then
            z(k) = x(i)
            i = i+1
         else if (order == 'DESC' .and. x(i) >= y(j)) then
            z(k) = x(i)
            i = i+1
         else
            z(k) = y(j)
            j = j+1
         end if
         k = k+1
      end do
   end subroutine merge_strings   

   function sorted_integers(n, a, order) result(b)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      integer, dimension(n), intent(in) :: a
      character (len = *), intent(in) :: order
      ! function return location
      logical :: b
      ! local data
      integer :: i
      ! processing
      i = 1
      do
         if (i == n-1) exit
         if (order == 'ASC' .and. a(i+1) < a(i)) exit
         if (order == 'DESC' .and. a(i+1) > a(i)) exit
         i = i + 1
      end do
      if (i == n-1) then
         b = .true.
      else
         b = .false.
      end if
   end function sorted_integers

   function sorted_reals(n, a, order) result(b)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      double precision, dimension(n), intent(in) :: a
      character (len = *), intent(in) :: order
      ! function return location
      logical :: b
      ! local data
      integer :: i
      ! processing
      i = 1
      do
         if (i == n-1) then
            exit
         else if (order == 'ASC' .and. a(i+1) < a(i)) then
            exit
         else if (order == 'DESC' .and. a(i+1) > a(i)) then
            exit
         else
            i = i + 1
         end if
      end do
      if (i == n-1) then
         b = .true.
      else
         b = .false.
      end if
   end function sorted_reals

   function sorted_strings(n, a, order) result(b)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      character (len = *), dimension(n), intent(in) :: a
      character (len = *), intent(in) :: order
      ! function return location
      logical :: b
      ! local data
      integer :: i
      ! processing
      i = 1
      do
         if (i == n-1) then
            exit
         else if (order == 'ASC' .and. a(i+1) < a(i)) then
            exit
         else if (order == 'DESC' .and. a(i+1) > a(i)) then
            exit
         else
            i = i + 1
         end if      
      end do
      if (i == n-1) then
         b = .true.
      else
         b = .false.
      end if
   end function sorted_strings 
end module
