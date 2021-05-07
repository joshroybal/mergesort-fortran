program mergesort
   use mergesort_module
   use random_module
   use dump_module
   implicit none
   ! variables and arrays
   integer, parameter :: RECSIZ = 128, NOSTR = 405995
   integer :: i, idx, n, clock
   real :: t1, t2, rnd
   character (len = 12) :: nstr
   character (len = 50) :: path
   character (len = RECSIZ) :: word
   integer, dimension(:), allocatable :: x, y
   double precision, dimension(:), allocatable :: r, s
   character (len = RECSIZ), dimension(:), allocatable :: wshuf, wsort
   ! processing
   if (command_argument_count() < 1) then
      call get_command(path)
      write (0,*) 'usage: ', trim(path), ' n'
      stop 'processing terminated'
   end if
   call get_command_argument(1, nstr)
   read (nstr,*) n
   ! integer section
   write (*,*) 'integers'
   allocate (x(n))
   allocate (y(n))
   call init_random_seed()
   x = random_integers(n)
   if (n <= 50) call dump(n, x)
   write (*,*) 'ascending'
   write (*,*) sorted(n, x, 'ASC')
   call cpu_time(t1)
   call sort(n, x, y, 'ASC')
   call cpu_time(t2)
   if (n <= 50) call dump(n, y)
   write (*,*) sorted(n, y, 'ASC')
   write (*,*) 'elapsed time = ', t2-t1, ' secs.'
   x = random_integers(n)
   if (n <= 50) call dump(n, x)
   write (*,*) 'descending'
   call cpu_time(t1)
   call sort(n, x, y, 'DESC')
   call cpu_time(t2)
   if (n <= 50) call dump(n, y)
   write (*,*) sorted(n, y, 'DESC')
   write (*,*) 'elapsed time = ', t2-t1, ' secs.'   
   deallocate (x)
   deallocate (y)
   ! double precision section
   write (*,*) 'double precision'
   allocate (r(n))
   allocate (s(n))
   r = random_reals(n)
   if (n <= 50) call dump(n, r)
   write (*,*) sorted(n, r, 'ASC')
   write (*,*) 'ascending'
   call cpu_time(t1)
   call sort(n, r, s, 'ASC')
   call cpu_time(t2)
   if (n <= 50) call dump(n, s)
   write (*,*) sorted(n, s, 'ASC')
   write (*,*) 'elapsed time = ', t2-t1, ' secs.'
   r = random_reals(n)
   if (n <= 50) call dump(n, r)
   write (*,*) 'descending'
   call cpu_time(t1)
   call sort(n, r, s, 'DESC')
   call cpu_time(t2)
   if (n <= 50) call dump(n, s)
   write (*,*) sorted(n, s, 'DESC')
   write (*,*) 'elapsed time = ', t2-t1, ' secs.'   
   deallocate (r)
   deallocate (s)
   ! character string section
   write (*,*) 'character strings'
   n = MIN(n, NOSTR)
   allocate (wshuf(n))
   allocate (wsort(n))
   wshuf = random_strings(n, RECSIZ, NOSTR)
   if (n <= 50) call dump(n, wshuf)
   write (*,*) sorted(n, wshuf, 'ASC')
   write (*,*) 'ascending'
   call cpu_time(t1)
   call sort(n, wshuf, wsort, 'ASC')
   call cpu_time(t2)
   if (n <= 50) call dump(n, wsort)
   write (*,*) sorted(n, wsort, 'ASC')   
   write (*,*) 'elapsed time = ', t2-t1, ' secs.'
   wshuf = random_strings(n, RECSIZ, NOSTR)
   if (n <= 50) call dump(n, wshuf)
   write (*,*) 'descending'
   call cpu_time(t1)
   call sort(n, wshuf, wsort, 'DESC')
   call cpu_time(t2)
   if (n <= 50) call dump(n, wsort)
   write (*,*) sorted(n, wsort, 'DESC')
   write (*,*) 'elapsed time = ', t2-t1, ' secs.'
   deallocate (wshuf)
   deallocate (wsort)
end program mergesort
