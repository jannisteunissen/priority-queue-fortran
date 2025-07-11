program main
  use m_pq
  implicit none

  integer, parameter :: dp = kind(0.0d0)

  call test_strings()
  call test_indexing()
  call benchmark_int_priority(1000*1000)
  call benchmark_real_priority(1000*1000)

contains

  subroutine test_strings()
    type(pqr_t) :: pq
    character(len=*), parameter :: tasks(*) = [character(20) :: &
         "Clear drains", &
         "Feed cat", &
         "Make tea", &
         "Solve RC tasks", &
         "Tax return", &
         "Feed another cat" &
         ]

    integer  :: i, val
    real(dp) :: priority

    call pqr_create(pq)

    call pqr_push(pq, 1, 3.0_dp)
    call pqr_push(pq, 2, 4.0_dp)
    call pqr_push(pq, 3, 5.0_dp)
    call pqr_push(pq, 4, 1.0_dp)
    call pqr_push(pq, 5, 2.0_dp)
    call pqr_push(pq, 6, 4.0_dp)

    do i = 1, size(tasks)
       call pqr_pop(pq, val, priority)
       print *, priority, tasks(val)
    end do

    call pqr_destroy(pq)
  end subroutine test_strings

  subroutine test_indexing()
    type(pqr_t)                    :: pq
    character(len=20), allocatable :: strings(:)
    integer                        :: i
    real(dp)                       :: priority

    allocate(strings(100))
    call pqr_create(pq)

    call pqr_push_aix(pq, i, 4.0_dp)
    strings(i) = 'a'

    call pqr_push_aix(pq, i, 5.0_dp)
    strings(i) = 'great'

    call pqr_push_aix(pq, i, 6.0_dp)
    strings(i) = 'test'

    call pqr_push_aix(pq, i, 1.0_dp)
    strings(i) = 'hello'

    call pqr_push_aix(pq, i, 2.0_dp)
    strings(i) = 'this'

    call pqr_push_aix(pq, i, 3.0_dp)
    strings(i) = 'is'

    do while (pq%n_stored > 3)
       call pqr_pop_aix(pq, i, priority)
       print *, priority, i, strings(i)
    end do

    print *, "Highest index: ", pq%highest_ix
    print *, "Number of free indices: ", pq%n_free_ix

    call pqr_push_aix(pq, i, 1.0_dp)
    strings(i) = 'indeed'

    call pqr_push_aix(pq, i, 2.0_dp)
    strings(i) = 'very'

    call pqr_push_aix(pq, i, 3.0_dp)
    strings(i) = 'much'

    do while (pq%n_stored > 0)
       call pqr_pop_aix(pq, i, priority)
       print *, priority, i, strings(i)
    end do

    print *, "Highest index: ", pq%highest_ix
    print *, "Number of free indices: ", pq%n_free_ix

    call pqr_destroy(pq)
  end subroutine test_indexing

  subroutine benchmark_real_priority(n_elements)
    use iso_fortran_env, only: int64
    integer, intent(in) :: n_elements

    type(pqr_t)            :: pq
    integer               :: i, val
    real(dp)              :: priority
    integer(int64)        :: t_start, t_end, count_rate
    real(dp)              :: cpu_time
    real(dp), allocatable :: priorities(:)

    allocate(priorities(n_elements))
    call random_number(priorities)

    call pqr_create(pq)

    call system_clock(t_start, count_rate)
    do i = 1, n_elements
       call pqr_push(pq, i, priorities(i))
    end do

    do i = 1, n_elements
       call pqr_pop(pq, val, priority)
    end do
    call system_clock(t_end, count_rate)

    call pqr_destroy(pq)

    cpu_time = (t_end-t_start) / real(count_rate, dp)
    write(*, "(A,I0,A,E10.3,A)") " For ", n_elements, &
         " elements: ", cpu_time/n_elements, " second per push and pop"
  end subroutine benchmark_real_priority

  subroutine benchmark_int_priority(n_elements)
    use iso_fortran_env, only: int64
    integer, intent(in) :: n_elements

    type(pqi_t)           :: pq
    integer               :: i, val, priority
    integer(int64)        :: t_start, t_end, count_rate
    real(dp)              :: cpu_time
    real(dp), allocatable :: priorities_real(:)
    integer, allocatable  :: priorities(:)

    allocate(priorities(n_elements))
    allocate(priorities_real(n_elements))

    call random_number(priorities_real)
    priorities = floor(priorities_real * huge(1))

    call pqi_create(pq)

    call system_clock(t_start, count_rate)
    do i = 1, n_elements
       call pqi_push(pq, i, priorities(i))
    end do

    do i = 1, n_elements
       call pqi_pop(pq, val, priority)
    end do
    call system_clock(t_end, count_rate)

    call pqi_destroy(pq)

    cpu_time = (t_end-t_start) / real(count_rate, dp)
    write(*, "(A,I0,A,E10.3,A)") " For ", n_elements, &
         " elements: ", cpu_time/n_elements, " second per push and pop"
  end subroutine benchmark_int_priority

end program main
