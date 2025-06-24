# Min Priority Queue Implementation in Fortran

This module implements a minimum priority queue using a binary min-heap. Both integer and floating-point priorities are supported.

## Features

- **Priority Queues**: Separate implementations for integer (`pqi_t`) and real (`pqr_t`) priorities.
- **Dynamic Sizing**: Automatically resizes the backing array as needed.

## How to Use

### Module Usage
To use the priority queue, include the module in your Fortran program:

```fortran
use m_pq
```

### Creating a Priority Queue
You can create a priority queue with an optional initial size:

```fortran
type(pqi_t) :: int_queue
call pqi_create(int_queue, initial_size=128)
```

### Pushing Elements
Add elements to the queue with their associated priority:

```fortran
call pqi_push(int_queue, value, priority)
```

### Popping Elements
Remove and retrieve the element with the lowest priority:

```fortran
integer :: value, priority
call pqi_pop(int_queue, value, priority)
```

### Peeping Elements
Retrieve the lowest priority element without removing it:

```fortran
call pqi_peep(int_queue, value, priority)
```

### Cleaning Up

```fortran
call pqi_destroy(int_queue)
```

## Example

Refer to `test.f90`
