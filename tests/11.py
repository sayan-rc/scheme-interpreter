test = {
  'name': 'Question 11',
  'points': 2,
  'suites': [
    {
      'cases': [
        {
          'code': r"""
          >>> frame = global_frame.make_child_frame(Pair('a', Pair('b', Pair('c', nil))), Pair(1, Pair(2, Pair(3, nil))))
          >>> global_frame.lookup('a')
          SchemeError
          >>> frame.lookup('a')
          1
          >>> frame.lookup('b')
          2
          >>> frame.lookup('c')
          3
          """,
          'hidden': False,
          'locked': False
        },
        {
          'code': r"""
          >>> frame = global_frame.make_child_frame(nil, nil)
          >>> frame.parent is global_frame
          True
          """,
          'hidden': False,
          'locked': False
        },
        {
          'code': r"""
          >>> first = Frame(global_frame)
          >>> second = first.make_child_frame(nil, nil)
          >>> second.parent is first
          True
          """,
          'hidden': False,
          'locked': False
        }
      ],
      'scored': True,
      'setup': r"""
      >>> from scheme import *
      >>> global_frame = create_global_frame()
      """,
      'teardown': '',
      'type': 'doctest'
    },
    {
      'cases': [
        {
          'code': r"""
          >>> # More argument values than formal parameters
          >>> global_frame.make_child_frame(Pair('a', nil), Pair(1, Pair(2, Pair(3, nil))))
          SchemeError
          """,
          'hidden': False,
          'locked': False
        },
        {
          'code': r"""
          >>> # More formal parameters than argument values
          >>> global_frame.make_child_frame(Pair('a', Pair('b', Pair('c', nil))), Pair(1, nil))
          SchemeError
          """,
          'hidden': False,
          'locked': False
        },
        {
          'code': r"""
          >>> # Values can be pairs.
          >>> frame = global_frame.make_child_frame(Pair('a', Pair('b', nil)), Pair(Pair(1, nil), Pair(Pair(2, nil), nil)))
          >>> frame.lookup('a')
          Pair(1, nil)
          >>> frame.lookup('b')
          Pair(2, nil)
          >>> frame2 = frame.make_child_frame(nil, nil) # Bind parents correctly
          >>> frame2.lookup('a')
          Pair(1, nil)
          """,
          'hidden': False,
          'locked': False
        }
      ],
      'scored': True,
      'setup': r"""
      >>> from scheme import *
      >>> global_frame = create_global_frame()
      """,
      'teardown': '',
      'type': 'doctest'
    }
  ]
}
