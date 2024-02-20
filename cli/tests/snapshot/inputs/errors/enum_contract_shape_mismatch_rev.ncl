# capture = 'stderr'
# command = ['eval']
(std.function.id | forall r. [| 'Foo, 'Bar String, 'Qux Dyn; r |] -> [| 'Foo, 'Bar String, 'Qux Dyn; r |]) ('Foo 5)
