class SomeClass:
    a = 2
    b = 3

    @property
    def c(self):
        print 'C has been called'
        return 15


def test_class_property():
    sc = SomeClass()

    print '\n'
    print sc.a
    print sc.b
    print '\n Here I expect the other print'
    print sc.c


def test_class_property_without_init():
    print '\n'
    print SomeClass.a
    print SomeClass.b
    print '\n Here I expect the other print'
    print SomeClass.c