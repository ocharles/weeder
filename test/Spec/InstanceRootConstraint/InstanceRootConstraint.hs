module Spec.InstanceRootConstraint.InstanceRootConstraint where


class Foo a where
  foo :: a -> Char


instance Foo Char where
  foo = id


instance (Foo a) => Foo [a] where
  foo = const a


a :: Char
a = foo 'a'
