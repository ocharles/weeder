-- | Test for correct output of unreachable classes and instances
module Spec.InstanceTypeclass.InstanceTypeclass where


class Foo a where
  foo :: a -> Char


-- this instance is not marked as root,
-- therefore class Foo will show up in the output
-- as well
instance Foo Char where
  foo = id


class RootClass a where
  rootClass :: a -> Char


-- this instance is explicitly marked as root,
-- hence RootClass will not show up in the output
-- (note the way it is written in InstanceTypeclass.toml)
instance RootClass Char where
  rootClass = id
