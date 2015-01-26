module FlintMatArithmetic
where

class (FlintMat a) => FlintMatArithmetic a where
    (!+!) :: a -> a -> a
    (!*!) :: a -> a -> a
