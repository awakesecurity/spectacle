Version 0.1.0.0
===============

#### Extensible Records

* 'RecT' renamed to 'RecF' to avoid colliding with the naming scheme generally reserved for monad transformers. Record constructor names reflect this change.

* The "rec" suffix removed from the functions 'setRecF', 'getRecF', 'getRec', and 'setRec' renamed to prefer the less noise 'setF', getF', 'set', and 'get'.

* The selector constraint (.|) and (#) type synonym replaced with 'Has' constraint for readiblity. 
  ```haskell 
  -- previously 
  type HasFooInt ctx = "foo" # Int .| ctx 
  
  -- ... is now 
  type HasFootInt ctx = Has "foo" Int ctx
  ```

* A new type 'Evident' and class 'HasDict' now give a uniform way for capturing typeclass evidence of extensible records.

* 'Rec' and 'RecT' instance declaration are no longer defined for the base case and inductive case. Instead, extensible record instances pass the responsibility witnessing a dictionary to a 'HasDict' superclass.  
  ```haskell 
  -- old version
  instance Show (Rec '[]) where 
    show RNil = ...

  instance (Show x, Show xs) => Show (Rec (x ': xs)) where 
    show RNil = ...
    
  -- new version
  instance HasDict Show ctx => Show (Rec ctx) where
    show Nil = ... 
    show Con {} = ... 
  ```
