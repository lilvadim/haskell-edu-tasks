import Data.Map(lookup, fromList, Map)

type Variable = String
data Expr = Variable String | Const Double | Plus Expr Expr |
            Minus Expr Expr | Mult Expr Expr | Div Expr Expr |
            Pow Expr Expr | Neg Expr | PlusFew [Expr] | MultFew [Expr] | Ln Expr
            deriving (Show, Eq) 

simplify :: Expr -> Expr 
simplify v@(Variable _) = v
simplify x@(Const _) = x
simplify (Ln (Const x)) = Const (log x)
simplify id@(Ln _) = id
----
simplify (PlusFew xs) = simplify $ Plus (foldl (\acc x -> simplify (Plus acc x)) (Const 0) (filter helper xs)) (foldl (\acc x -> simplify (Plus acc x)) (Const 0) (filter (not . helper) xs)) where 
                                                                                                                                                                                                    helper (Const _) = True 
                                                                                                                                                                                                    helper _ = False
simplify (MultFew xs) = simplify $ Mult (foldl (\acc x -> simplify (Mult acc x)) (Const 1) (filter helper xs)) (foldl (\acc x -> simplify (Mult acc x)) (Const 1) (filter (not . helper) xs)) where 
                                                                                                                                                                                                    helper (Const _) = True 
                                                                                                                                                                                                    helper _ = False

----
simplify (Const a `Minus` Const b) = Const (a-b)
simplify (Minus a b) = Plus a (Mult (Const (-1)) b)
simplify (Plus (Const 0) e) = e --
simplify (Plus e (Const 0)) = e -- 
simplify (Plus (Const a) (Const b)) = Const (a+b) 
simplify id@(Plus (a `Mult` x) (b `Mult` y)) | x == y = Plus a b `Mult` x
                                             | otherwise = id
simplify (Neg (Const a)) = Const (-a)
simplify (Neg a) = Const (-1) `Mult` a
----
simplify (Mult (Const 0) _) = Const 0 --
simplify (Mult _ (Const 0)) = Const 0 --
simplify (Mult (Const 1.0) e) = e
simplify (Mult e (Const 1.0)) = e 
simplify (Mult (Const a) (Const b)) = Const (a*b)
simplify id@(Mult e1@(x `Pow` a) e2@(y `Pow` b)) | x == y = x `Pow` (a `Plus` b)
                                                 | otherwise = id
simplify ((Mult a b) `Div` c) | a == c = b 
                              | b == c = a
simplify (a `Div` Const 1) = a
simplify (a `Mult` (b `Div` c)) = Mult a b `Div` c
simplify (Div (Const a) (Const b)) = Const (a/b)
----
simplify (Pow e (Const 1)) = e --
simplify (Pow _ (Const 0)) = Const 1 --
simplify (Pow (Const 1) _) = Const 1 --
simplify (Pow (Const a) (Const b)) = Const (a ** b)
----
-- simplify (Mult (Plus a b)(Plus c d)) = (Mult a c) `Plus` (Mult a d) `Plus` (Mult b c) `Plus` (Mult b d)
-- simplify ((Pow a (Const 2)) `Minus` (Pow b (Const 2))) = (a `Plus` b) `Mult` (a `Minus` b)
----
simplify e = e

diff :: Expr -> Expr 
diff (Const _) = Const 0
diff (Variable _) = Const 1
diff f@(MultFew xs) = diff $ simplify f
diff (f `Mult` g) = simplify $ simplify ((simplify (diff (simplify f)) `Mult` simplify g) `Plus` simplify (simplify (diff (simplify g)) `Mult` simplify f))
diff f@(PlusFew xs) = diff $ simplify f
diff (f `Plus` g) = simplify (simplify (diff  (simplify f)) `Plus` simplify (diff (simplify g)))
diff (f `Pow` g) = simplify (simplify (f `Pow` g) `Mult` simplify (simplify (diff f)) `Mult` simplify (g `Div` f) `Plus` simplify (diff g `Mult` Ln f))

substitute :: Map String Double -> Expr -> Expr
substitute _ id@(Const _) = id
substitute m (Variable var) = maybe (Variable var) Const (Data.Map.lookup var m)
substitute m (Plus e1 e2) = Plus (substitute m e1) (substitute m e2)
substitute m (Minus e1 e2) = Minus (substitute m e1) (substitute m e2)
substitute m (Mult e1 e2) = Mult (substitute m e1) (substitute m e2)
substitute m (Div e1 e2) = Div (substitute m e1) (substitute m e2)
substitute m (Ln e) = Ln (substitute m e)
substitute m (Pow e1 e2) = Pow (substitute m e1) (substitute m e2)
substitute m (Neg e) = Neg (substitute m e)
----
substitute m e@(PlusFew _) = substitute m (simplify e)
substitute m e@(MultFew _) = substitute m (simplify e)
