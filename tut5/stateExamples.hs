newtype State s v = State { runState :: s -> (v,s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, newState) = g s
                                   in (f a, newState)

instance Applicative (State s) where
  pure x = State $ \s -> (x,s)
  (State sf) <*> (State sv) =  State $ \st -> let (f, st1) = sf st -- first run the given stateful computation on st and extract a function and a new state
                                                  (a, st2) = sv st1 -- now run the second stateful computation on the extracted state to create another state and a value
                                              in (f a, st2) -- now, apply the function on the value, and keep the last state


instance Monad (State s) where
    return x = State $ \s -> (x,s)
    (State h) >>= f = State $ \s -> let (a, newState) = h s  -- run the first stateful computation, and call the result a
                                        (State g) = f a -- now, use this result to create a new stateful computation
                                    in  g newState

data RandGen = RandGen {curState :: Int,
                        multiplier :: Int,
                        increment :: Int,
                        modulus :: Int} deriving Show

runRandom :: State RandGen Int
runRandom = State $ f
  where
    f gen1 = (v, gen2)
        where
          v = curState gen1
          v2 = (v * (multiplier gen1) + (increment gen1)) `mod` (modulus gen1)
          gen2 = gen1 { curState = v2}

threeRandoms :: State RandGen (Int, Int, Int)
threeRandoms = do
    a <- runRandom
    b <- runRandom
    c <- runRandom
    return (a,b,c)

infiniteRandoms :: State RandGen [Int]
infiniteRandoms = do
  x <- runRandom
  xs <- infiniteRandoms
  return (x:xs)

type Stack = [Int]

pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop
