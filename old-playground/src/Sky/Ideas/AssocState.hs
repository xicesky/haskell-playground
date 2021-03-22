
module Sky.Ideas.AssocState where

----------------------------------------------------------------------------------------------------

{- The problem: Stacked state monads.
    Say you are using state monads and have multiple libraries (or simply seperate functionality)
    that uses them. E.g: Your program has state stprog, you are also using some random generator
    that uses the state stgen, a second random generator uses a state stgen2, some other library
    uses stlib...

    Your Monad is going to look very much like this:
        type MyMonad a = StateT stprog (StateT stgen1 (StateT stgen2 (StateT stlib))) a

    Of course you don't want your program to "lift" everything, but at least we have the
    "Control.Monad.State.MonadState" class, right? But there are a few problems here:
        1. Actually MonadState doesn't stack:
            "MyMonad" implements "MonadState MyMonad stprog" but not
            "MonadState MyMonad stgen1"
        2. If we could even make it stack, we run into another problem: What if two of those
            types are actually the same? Lets say stgen1 and stgen2 are the same type, then
            Haskell couldn't ever resolve what "get :: MyMonad stgen1" should do, because it
            can't know that we don't want to use the state "stgen1" and not "stgen2"

    Possible solutions:
        1. Associative State
            Use a class "AssocState s t a", where t is a marker type:
                get :: (MonadState s m, AssocState s RndGen1 a) => m a
                put :: (MonadState s m, AssocState s RndGen1 a) => a -> m ()
            "AssocState s RndGen1 a" means that the state s contains a value of type a
            for the library RndGen1.

        2. Lenses
            Instead of using "MonadState s m" directly, our state handling functions need a
            "pointer" which is just a lens:
                get :: (MonadState s m) => Lens s s a b -> m a
                put :: (MonadState s m) => Lens s s a b -> b -> m ()
            These are, btw, just the "use" and "assign" functions from the lens library.

    Comparison:
        AssocState resolves a "pointer" (the marker type) at compile time, while lenses do so at
        runtime. AssocState needs additional instance declarations everywhere and would probably
        require Haskell extensions, making it far more complicated.
        The marker type for AssocState can be fixed for a certain library, which would mean you
        could only use the library once - or you'd have to "pass" the marker type, which requires
        and additional parameter (that would just be a "Proxy X", where X is the marker type).
        But this would defeat the simplicity of calls to some degree, compared to the lens
        solution. (Resolving at compile time could, in theory, still be faster, though.)
        On the other hand: A compiler with partial evaluation could resolve a fixed lens at compile
        time.

-}
