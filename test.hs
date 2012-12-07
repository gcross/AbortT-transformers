{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error (ErrorT(..),catchError,throwError)
import Control.Monad.Trans.State (StateT(..),execState,modify)
import Control.Monad.Trans.Writer (WriterT(..),tell,listen,pass,runWriter)

import Data.Functor.Identity

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Control.Monad.Trans.Abort

main = defaultMain
    [testGroup "Functor"
        [testGroup "Identity"
            [testProperty "without Abort" $
                \(x :: Int) (y :: Int) → (== x+y) . runAbort . fmap (+y) . return $ x
            ,testProperty "with goto" $
                \(x :: Int) (y :: Int) → (== x) . runAbort . fmap (+y) . abort $ x
            ]
        ,testGroup "Maybe"
            [testProperty "without Abort" $
                \(x :: Int) (y :: Int) → (== Just (x+y)) . runAbortT . fmap (+y) . lift . Just $ x
            ,testProperty "with Abort" $
                \(x :: Int) (y :: Int) → (== Just x) . runAbortT . fmap (+y) . (>>= abort) . lift . Just $ x
            ]
        ]
    ,testGroup "Applicative"
        [testGroup "Identity"
            [testProperty "without Abort" $
                \(x :: Int) (y :: Int) → runAbort (return (+y) <*> return x) == x+y
            ,testProperty "with Abort" $
                \(x :: Int) (y :: Int) → runAbort (return (+y) <*> abort x) == x
            ]
        ]
    ,testGroup "Monad"
        [testGroup "Maybe"
            [testGroup "Just"
                [testProperty "without Abort" $
                    \(x :: Int) (y :: Int) → (== Just (x+y)) . runAbortT $ do
                        a ← lift (Just x)
                        b ← lift (Just y)
                        return (a+b)
                ,testProperty "with Abort" $
                    \(x :: Int) (y :: Int) → (== Just x) . runAbortT $ do
                        a ← lift (Just x)
                        abort a
                        b ← lift (Just y)
                        return (a+b)
                ]
            ,testGroup "Nothing"
                [testProperty "without Abort" $
                    \(x :: Int) (y :: Int) → (== Nothing) . runAbortT $ do
                        a ← lift (Just x)
                        b ← lift (Just y)
                        lift Nothing
                        return (a+b)
                ,testProperty "with Abort" $
                    \(x :: Int) (y :: Int) → (== Just x) . runAbortT $ do
                        a ← lift (Just x)
                        abort a
                        b ← lift (Just y)
                        lift Nothing
                        return (a+b)
                ]
            ]
        ,testGroup "State"
            [testProperty "without Abort" $
                \(x :: Int) (y :: Int) → (== x+y) . flip execState x . runAbortT $ do
                    lift (modify (+y))
            ,testProperty "with Abort" $
                \(x :: Int) (y :: Int) → (== x) . flip execState x . runAbortT $ do
                    abort ()
                    lift (modify (+y))
            ]
        ]
    ,testGroup "lifters"
        [testGroup "liftCallCC"
            [testCase "callCC bypasses abort" $
                True @=? (flip runCont id . runAbortT . liftCallCC callCC $ \c → (c True >> abort False))
            ,testCase "abort bypasses callCC" $
                True @=? (flip runCont id . runAbortT . liftCallCC callCC $ \c → (abort True >> c False))
            ]
        ,testGroup "liftCatch"
            [testCase "throwError bypasses abort" $
                Right True @=? (runIdentity . runErrorT . runAbortT $
                    liftCatch catchError
                        (lift (throwError "") >> abort False)
                        (\_ → return True)
                )
            ,testCase "abort bypasses throwError" $
                Right True @=? (runIdentity . runErrorT . runAbortT $
                    liftCatch catchError
                        (abort True >> lift (throwError ""))
                        (\_ → return False)
                )
            ]
        ,testGroup "liftListen"
            [testCase "abort before tell" $
                ((True,"right"),"") @=? (runWriter . runAbortT $ do
                    liftListen listen (abort (True,"right") >> lift (tell "wrong") >> return False)
                )
            ,testCase "abort after tell" $
                ((True,"A"),"B") @=? (runWriter . runAbortT $ do
                    liftListen listen (lift (tell "B") >> abort (True,"A") >> return False)
                )
            ]
        ,testGroup "liftPass"
            [testCase "abort bypasses function" $
                (True,"") @=? (runWriter . runAbortT $ do
                    liftPass pass (abort True >> return (False,const "wrong"))
                )
            ]
        ]
    ]
